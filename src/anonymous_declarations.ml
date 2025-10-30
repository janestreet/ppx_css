open! Core
open! Ppxlib
open Css_parser

type t =
  { original_declaration_string : string loc
  ; inferred_do_not_hash : string list
  ; anonymous_variables : Anonymous_variable.Collection.t
  ; postprocessed_stylesheet : Stylesheet.t
  ; anonymous_class_name : string
  }
[@@deriving fields ~getters]

(* This function allows us to wrap a [Style_block.t] in a selector to create a stylesheet. 
   The actual location of the selector doesn't exist, as it's added virtually. *)
let dummy_selector ~loc class_ : Selector_list.t =
  let classname = Selector.Class class_ in
  let compound_selector = [ (classname, loc), [] ] in
  let complex_selector =
    [ Complex_selector.Selector (compound_selector, loc), [] ], loc
  in
  [ complex_selector, [] ], loc
;;

(* Create a [Stylesheet.t] from a style block contents string. This allows 
   us to parse the style block contents once and then pass it around within 
   this module instead of parsing it 2 times within this module and then
   once again in [traverse_css].
*)
let style_block_contents_string_to_stylesheet
  ~string_loc
  ~anonymous_class_name
  original_declaration_string
  =
  let style_block =
    Css_parser.parse_style_block_contents
      ~parsing_config:Css_parser.Parsing_config.raise_on_recoverable_errors
      ~pos:string_loc.loc_start
      original_declaration_string
  in
  let rule =
    { Style_rule.selectors = dummy_selector ~loc:string_loc anonymous_class_name
    ; block = style_block
    ; loc = string_loc
    }
  in
  [ Rule.Style_rule rule ], string_loc
;;

let inferred_do_not_hash ~anonymous_class_name stylesheet =
  let style_sheet = Tuple2.map_fst ~f:Rule_id.identify_stylesheet stylesheet in
  let ( - ) = Set.remove in
  (Traverse_css.get_all_identifiers style_sheet
   |> String.Set.map ~f:Css_identifier.extract_css_identifier)
  - anonymous_class_name
  |> Set.to_list
;;

let ocaml_expression ({ txt; loc } : string Loc.t) =
  (* borrowed from [ppx_string] *)
  let lexbuf = Lexing.from_string txt in
  lexbuf.lex_abs_pos <- loc.loc_start.pos_cnum;
  lexbuf.lex_curr_p <- loc.loc_start;
  Parse.expression lexbuf
;;

module Find_anonymous_variables = struct
  type result =
    { anonymous_variables : Anonymous_variable.t list
    ; postprocessed_stylesheet : Stylesheet.t
    }

  let mapper ~f =
    object
      inherit Css_parser.Traverse.map as super

      method! component_value =
        function
        | Css_parser.Component_value.Ocaml_code (code, loc) ->
          Ident (f (code, loc), loc) |> super#component_value
        | other -> super#component_value other
    end
  ;;

  let f (stylesheet : Css_parser.Stylesheet.t) : result =
    let acc = ref Reversed_list.[] in
    let f ((code, sigil), string_loc) =
      let loc =
        let location_modifier =
          (* The ocaml tokenizer parses columns as 0-indexed but the css parser expects
             them to be 1-indexed. While the printed location is off by 1 when compared
             to other errors, the actual location that the user's cursor is taken to
             is correct when this value is 0. Leaving this here for symbolic purposes
          *)
          0
          (* We also have to add 2 to the start of the location since this is the code
             without the "%{" included, so we have to pretend like we've already parsed 
             the first 2 characters *)
          + 2
        in
        let offset_position position offset =
          { position with pos_cnum = position.pos_cnum + offset }
        in
        let loc_start = offset_position string_loc.loc_start location_modifier
        and loc_end =
          (* We have to subtract 1 from the end of the location because we're 
             missing the "}" that terminates the interpolated block here *)
          offset_position string_loc.loc_end (location_modifier - 1)
        in
        { string_loc with loc_start; loc_end }
      in
      let value, module_path =
        match Css_parser.Lexer.rsplit_on_hash code with
        | None -> code, None
        | Some (value, module_path) -> value, Some module_path
      in
      let ( (* Raise if invalid sigil + module path. [ #{<string_literal} ] does not allow 
              a module path, while [ %{<Css_gen_value>} ] allows for them.
            *) )
        =
        match sigil, module_path with
        | Interpolation_sigil.Hash, Some _ ->
          Location.raise_errorf
            ~loc:string_loc
            "#{ } interpolation blocks only accept string values and do not accept \
             module paths to convert to strings"
        | _ -> ()
      in
      let expression =
        let expression =
          Merlin_helpers.focus_expression
            (let value = ocaml_expression { txt = value; loc } in
             match module_path with
             | Some module_path ->
               let to_string_css =
                 let module_path_to_string = [%string "%{module_path}.to_string_css"] in
                 Ast_builder.Default.pexp_ident
                   ~loc
                   { txt = Longident.parse module_path_to_string; loc }
               in
               [%expr [%e to_string_css] [%e value]]
             | None -> [%expr [%e value]])
        in
        [%expr ([%e expression] : string)]
      in
      let anonymous_variable = Anonymous_variable.of_expression ~loc expression in
      acc := anonymous_variable :: !acc;
      let name = Anonymous_variable.name anonymous_variable in
      Anonymous_variable.Name.to_css_variable name
    in
    let postprocessed_stylesheet = (mapper ~f)#stylesheet stylesheet in
    let anonymous_variables = Reversed_list.rev !acc in
    { anonymous_variables; postprocessed_stylesheet }
  ;;
end

let anonymous_class_name location =
  let file_basename =
    match String.split ~on:'/' location.loc_start.pos_fname |> List.rev with
    | [] -> None
    | basename :: _ ->
      Option.map (String.lsplit2 ~on:'.' basename) ~f:(fun (name, _) ->
        String.filter name ~f:(function
          | '_' | 'a' .. 'z' | 'A' .. 'Z' -> true
          | _ -> false))
  in
  let name = "inline_class" in
  match file_basename with
  | None -> name
  | Some file_basename -> [%string "%{file_basename}__%{name}"]
;;

let create
  { Ppx_css_syntax.String_constant.css_string = original_declaration_string
  ; delimiter = _
  ; string_loc
  }
  =
  let anonymous_class_name = anonymous_class_name string_loc in
  let stylesheet =
    style_block_contents_string_to_stylesheet
      ~anonymous_class_name
      ~string_loc
      original_declaration_string
  in
  let inferred_do_not_hash = inferred_do_not_hash ~anonymous_class_name stylesheet in
  let%tydi { anonymous_variables; postprocessed_stylesheet } =
    Find_anonymous_variables.f stylesheet
  in
  let anonymous_variables = Anonymous_variable.Collection.of_list anonymous_variables in
  { original_declaration_string = { txt = original_declaration_string; loc = string_loc }
  ; inferred_do_not_hash
  ; anonymous_variables
  ; postprocessed_stylesheet
  ; anonymous_class_name
  }
;;

let always_hash t =
  let init = String.Set.singleton t.anonymous_class_name in
  List.fold t.anonymous_variables.variables ~init ~f:(fun acc variable ->
    let name = Anonymous_variable.name variable in
    Set.add acc ("--" ^ Anonymous_variable.Name.to_string name))
;;

let%expect_test "[always_hash]" =
  let test s =
    let string_constant =
      { Ppx_css_syntax.String_constant.css_string = s
      ; delimiter = Some ""
      ; string_loc = Location.none
      }
    in
    let result = create string_constant in
    print_s [%sexp (always_hash result : String.Set.t)]
  in
  test {|background-color: red;|};
  [%expect {| (inline_class) |}];
  (* (--red) is not hashed *)
  test {|background-color: var(--red);|};
  [%expect {| (inline_class) |}];
  test {|background-color: %{color};|};
  [%expect {| (--ppx_css__none__anon_variable_1 inline_class) |}]
;;

let%expect_test "[inferred_do_not_hash]" =
  let test s =
    let string_constant =
      { Ppx_css_syntax.String_constant.css_string = s
      ; delimiter = None
      ; string_loc = Location.none
      }
    in
    let result = create string_constant in
    print_s [%sexp (result.inferred_do_not_hash : string list)]
  in
  test {|background-color: red;|};
  [%expect {| () |}];
  test {|background-color: %{color};|};
  [%expect {| () |}];
  test {|background-color: %{color#Module.Foo};|};
  [%expect {| () |}];
  test
    {|
    background-color: red;
    background-color: var(--foo);
  |};
  [%expect {| (--foo) |}];
  (* No trailing semi-colon on last declaration. *)
  test
    {|
    background-color: red;
    background-color: var(--foo);
    background-color: var(--beep);
  |};
  [%expect {| (--beep --foo) |}];
  test
    {|
    background-color: var(--i-have-slashes);
  |};
  [%expect {| (--i-have-slashes) |}];
  test
    {|
    --tom: tomato;
    background-color: var(--tom);
  |};
  [%expect {| (--tom) |}]
;;

let to_stylesheet_string t = stylesheet_to_string t.postprocessed_stylesheet

let%expect_test _ =
  let test s =
    Anonymous_variable.For_testing.restart_identifiers ();
    let string_constant =
      { Ppx_css_syntax.String_constant.css_string = s
      ; delimiter = None
      ; string_loc = Location.none
      }
    in
    create string_constant |> to_stylesheet_string |> print_endline
  in
  test {|background-color: blue;|};
  [%expect
    {|
    .inline_class {
      background-color: blue;
    }
    |}];
  test {|background-color: %{color};|};
  [%expect
    {|
    .inline_class {
      background-color: var(--ppx_css__none__anon_variable_1);
    }
    |}];
  test {|background-color: %{color#Module.Foo};|};
  [%expect
    {|
    .inline_class {
      background-color: var(--ppx_css__none__anon_variable_1);
    }
    |}];
  test
    {|
    background-color: red;
    background-color: var(--foo);
    --tom: tomato;
    --tom: %{color};
    background-color: %{f () () ()};
    background-color: %{g ()#Mod.Mod};

  |};
  [%expect
    {|
    .inline_class {
      background-color: red;
      background-color: var(--foo);
      --tom: tomato;
      --tom: var(--ppx_css__none__anon_variable_1);
      background-color: var(--ppx_css__none__anon_variable_2);
      background-color: var(--ppx_css__none__anon_variable_3);
    }
    |}]
;;

let inferred_do_not_hash t = t.inferred_do_not_hash

module For_stylesheet = struct
  type t =
    { original_stylesheet_string : string loc
    ; substituted_stylesheet : Stylesheet.t
    ; anonymous_variables : Anonymous_variable.Collection.t
    }

  let create
    { Ppx_css_syntax.String_constant.css_string = stylesheet_string
    ; string_loc
    ; delimiter = _
    }
    =
    let original_stylesheet_string = { txt = stylesheet_string; loc = string_loc } in
    let%tydi { anonymous_variables; postprocessed_stylesheet } =
      Find_anonymous_variables.f
        (parse_stylesheet
           ~parsing_config:Css_parser.Parsing_config.raise_on_recoverable_errors
           ~pos:string_loc.loc_start
           stylesheet_string)
    in
    let anonymous_variables = Anonymous_variable.Collection.of_list anonymous_variables in
    { original_stylesheet_string
    ; anonymous_variables
    ; substituted_stylesheet = postprocessed_stylesheet
    }
  ;;

  let anonymous_variables t = t.anonymous_variables
  let to_stylesheet_string t = stylesheet_to_string t.substituted_stylesheet

  let always_hash t =
    List.fold
      t.anonymous_variables.variables
      ~init:String.Set.empty
      ~f:(fun acc variable ->
        let name = Anonymous_variable.name variable in
        Set.add acc ("--" ^ Anonymous_variable.Name.to_string name))
  ;;
end
