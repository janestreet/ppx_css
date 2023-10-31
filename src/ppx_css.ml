open! Core
open! Ppxlib
open Css_jane
module Preprocess_arguments = Ppx_css_syntax.Preprocess_arguments

module With_hoisted_expression = struct
  type 'a t =
    { txt : 'a
    ; ppx_css_string_expression : expression
    }
end

module Expansion_kind = struct
  type t =
    | Stylesheet
    | Styled_component of Anonymous_declarations.t
end

let () =
  Driver.add_arg
    "-rewrite"
    ~doc:
      "{string:string} Lets you override or rewrite the identifier that is used for an \
       identifier. Syntax: '-rewrite=classname:new_classname'"
    (String
       (fun s ->
         match String.lsplit2 s ~on:':' with
         | Some (from, to_) -> Preprocess_arguments.add_rewrite ~from ~to_
         | None ->
           raise_s
             [%message
               "Invalid '-rewrite' arg"
                 ~expected_something_like:"existing_identifier:identifier_to_use"
                 ~but_got:(s : string)]))
;;

let () =
  Driver.add_arg
    "-dont-hash"
    ~doc:
      "{string} Disable hashing behavior for an identifier. Syntax: \
       '-dont-hash=table-header'"
    (String (fun s -> Preprocess_arguments.add_dont_hash s))
;;

let () =
  Driver.add_arg
    "-dont-hash-prefixes"
    ~doc:
      "{string} Disable hashing behavior for an identifier. Syntax: \
       '-dont-hash=table-header'"
    (String (fun s -> Preprocess_arguments.add_dont_hash_prefixes s))
;;

let disable_warning_32 ~loc =
  let open (val Ast_builder.make loc) in
  attribute
    ~name:(Located.mk "ocaml.warning")
    ~payload:(PStr [ pstr_eval (estring "-32") [] ])
;;

let loc_ghoster =
  object
    inherit Ast_traverse.map as super
    method! location location = super#location { location with loc_ghost = true }
  end
;;

let var_builder_signature ~loc ~user_variables : signature_item option =
  let open (val Ast_builder.make loc) in
  match List.is_empty user_variables with
  | true -> None
  | false ->
    let variables = List.sort user_variables ~compare:String.compare in
    let create_function_type ~name ~return_type ~create_arg =
      let set_function_type =
        List.fold_right variables ~init:return_type ~f:(fun variable_name acc ->
          ptyp_arrow (create_arg variable_name) [%type: string] acc)
      in
      psig_value
        (value_description ~name:(Located.mk name) ~prim:[] ~type_:set_function_type)
    in
    let set =
      create_function_type
        ~name:"set"
        ~return_type:[%type: unit -> Virtual_dom.Vdom.Attr.t]
        ~create_arg:(fun arg_name -> Optional arg_name)
    in
    let set_all =
      create_function_type
        ~name:"set_all"
        ~return_type:[%type: Virtual_dom.Vdom.Attr.t]
        ~create_arg:(fun arg_name -> Labelled arg_name)
    in
    let type_ = pmty_signature [ set; set_all ] in
    let out =
      psig_module (module_declaration ~name:(Located.mk (Some "Variables")) ~type_)
    in
    Some out
;;

let module_type_of_identifiers
  ~loc
  ~(identifiers : (string * [ `Both | `Only_class | `Only_id ]) list)
  ~user_variables
  =
  let open (val Ast_builder.make loc) in
  let var_builder = var_builder_signature ~loc ~user_variables in
  let identifier_keys = identifiers |> List.map ~f:Tuple2.get1 in
  let string_module =
    let signature_items =
      identifier_keys @ user_variables
      |> List.dedup_and_sort ~compare:String.compare
      |> List.map ~f:(fun ident ->
           let type_ = [%type: string] in
           let name = Located.mk ident in
           psig_value (value_description ~name ~type_ ~prim:[]))
    in
    let type_ = pmty_signature signature_items in
    psig_module (module_declaration ~name:(Located.mk (Some "For_referencing")) ~type_)
  in
  let identifier_signature_items =
    identifiers
    (* The [dedup_and_sort] below only really cares about the [dedup] behaviour. We need
       to dedup the list so that we don't end up with duplicate declarations in the
       signature. The sorting is a nice bonus for output stability. *)
    |> List.dedup_and_sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
    |> List.concat_map ~f:(fun (ident, case) ->
         let type_ = [%type: Virtual_dom.Vdom.Attr.t] in
         let name = Located.mk ident in
         match case with
         | `Only_class | `Only_id ->
           [ psig_value (value_description ~name ~type_ ~prim:[]) ]
         | `Both ->
           let id_name = Located.mk [%string "%{ident}_id"] in
           let class_name = Located.mk [%string "%{ident}_class"] in
           let error_attribute =
             let error_message =
               pexp_constant
                 (Pconst_string
                    ( sprintf
                        "An id and a class both share the name \"%s\" which is \
                         ambiguous. Please use \"%s_id\" or \"%s_class\" instead."
                        ident
                        ident
                        ident
                    , loc
                    , None ))
             in
             let payload = PStr [ pstr_eval [%expr unsafe [%e error_message]] [] ] in
             attribute ~name:(Located.mk "alert") ~payload
           in
           [ psig_value
               { (value_description ~name ~type_ ~prim:[]) with
                 pval_attributes = [ error_attribute ]
               }
           ; psig_value (value_description ~name:id_name ~type_ ~prim:[])
           ; psig_value (value_description ~name:class_name ~type_ ~prim:[])
           ])
  in
  let base = string_module :: identifier_signature_items in
  Option.value_map var_builder ~f:(fun var_builder -> var_builder :: base) ~default:base
  |> List.map ~f:loc_ghoster#signature_item
;;

let css_string_to_expression ~loc ~css_string ~(reference_order : expression list) =
  let open (val Ast_builder.make loc) in
  (* The [Some ""] means that the string will use the multiline string literal
     syntax, but with no termination identifier. *)
  let string_constant l = pexp_constant (Pconst_string (l, loc, Some "")) in
  match List.is_empty reference_order with
  | true -> string_constant css_string
  | false ->
    let args =
      List.map (string_constant css_string :: reference_order) ~f:(fun arg ->
        Nolabel, arg)
    in
    pexp_apply [%expr Base.Printf.sprintf] args
;;

let create_type_info_function ~loc ~stylesheet_location =
  let open (val Ast_builder.make loc) in
  let name =
    (* We give ppat_var the same exact location as the css string so that
       MerlinTypeOf thinks the string is of the the type with all of the
       that ppx_css can take. *)
    let open (val Ast_builder.make stylesheet_location) in
    ppat_var (Located.mk "__type_info_for_ppx_css")
  in
  pstr_value
    Nonrecursive
    [ value_binding
        ~pat:
          [%pat?
            ([%p name] :
              ?rewrite:(string * string) list
              -> ?dont_hash:string list
              -> ?dont_hash_prefixes:string list
              -> string
              -> unit)]
        ~expr:[%expr fun ?rewrite:_ ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()]
    ]
;;

module Mint_hygenic_identifier = struct
  type result =
    { expression : expression
    ; pattern : pattern
    }

  let f ~loc ?prefix () =
    let open (val Ast_builder.make loc) in
    let string = gen_symbol ?prefix () in
    let expression = pexp_ident (Located.mk (Lident string)) in
    let pattern = ppat_var (Located.mk string) in
    { expression; pattern }
  ;;
end

(* Produces:
   {[
     module Variables = struct
       let set ?var1 ?var_2 () =
         let acc = [] in
         let acc = match var1 with | None -> acc
                                   | Some value -> ("--var1", value) :: acc
         in
         Vdom.Attr.__vars_kebabless acc
       ;;
     end
   ]} *)
let var_builder_structure ~loc ~variables ~maybe_combine_with_anonymous_variables
  : structure_item option
  =
  let open (val Ast_builder.make loc) in
  match List.is_empty variables with
  | true -> None
  | false ->
    let variables =
      List.sort variables ~compare:(fun (a, _) (b, _) -> String.compare a b)
    in
    let { Mint_hygenic_identifier.expression = acc_expression; pattern = acc_pattern } =
      Mint_hygenic_identifier.f ~loc ~prefix:"ppx_css_acc" ()
    in
    let initial_acc_binding ~in_ =
      (* produces {[ let acc = [] in in_ ]} *)
      pexp_let Nonrecursive [ value_binding ~pat:acc_pattern ~expr:[%expr []] ] in_
    in
    let inline_folding_of_acc ~in_ =
      (* Produces:

         {[
           let acc = match var1 with
             | None -> acc
             | Some value -> ("--var1", value) :: acc
           in
           let acc = match var2 with
             | None -> acc
             | Some value -> ("--var2", value) :: acc
           in
           in_ ]}
      *)
      let { Mint_hygenic_identifier.expression = value_expression
          ; pattern = value_pattern
          }
        =
        Mint_hygenic_identifier.f ~loc ~prefix:"ppx_css_value" ()
      in
      List.fold_right
        variables
        ~init:in_
        ~f:(fun (ocaml_identifier, variable_expression) acc ->
        let ocaml_identifier_expression =
          pexp_ident (Located.mk (Lident ocaml_identifier))
        in
        let expr =
          [%expr
            match [%e ocaml_identifier_expression] with
            | None -> [%e acc_expression]
            | Some [%p value_pattern] ->
              ([%e variable_expression], [%e value_expression]) :: [%e acc_expression]]
        in
        pexp_let Nonrecursive [ value_binding ~pat:acc_pattern ~expr ] acc)
    in
    let call_to_vdom_attr_acc =
      let actual_call =
        [%expr Virtual_dom.Vdom.Attr.__css_vars_no_kebabs [%e acc_expression]]
      in
      match maybe_combine_with_anonymous_variables with
      | None -> actual_call
      | Some unique_name ->
        let anonymous_variables_setter = pexp_ident (Located.mk (Lident unique_name)) in
        [%expr
          Virtual_dom.Vdom.Attr.combine [%e anonymous_variables_setter] [%e actual_call]]
    in
    let set_function_body =
      initial_acc_binding ~in_:(inline_folding_of_acc ~in_:call_to_vdom_attr_acc)
    in
    let set_function_expression =
      List.fold_right
        variables
        ~init:[%expr fun () -> [%e set_function_body]]
        ~f:(fun (k, _) acc -> pexp_fun (Optional k) None (ppat_var (Located.mk k)) acc)
    in
    (* NOTE: A hygenic name is minted in case there is a collision with a variable
       parameter called [set]. *)
    let unique_set_name = gen_symbol ~prefix:"ppx_css_variable_set" () in
    let unique_set =
      pstr_value
        Nonrecursive
        [ value_binding
            ~pat:(ppat_var (Located.mk unique_set_name))
            ~expr:set_function_expression
        ]
    in
    let set =
      pstr_value
        Nonrecursive
        [ value_binding
            ~pat:[%pat? set]
            ~expr:(pexp_ident (Located.mk (Lident unique_set_name)))
        ]
    in
    let set_all =
      let set_all_body =
        pexp_apply
          (pexp_ident (Located.mk (Lident unique_set_name)))
          ((Nolabel, [%expr ()])
           :: List.map variables ~f:(fun (identifier, _) ->
                Labelled identifier, pexp_ident (Located.mk (Lident identifier))))
      in
      let set_all_function_expression =
        List.fold_right variables ~init:set_all_body ~f:(fun (identifier, _) acc ->
          pexp_fun (Labelled identifier) None (ppat_var (Located.mk identifier)) acc)
      in
      pstr_value
        Nonrecursive
        [ value_binding ~pat:[%pat? set_all] ~expr:set_all_function_expression ]
    in
    let expr = pmod_structure [ unique_set; set; set_all ] in
    let out = pstr_module (module_binding ~name:(Located.mk (Some "Variables")) ~expr) in
    Some out
;;

let calculate_user_variables ~variables ~anonymous_variables =
  let anonymous_variable_names =
    List.map
      anonymous_variables.Anonymous_variable.Collection.variables
      ~f:(fun anonymous_variable ->
      Anonymous_variable.name anonymous_variable |> Anonymous_variable.Name.to_string)
    |> String.Set.of_list
  in
  List.filter variables ~f:(fun (name, _) -> not (Set.mem anonymous_variable_names name))
;;

let validate_no_collisions_after_warnings_and_rewrites
  ~loc
  ~(identifiers : (label * [ `Both | `Only_class | `Only_id ]) list)
  =
  (* This function only checks that there are no collisions from the potentially newly
     minted names that occur from occurrances on `Both. Since original ^ "_id" and
     original ^ "_class"  are added, these conditions must be checked for. *)
  let all_identifiers = String.Set.of_list (List.map identifiers ~f:Tuple2.get1) in
  let newly_minted_names =
    String.Set.of_list
      (List.concat_map identifiers ~f:(fun (label, case) ->
         match case with
         | `Both -> [ label ^ "_id"; label ^ "_class" ]
         | `Only_class | `Only_id -> []))
  in
  let conflicts = Set.inter all_identifiers newly_minted_names in
  match Set.is_empty conflicts with
  | true -> ()
  | false ->
    Location.raise_errorf
      ~loc
      "Collision between identifiers! This occurs when a disambiguated identifier \
       matches an existing identifier. To resolve this, rename the following \
       identifiers: %s."
      (Sexp.to_string_hum ([%sexp_of: String.Set.t] conflicts))
;;

(* Creates the module struct that - given "var1" and "var2" as variables, and "classname_1"
   as an identifier  will create the below code:

   {[
     module Default = struct
       module Variables = struct
         let set ?var1 ?var2 () =
           let acc = [] in
           let acc = match var1 with
             | None -> acc
             | Some value -> ("--var1", value) :: acc
           in
           let acc = match var2 with
             | None -> acc
             | Some value -> ("--var2", value) :: acc
           in
           Vdom.Attr.__vars_kebabless acc
         ;;
       end
       let classname_1 = "classname-1_hash_2341"
     end
   ]}*)
let create_default_module_struct
  ~loc
  ~(identifiers : (label * ([ `Both | `Only_class | `Only_id ] * expression)) list)
  ~variables
  ~(expansion_kind : Expansion_kind.t)
  ~(anonymous_variables : Anonymous_variable.Collection.t)
  : module_expr
  =
  validate_no_collisions_after_warnings_and_rewrites
    ~loc
    ~identifiers:(List.map identifiers ~f:(Tuple2.map_snd ~f:Tuple2.get1));
  let open (val Ast_builder.make loc) in
  let anonymous_variable_setter : structure_item option =
    match anonymous_variables.variables with
    | [] -> None
    | _ :: _ ->
      let open struct
        type t =
          { css_variable_name_after_name_resolution : expression
          ; anonymous_variable : Anonymous_variable.t
          }
      end in
      let unique_name = anonymous_variables.unique_name in
      let anonymous_variables =
        List.filter_map anonymous_variables.variables ~f:(fun anonymous_variable ->
          let%map.Option css_variable_name_after_name_resolution =
            List.Assoc.find
              variables
              ~equal:[%equal: string]
              (Anonymous_variable.name anonymous_variable
               |> Anonymous_variable.Name.to_string)
          in
          With_temporary_name.mint
            { css_variable_name_after_name_resolution; anonymous_variable })
      in
      let call_to_css_var_no_kebabs =
        let kebabs =
          List.fold_right
            anonymous_variables
            ~init:[%expr []]
            ~f:
              (fun
                { txt =
                    { css_variable_name_after_name_resolution; anonymous_variable = _ }
                ; temporary_name
                }
                acc
                ->
            let temporary_name = pexp_ident (Located.mk (Lident temporary_name)) in
            [%expr
              (::)
                ( ([%e css_variable_name_after_name_resolution], [%e temporary_name])
                , [%e acc] )])
        in
        [%expr Virtual_dom.Vdom.Attr.__css_vars_no_kebabs [%e kebabs]]
      in
      let setter_expression =
        List.fold_right
          anonymous_variables
          ~init:call_to_css_var_no_kebabs
          ~f:
            (fun
              { txt = { css_variable_name_after_name_resolution = _; anonymous_variable }
              ; temporary_name
              }
              acc
              ->
          pexp_let
            Nonrecursive
            [ (let pat = ppat_var (Located.mk temporary_name) in
               value_binding ~pat ~expr:(Anonymous_variable.expression anonymous_variable))
            ]
            acc)
      in
      let setter_structure_item =
        pstr_value
          Nonrecursive
          [ value_binding ~pat:(ppat_var (Located.mk unique_name)) ~expr:setter_expression
          ]
      in
      Some setter_structure_item
  in
  let variable_module =
    match expansion_kind with
    | Stylesheet ->
      let user_variables = calculate_user_variables ~variables ~anonymous_variables in
      var_builder_structure
        ~loc
        ~variables:user_variables
        ~maybe_combine_with_anonymous_variables:
          (Option.map anonymous_variable_setter ~f:(fun _ ->
             anonymous_variables.unique_name))
    | Styled_component _ -> None
  in
  let identifiers_structure_items =
    let maybe_set_anon_variables expression =
      match anonymous_variables.variables with
      | [] -> expression
      | _ :: _ ->
        let preconstructed_attr_expression =
          pexp_ident (Located.mk (Lident anonymous_variables.unique_name))
        in
        [%expr
          Virtual_dom.Vdom.Attr.combine
            [%e expression]
            [%e preconstructed_attr_expression]]
    in
    identifiers
    |> List.concat_map ~f:(fun (original_name, (case, e)) ->
         match case with
         | `Both ->
           let id_string = original_name ^ "_id" in
           let class_string = original_name ^ "_class" in
           let id_pattern = ppat_var (Located.mk id_string) in
           let original_pattern = ppat_var (Located.mk original_name) in
           let class_pattern = ppat_var (Located.mk class_string) in
           [ [%stri let [%p original_pattern] = Virtual_dom.Vdom.Attr.empty]
           ; [%stri
               let [%p class_pattern] =
                 [%e maybe_set_anon_variables [%expr Virtual_dom.Vdom.Attr.class_ [%e e]]]
               ;;]
           ; [%stri let [%p id_pattern] = Virtual_dom.Vdom.Attr.id [%e e]]
           ]
         | `Only_class ->
           [ [%stri
               let [%p ppat_var (Located.mk original_name)] =
                 [%e maybe_set_anon_variables [%expr Virtual_dom.Vdom.Attr.class_ [%e e]]]
               ;;]
           ]
         | `Only_id ->
           [ [%stri
               let [%p ppat_var (Located.mk original_name)] =
                 [%e maybe_set_anon_variables [%expr Virtual_dom.Vdom.Attr.id [%e e]]]
               ;;]
           ])
  in
  let identifiers_and_variables_as_string =
    List.map identifiers ~f:(fun (label, (_, expression)) -> label, expression)
    @ calculate_user_variables ~variables ~anonymous_variables
    |> List.map ~f:(fun (k, e) -> [%stri let [%p ppat_var (Located.mk k)] = [%e e]])
  in
  let base =
    match expansion_kind with
    | Styled_component _ -> identifiers_structure_items
    | Stylesheet ->
      let string_module =
        pstr_module
          (module_binding
             ~name:(Located.mk (Some "For_referencing"))
             ~expr:(pmod_structure identifiers_and_variables_as_string))
      in
      string_module :: identifiers_structure_items
  in
  let structure_items =
    List.filter_opt [ anonymous_variable_setter; variable_module ] @ base
  in
  pmod_structure structure_items |> loc_ghoster#module_expr
;;

let generate_struct_from_css_string_and_options
  ~loc
  ~rewrite
  ~css_string
  ~dont_hash_prefixes
  ~stylesheet_location
  ~(expansion_kind : Expansion_kind.t)
  ~unused_allow_set
  ~always_hash
  ~anonymous_variables
  : structure With_hoisted_expression.t
  =
  let open (val Ast_builder.make loc) in
  let { Traverse_css.Transform.css_string; identifier_mapping; reference_order } =
    Traverse_css.Transform.f
      ~loc
      ~pos:loc.loc_start
      ~rewrite
      ~css_string
      ~dont_hash_prefixes
      ~unused_allow_set
      ~always_hash
  in
  let identifier_mapping = Hashtbl.to_alist identifier_mapping in
  let css_string = css_string_to_expression ~loc ~css_string ~reference_order in
  let variables =
    List.filter_map identifier_mapping ~f:(fun (k, (identifier_kinds, e)) ->
      match Set.mem identifier_kinds Variable with
      | false -> None
      | true -> Some (k, e))
  in
  let identifiers =
    List.filter_map identifier_mapping ~f:(fun (k, (identifier_kinds, e)) ->
      match Set.mem identifier_kinds Class, Set.mem identifier_kinds Id with
      | true, true -> Some (k, (`Both, e))
      | true, false -> Some (k, (`Only_class, e))
      | false, true -> Some (k, (`Only_id, e))
      | false, false -> None)
  in
  let t_module =
    create_default_module_struct
      ~loc
      ~identifiers
      ~variables
      ~expansion_kind
      ~anonymous_variables
  in
  let structure =
    match expansion_kind with
    | Styled_component _ -> [ [%stri include [%m t_module]] ]
    | Stylesheet ->
      (* We want to remove the variables that were introduced during the interpolation. *)
      let user_variables =
        calculate_user_variables ~variables ~anonymous_variables |> List.map ~f:fst
      in
      let t_sig =
        module_type_of_identifiers
          ~loc
          ~identifiers:(List.map identifiers ~f:(Tuple2.map_snd ~f:Tuple2.get1))
          ~user_variables
        |> pmty_signature
      in
      [ pstr_attribute (disable_warning_32 ~loc)
      ; create_type_info_function ~loc ~stylesheet_location
      ; [%stri module type S = [%m t_sig]]
      ; [%stri type t = (module S)]
      ; [%stri module Default : S = [%m t_module]]
      ; [%stri include Default]
      ; [%stri let default : t = (module Default)]
      ]
  in
  { With_hoisted_expression.txt = structure; ppx_css_string_expression = css_string }
;;

let generate_struct ~loc (expr : expression) =
  let loc = { loc with loc_ghost = true } in
  let expr = loc_ghoster#expression expr in
  let { Ppx_css_syntax.rewrite; css_string; stylesheet_location; dont_hash_prefixes } =
    Ppx_css_syntax.parse_stylesheet expr
  in
  let anonymous_declarations =
    Anonymous_declarations.For_stylesheet.create
      ~string_loc:stylesheet_location
      css_string
  in
  generate_struct_from_css_string_and_options
    ~expansion_kind:Stylesheet
    ~loc
    ~rewrite
    ~css_string:
      (Anonymous_declarations.For_stylesheet.to_stylesheet_string anonymous_declarations)
    ~dont_hash_prefixes
    ~stylesheet_location
    ~unused_allow_set:String.Set.empty
    ~always_hash:
      (Anonymous_declarations.For_stylesheet.always_hash anonymous_declarations)
    ~anonymous_variables:
      (Anonymous_declarations.For_stylesheet.anonymous_variables anonymous_declarations)
;;

let generate_expression_from_css_declarations_and_options
  ~loc
  ~rewrite
  ~anonymous_declarations
  ~dont_hash_prefixes
  ~stylesheet_location
  : expression With_hoisted_expression.t
  =
  let open (val Ast_builder.make loc) in
  let inferred_do_not_hash =
    Anonymous_declarations.inferred_do_not_hash anonymous_declarations
  in
  let rewrite =
    List.fold inferred_do_not_hash ~init:rewrite ~f:(fun rewrite dont_hash ->
      match Map.add rewrite ~key:dont_hash ~data:(estring dont_hash) with
      | `Ok rewrite -> rewrite
      | `Duplicate ->
        (* NOTE: [rewrite] takes precedence over [inferred_dont_hash] *)
        rewrite)
  in
  let { With_hoisted_expression.txt = module_; ppx_css_string_expression } =
    let unused_allow_set = String.Set.of_list inferred_do_not_hash in
    generate_struct_from_css_string_and_options
      ~expansion_kind:(Styled_component anonymous_declarations)
      ~loc
      ~css_string:(Anonymous_declarations.to_stylesheet_string anonymous_declarations)
      ~rewrite
      ~dont_hash_prefixes
      ~stylesheet_location
      ~unused_allow_set
      ~always_hash:(Anonymous_declarations.always_hash anonymous_declarations)
      ~anonymous_variables:
        (Anonymous_declarations.anonymous_variables anonymous_declarations)
  in
  let style_module_name = gen_symbol ~prefix:"Ppx_css_anonymous_style" () in
  let body =
    pexp_ident
      (Located.mk
         (Ldot (Lident style_module_name, Anonymous_declarations.anonymous_class_name)))
  in
  { With_hoisted_expression.txt =
      pexp_letmodule (Located.mk (Some style_module_name)) (pmod_structure module_) body
  ; ppx_css_string_expression
  }
;;

let generate_inline_expression ~loc (expr : expression)
  : expression With_hoisted_expression.t
  =
  let open (val Ast_builder.make loc) in
  let loc = { loc with loc_ghost = true } in
  let expr = loc_ghoster#expression expr in
  let { Ppx_css_syntax.rewrite; css_string; stylesheet_location; dont_hash_prefixes } =
    Ppx_css_syntax.parse_inline_expression expr
  in
  let anonymous_declarations =
    Anonymous_declarations.create ~string_loc:stylesheet_location css_string
  in
  generate_expression_from_css_declarations_and_options
    ~loc
    ~rewrite
    ~anonymous_declarations
    ~dont_hash_prefixes
    ~stylesheet_location
;;

let create_sig_from_idents
  ~loc
  ~(identifiers : (string * [> `Both | `Only_class | `Only_id ]) list)
  ~user_variables
  =
  let open (val Ast_builder.make loc) in
  validate_no_collisions_after_warnings_and_rewrites ~loc ~identifiers;
  let basic_sig = module_type_of_identifiers ~loc ~identifiers ~user_variables in
  pmty_signature
    ([ [%sigi: module type S = [%m pmty_signature basic_sig]]
     ; [%sigi: type t = (module S)]
     ; [%sigi: val default : t]
     ]
     @ basic_sig)
;;

module For_css_inliner = struct
  let gen_struct ~rewrite ~css_string ~dont_hash_prefixes ~stylesheet_location =
    let buffer = Buffer.create 1024 in
    let loc = Location.none in
    let%tydi { txt = module_expr; ppx_css_string_expression } =
      generate_struct_from_css_string_and_options
        ~expansion_kind:Stylesheet
        ~loc
        ~rewrite
        ~css_string
        ~dont_hash_prefixes
        ~stylesheet_location
        ~unused_allow_set:String.Set.empty
        ~always_hash:String.Set.empty
        ~anonymous_variables:Anonymous_variable.Collection.empty
    in
    let module_expr =
      let open (val Ast_builder.make loc) in
      pmod_structure
        (Hoister.ppx_css_expression_to_structure_item ~loc ppx_css_string_expression
         :: module_expr)
    in
    Pprintast.module_expr (Format.formatter_of_buffer buffer) module_expr;
    Buffer.contents buffer
  ;;

  open Traverse_css

  let gen_sig css =
    let buffer = Buffer.create 1024 in
    let stylesheet = Stylesheet.of_string css in
    let { Get_all_identifiers.identifiers; variables } =
      Get_all_identifiers.ocaml_identifiers stylesheet
    in
    let mli_as_an_ast =
      (* interpolation is currently not allowed in the css inliner so all variables are
         user variables. *)
      create_sig_from_idents ~loc:Location.none ~identifiers ~user_variables:variables
    in
    Pprintast.module_type (Format.formatter_of_buffer buffer) mli_as_an_ast;
    Buffer.contents buffer
  ;;
end

let ml_extension =
  Extension.declare
    "css"
    Extension.Context.module_expr
    Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path:_ (expr : expression) ->
      let%tydi { txt = module_; ppx_css_string_expression } = generate_struct ~loc expr in
      Hoister.register ~ppx_css_string_expression;
      let open (val Ast_builder.make loc) in
      pmod_structure module_)
;;

let inline_extension =
  Extension.declare
    "css"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path:_ (expr : expression) ->
      let%tydi { txt = expression; ppx_css_string_expression } =
        generate_inline_expression ~loc expr
      in
      Hoister.register ~ppx_css_string_expression;
      expression)
;;

let private_register_ppx_css =
  Extension.declare_inline
    "css.__private_register_css"
    Extension.Context.structure_item
    Ast_pattern.(pstr nil)
    (fun ~loc ~path:_ ->
      let loc = { loc with loc_ghost = true } in
      match Hoister.is_empty () with
      | true -> []
      | false -> [ Hoister.create_hoisted_module ~loc ])
;;

let enclose_impl (loc : location option) =
  let header = [] in
  let footer =
    Option.value_map loc ~default:[] ~f:(fun loc -> [%str [%%css.__private_register_css]])
  in
  header, footer
;;

let () =
  Driver.register_transformation
    "css"
    ~enclose_impl
    ~extensions:[ ml_extension; inline_extension; private_register_ppx_css ]
;;

module For_testing = struct
  let generate_struct = generate_struct ~loc:Location.none
  let generate_inline_expression = generate_inline_expression ~loc:Location.none
  let map_style_sheet = Traverse_css.For_testing.map_style_sheet

  module Traverse_css = Traverse_css

  let ppx_css_expression_to_structure_item = Hoister.ppx_css_expression_to_structure_item
end
