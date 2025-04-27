open! Core
open! Ppxlib
open Css_parser
module Preprocess_arguments = Ppx_css_syntax.Preprocess_arguments
module Group_type = Traverse_css.Graph.Group_type
module Rule_id = Rule_id
module Stable_stylesheet = Stable_stylesheet
module Css_identifier = Css_identifier

module With_hoisted_expression = struct
  type 'a t =
    { txt : 'a
    ; hoisted_structure_items : structure_item list
    ; css_string_for_testing : string Lazy.t
    }
end

module Expansion_kind = struct
  type t =
    | Stylesheet
    | Styled_component of Anonymous_declarations.t
end

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

let () =
  Driver.add_arg
    "-lazy-loading-optimization"
    ~doc:
      "{boolean} turns on the lazy loading optimization. Currently experimental, please \
       audit your application's CSS if you turn this on "
    (Bool (fun v -> Preprocess_arguments.set_lazy_loading_optimization (Some v)))
;;

let disable_warning_num ~loc num =
  let open (val Ast_builder.make loc) in
  pstr_attribute
    (attribute
       ~name:(Located.mk "ocaml.warning")
       ~payload:(PStr [ pstr_eval (estring ("-" ^ Int.to_string num)) [] ]))
;;

let disable_warning_32 ~loc = disable_warning_num ~loc 32

let loc_ghoster =
  object
    inherit Ast_traverse.map as super
    method! location location = super#location { location with loc_ghost = true }
  end
;;

let var_builder_signature ~loc ~identifiers : signature_item option =
  let open (val Ast_builder.make loc) in
  let user_variables =
    Set.to_list identifiers
    |> List.filter_map ~f:(function
      | Css_identifier.Variable _ as variable ->
        Some (Css_identifier.extract_ocaml_identifier variable)
      | _ -> None)
  in
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

let module_type_of_identifiers ~loc ~identifiers =
  let open (val Ast_builder.make loc) in
  let var_builder = var_builder_signature ~loc ~identifiers in
  let identifier_keys =
    String.Set.map identifiers ~f:Css_identifier.extract_ocaml_identifier
  in
  let string_module =
    let signature_items =
      identifier_keys
      |> Set.to_list
      |> List.map ~f:(fun ident ->
        let type_ = [%type: string] in
        let name = Located.mk ident in
        psig_value (value_description ~name ~type_ ~prim:[]))
    in
    let type_ = pmty_signature signature_items in
    psig_module (module_declaration ~name:(Located.mk (Some "For_referencing")) ~type_)
  in
  let identifier_signature_items =
    Set.to_list identifiers
    |> Css_identifier.group_by_ocaml_identifier
    |> Map.to_alist
    |> List.concat_map ~f:(fun (ocaml_identifier, identifiers) ->
      let type_ = [%type: Virtual_dom.Vdom.Attr.t] in
      let name = Located.mk ocaml_identifier in
      match Css_identifier.set_includes_class_or_id identifiers with
      | `None -> []
      | `Only_class | `Only_id -> [ psig_value (value_description ~name ~type_ ~prim:[]) ]
      | `Both ->
        let id_name = Located.mk [%string "%{ocaml_identifier}_id"] in
        let class_name = Located.mk [%string "%{ocaml_identifier}_class"] in
        let error_attribute =
          let error_message =
            pexp_constant
              (Pconst_string
                 ( sprintf
                     "An id and a class both share the name \"%s\" which is ambiguous. \
                      Please use \"%s_id\" or \"%s_class\" instead."
                     ocaml_identifier
                     ocaml_identifier
                     ocaml_identifier
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
              ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit)]
        ~expr:[%expr fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()]
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

let anonymous_variable_names_as_set anonymous_variables =
  List.map
    anonymous_variables.Anonymous_variable.Collection.variables
    ~f:(fun anonymous_variable ->
      Anonymous_variable.name anonymous_variable |> Anonymous_variable.Name.to_string)
  |> String.Set.of_list
;;

let is_not_anonymous_variable ~anonymous_variable_names = function
  | Css_identifier.Variable _ as variable ->
    let ocaml_identifier = Css_identifier.extract_ocaml_identifier variable in
    not (Set.mem anonymous_variable_names ocaml_identifier)
  | _ -> true
;;

let filter_anonymous_variables_from_identifiers ~anonymous_variables identifiers =
  let anonymous_variable_names = anonymous_variable_names_as_set anonymous_variables in
  Set.filter identifiers ~f:(is_not_anonymous_variable ~anonymous_variable_names)
;;

let validate_no_collisions_after_warnings_and_rewrites
  ~loc
  ~(identifiers : Css_identifier.Set.t)
  =
  (* This function only checks that there are no collisions from the potentially newly
     minted names that occur from occurrances on `Both. Since original ^ "_id" and
     original ^ "_class"  are added, these conditions must be checked for. *)
  let all_identifiers =
    String.Set.map identifiers ~f:Css_identifier.extract_ocaml_identifier
  in
  let identifiers =
    Css_identifier.group_by_ocaml_identifier (Set.to_list identifiers) |> Map.to_alist
  in
  let newly_minted_names =
    String.Set.of_list
      (List.concat_map identifiers ~f:(fun (ocaml_identifier, identifiers) ->
         match Css_identifier.set_includes_class_or_id identifiers with
         | `Both -> [ ocaml_identifier ^ "_id"; ocaml_identifier ^ "_class" ]
         | `Only_class | `Only_id | `None -> []))
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

let stylesheet_to_string (sheet, loc) =
  let css_string = Css_parser.stylesheet_to_string (sheet, loc) in
  sprintf
    "\n/* %s */\n\n%s"
    (Ppx_here_expander.expand_filename loc.loc_start.pos_fname)
    (String.strip css_string)
;;

let stylesheet_to_rule_strings (sheet, loc) =
  Map.map sheet ~f:(fun rule -> stylesheet_to_string ([ rule ], loc))
;;

(* Creates the struct that contains the side effects required to create the
   [CSSStylesheet]s as well as update them. 

   Also creates functions necessary for accessing the lazy functions created within the 
   hoisted module and forcing them

   [assert_post_conditions] makes sure that all expressions and patterns that were 
   generated in the hoisting module were called symmetrically, meaning that each variable
   created was referenced at least once in an expression.
*)
type hoisted_module_struct_result =
  { struct_to_hoist : structure
  ; maybe_generate_lazy_expr : original_expr:expression -> Css_identifier.t -> expression
  ; assert_post_conditions : unit -> unit
  ; css_string_for_testing : string Lazy.t
  }

let create_sheet_hash hash =
  let hash_prefix = Ppxlib.gen_symbol () in
  {%string|%{hash_prefix}__%{hash}|}
;;

let generate_struct_to_hoist
  ~(sheet_name : Rule_id.t Identifier_helper.t)
  ~group_lazy_fn_name
  ~(css_strings : string Rule_id.Map.t)
  ~loc
  ~(group_to_rule_indices :
      (Rule_id.t, Rule_id.comparator_witness) Nonempty_set.t Group_type.Map.t)
  =
  let open (val Ast_builder.make loc) in
  let create_sheet_items =
    (* NOTE: We _always_ create all stylesheets up-front. This preserves
       the order of stylesheets independent of the order in which they are forced. *)
    Map.mapi css_strings ~f:(fun ~key:index ~data:_ ->
      [%stri
        let [%p Identifier_helper.pattern sheet_name ~index] =
          let sheet = Inline_css.Private.create_stylesheet () in
          Inline_css.Private.append_stylesheet sheet;
          sheet
        ;;])
  in
  let create_update_sheet_item index =
    let css_string = Map.find_exn css_strings index in
    let css_string = pexp_constant (Pconst_string (css_string, loc, Some "")) in
    [%expr
      Inline_css.Private.update_stylesheet
        [%e Identifier_helper.expression sheet_name ~index]
        [%e css_string]]
  in
  let create_group_update_expression (indices : Rule_id.t Nonempty_list.t) =
    let (first :: tl) = indices in
    let first_expr = create_update_sheet_item first in
    List.fold tl ~init:first_expr ~f:(fun acc index ->
      let update_expr = create_update_sheet_item index in
      pexp_sequence acc update_expr)
  in
  let update_functions_for_groups =
    Map.fold_right
      group_to_rule_indices
      ~init:[]
      ~f:(fun ~key:group_index ~data:rule_indices acc ->
        let rule_indices = Nonempty_set.to_nonempty_list rule_indices in
        match group_index, rule_indices with
        | Group_type.Autoforced, rule_indices ->
          let group_update_expression = create_group_update_expression rule_indices in
          (* Autoforced group does not generate a function and instead just runs the
             updates *)
          [%stri let () = [%e group_update_expression]] :: acc
        (* Do not check if the [rule_indices] are empty, [create_group_update_expression]
           will do that
        *)
        | Group _, rule_indices ->
          let group_update_expression = create_group_update_expression rule_indices in
          [%stri
            let [%p Identifier_helper.pattern group_lazy_fn_name ~index:group_index] =
              lazy [%e group_update_expression]
            ;;]
          :: acc)
  in
  List.concat [ Map.data create_sheet_items; update_functions_for_groups ]
;;

let create_hoisted_module_struct
  ~(stylesheet :
      Rule.t Traverse_css.Original_and_post_processed.t Rule_id.Map.t * location)
  ~hash
  ~lazy_loading_optimization
  ~should_hash_identifier
  ~loc
  ~expansion_kind:(_ : Expansion_kind.t)
  =
  let open (val Ast_builder.make loc) in
  let original_stylesheet =
    Tuple2.map_fst stylesheet ~f:(fun rules ->
      Map.map rules ~f:(fun { original; post_processed = _ } -> original))
  in
  let hashed_stylesheet =
    Tuple2.map_fst stylesheet ~f:(fun rules ->
      Map.map rules ~f:(fun { original = _; post_processed } -> post_processed))
  in
  let sheet_hash = create_sheet_hash hash in
  let { Traverse_css.Graph.group_to_rule_indices; get_group_for_identifier; _ } =
    Traverse_css.Graph.map_stylesheet_for_graph
      ~lazy_loading_optimization
      ~should_hash_identifier
      original_stylesheet
  and css_strings = stylesheet_to_rule_strings hashed_stylesheet
  and sheet_name =
    Identifier_helper.create
      ~prefix:"sheet"
      ~to_string:Rule_id.to_string
      ~hash:sheet_hash
      ~loc
  and group_lazy_fn_name =
    Identifier_helper.create
      ~to_string:Group_type.to_string
      ~prefix:"update_sheet_lazy_fn"
      ~hash:sheet_hash
      ~loc
  in
  let assert_post_conditions () =
    Identifier_helper.assert_expression_and_pattern_symmetry sheet_name;
    Identifier_helper.assert_expression_and_pattern_symmetry group_lazy_fn_name
  in
  let css_string_for_testing = lazy (Map.data css_strings |> String.concat_lines) in
  let struct_to_hoist =
    generate_struct_to_hoist
      ~sheet_name
      ~group_lazy_fn_name
      ~css_strings
      ~loc
      ~group_to_rule_indices
  in
  let maybe_generate_lazy_expr ~original_expr identifier =
    let hoister_accessor_exp attribute_name =
      pexp_ident
        (Located.mk (Ldot (Lident "Ppx_css_hoister_do_not_collide", attribute_name)))
    in
    match get_group_for_identifier identifier with
    | Group_type.Autoforced -> original_expr
    | Group_type.Group _ as group_identifier ->
      let fn_name_exp =
        hoister_accessor_exp
          (Identifier_helper.string group_lazy_fn_name ~index:group_identifier Expression)
      in
      [%expr
        Virtual_dom.Vdom.Attr.lazy_
          (lazy
            (Inline_css.Ppx_css_runtime.force [%e fn_name_exp];
             [%e original_expr]))]
  in
  { struct_to_hoist
  ; maybe_generate_lazy_expr
  ; assert_post_conditions
  ; css_string_for_testing
  }
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
  ~(identifiers : (Css_identifier.Set.t * expression) String.Map.t)
  ~(expansion_kind : Expansion_kind.t)
  ~(anonymous_variables : Anonymous_variable.Collection.t)
  ~inferred_do_not_hash
  ~maybe_generate_lazy_expr
  : module_expr
  =
  let open (val Ast_builder.make loc) in
  let identifiers_list_without_anonymous_variables =
    let anonymous_variable_names = anonymous_variable_names_as_set anonymous_variables in
    Map.filter identifiers ~f:(fun (identifiers_set, _) ->
      Set.for_all identifiers_set ~f:(is_not_anonymous_variable ~anonymous_variable_names))
    |> Map.to_alist
  in
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
            let anonymous_variable_name =
              Anonymous_variable.name anonymous_variable
              |> Anonymous_variable.Name.to_string
            in
            let%bind.Option identifiers_for_ocaml_identifier, expression =
              Map.find identifiers anonymous_variable_name
            in
            let has_variable =
              Set.exists identifiers_for_ocaml_identifier ~f:(function
                | Css_identifier.Variable _ -> true
                | _ -> false)
            in
            match has_variable with
            | true -> Some expression
            | false -> None
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
                ( :: )
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
                 value_binding
                   ~pat
                   ~expr:(Anonymous_variable.expression anonymous_variable))
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
      let variables =
        List.filter_map
          identifiers_list_without_anonymous_variables
          ~f:(fun (ocaml_identifier, (identifiers, e)) ->
            let is_variable =
              Set.exists identifiers ~f:(function
                | Css_identifier.Variable _ -> true
                | _ -> false)
            in
            match is_variable with
            | true -> Some (ocaml_identifier, e)
            | false -> None)
      in
      var_builder_structure
        ~loc
        ~variables
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
        Merlin_helpers.focus_expression
          [%expr
            Virtual_dom.Vdom.Attr.combine
              [%e expression]
              [%e preconstructed_attr_expression]]
    in
    let inferred_do_not_hash_original =
      Set.to_list inferred_do_not_hash
      |> List.map ~f:Helper_utils.css_identifier_to_ocaml_identifier
      |> String.Set.of_list
    in
    identifiers_list_without_anonymous_variables
    |> List.filter ~f:(fun (ocaml_identifier, _) ->
      not (Set.mem inferred_do_not_hash_original ocaml_identifier))
    |> List.concat_map ~f:(fun (ocaml_identifier, (identifiers, e)) ->
      let class_identifier =
        Set.find identifiers ~f:(function
          | Css_identifier.Class _ -> true
          | _ -> false)
      in
      let id_identifier =
        Set.find identifiers ~f:(function
          | Css_identifier.Id _ -> true
          | _ -> false)
      in
      match `Class class_identifier, `Id id_identifier with
      | `Class (Some class_identifier), `Id (Some id_identifier) ->
        let id_string = ocaml_identifier ^ "_id" in
        let class_string = ocaml_identifier ^ "_class" in
        let id_pattern = ppat_var (Located.mk id_string) in
        let original_pattern = ppat_var (Located.mk ocaml_identifier) in
        let class_pattern = ppat_var (Located.mk class_string) in
        let attr_struct =
          [ [%stri let [%p original_pattern] = Virtual_dom.Vdom.Attr.empty]
          ; [%stri
              let [%p class_pattern] =
                [%e
                  maybe_generate_lazy_expr
                    ~original_expr:
                      (maybe_set_anon_variables
                         [%expr Virtual_dom.Vdom.Attr.class_ [%e e]])
                    class_identifier]
              ;;]
          ; [%stri
              let [%p id_pattern] =
                [%e
                  maybe_generate_lazy_expr
                    ~original_expr:[%expr Virtual_dom.Vdom.Attr.id [%e e]]
                    id_identifier]
              ;;]
          ]
        in
        attr_struct
      | `Class (Some class_identifier), _ ->
        [ [%stri
            let [%p ppat_var (Located.mk ocaml_identifier)] =
              [%e
                maybe_generate_lazy_expr
                  ~original_expr:
                    (maybe_set_anon_variables [%expr Virtual_dom.Vdom.Attr.class_ [%e e]])
                  class_identifier]
            ;;]
        ]
      | _, `Id (Some id_identifier) ->
        [ [%stri
            let [%p ppat_var (Located.mk ocaml_identifier)] =
              [%e
                maybe_generate_lazy_expr
                  ~original_expr:
                    (maybe_set_anon_variables [%expr Virtual_dom.Vdom.Attr.id [%e e]])
                  id_identifier]
            ;;]
        ]
      | `Class None, `Id None -> [])
  in
  let identifiers_and_variables_as_string =
    List.map
      identifiers_list_without_anonymous_variables
      ~f:(fun (ocaml_identifier, (_, expression)) -> ocaml_identifier, expression)
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

(** Extracting this part out so that we can use the same function in tests *)
let create_should_hash_identifier ~dont_hash ~dont_hash_prefixes ~always_hash =
  let is_matched_by_a_prefix =
    let dont_hash_prefixes =
      (* Sorted from most general to least general (i.e. shorter prefix to longest
           prefix)*)
      List.sort
        ~compare:(fun a_1 b_1 ->
          Comparable.lexicographic
            [ (fun a b -> Comparable.lift Int.ascending ~f:String.length a b)
            ; String.compare
            ]
            a_1
            b_1)
        dont_hash_prefixes
    in
    fun identifier ->
      List.find dont_hash_prefixes ~f:(fun prefix -> String.is_prefix identifier ~prefix)
  in
  Staged.stage (fun identifier ->
    let css_identifier = Css_identifier.extract_css_identifier identifier in
    match
      ( `Always_hash (Set.mem always_hash css_identifier)
      , `Dont_hash (Set.mem dont_hash css_identifier) )
    with
    | `Always_hash true, _ -> `Hash
    | `Always_hash false, `Dont_hash false ->
      (match is_matched_by_a_prefix css_identifier with
       | None -> `Hash
       | Some prefix -> `Dont_hash_prefixes prefix)
    | `Always_hash false, `Dont_hash true -> `Dont_hash)
;;

let generate_struct_from_css_string_and_options
  ~loc
  ~pos_for_hashing
  ~lazy_loading_optimization
  ~dont_hash
  ~original_css_string
  ~css_string
  ~dont_hash_prefixes
  ~stylesheet_location
  ~(expansion_kind : Expansion_kind.t)
  ~inferred_do_not_hash
  ~always_hash
  ~anonymous_variables
  ~disable_hashing
  : structure With_hoisted_expression.t
  =
  let open (val Ast_builder.make loc) in
  let stable_stylesheet =
    let stylesheet =
      let stylesheet =
        Css_parser.parse_stylesheet
          ~parsing_config:Css_parser.Parsing_config.raise_on_recoverable_errors
          ~pos:stylesheet_location.loc_start
          css_string
      in
      match lazy_loading_optimization with
      | Preprocess_arguments.Lazy_graph -> Traverse_css.split_layers stylesheet
      | Default | Eager -> stylesheet
    in
    Tuple2.map_fst stylesheet ~f:Rule_id.identify_stylesheet
  in
  let should_hash_identifier =
    create_should_hash_identifier ~dont_hash ~dont_hash_prefixes ~always_hash
    |> Staged.unstage
  in
  let () =
    Traverse_css.raise_if_unused_dont_hash_or_prefixes_or_collisions
      ~loc
      ~unused_dont_hash:dont_hash
      ~unused_dont_hash_prefixes:(String.Set.of_list dont_hash_prefixes)
      ~unused_allow_set:inferred_do_not_hash
      ~should_hash_identifier
      stable_stylesheet
  in
  let { Traverse_css.Transform.stylesheet; identifier_mapping; hash } =
    Traverse_css.Transform.f
      ~pos_for_hashing
      ~original_css_string
      ~should_hash_identifier
      ~disable_hashing
      stable_stylesheet
  in
  let { struct_to_hoist
      ; maybe_generate_lazy_expr
      ; assert_post_conditions
      ; css_string_for_testing
      }
    =
    create_hoisted_module_struct
      ~lazy_loading_optimization
      ~loc
      ~hash
      ~should_hash_identifier
      ~expansion_kind
      ~stylesheet
  in
  let identifiers_set = Hashtbl.keys identifier_mapping |> Css_identifier.Set.of_list in
  validate_no_collisions_after_warnings_and_rewrites ~loc ~identifiers:identifiers_set;
  let identifiers_by_ocaml_identifier =
    Hashtbl.to_alist identifier_mapping
    |> List.fold ~init:String.Map.empty ~f:(fun acc (identifier, hashed_identifier) ->
      let ocaml_identifier = Css_identifier.extract_ocaml_identifier identifier in
      Map.update acc ocaml_identifier ~f:(function
        | Some (existing_identifiers, hashed_identifier) ->
          Set.add existing_identifiers identifier, hashed_identifier
        | None -> Css_identifier.Set.singleton identifier, hashed_identifier))
  in
  let t_module =
    create_default_module_struct
      ~loc
      ~identifiers:identifiers_by_ocaml_identifier
      ~expansion_kind
      ~anonymous_variables
      ~inferred_do_not_hash
      ~maybe_generate_lazy_expr
  in
  assert_post_conditions ();
  let structure =
    match expansion_kind with
    | Styled_component _ -> [ [%stri include [%m t_module]] ]
    | Stylesheet ->
      (* We want to remove the variables that were introduced during the interpolation. *)
      let identifiers =
        identifiers_set
        |> filter_anonymous_variables_from_identifiers ~anonymous_variables
      in
      let t_sig = module_type_of_identifiers ~loc ~identifiers |> pmty_signature in
      [ disable_warning_32 ~loc
      ; create_type_info_function ~loc ~stylesheet_location
      ; [%stri module type S = [%m t_sig]]
      ; [%stri type t = (module S)]
      ; [%stri module Default : S = [%m t_module]]
      ; [%stri include Default]
      ; [%stri let default : t = (module Default)]
      ]
  in
  { With_hoisted_expression.txt = structure
  ; hoisted_structure_items = struct_to_hoist
  ; css_string_for_testing
  }
;;

let generate_struct ~loc ~disable_hashing (expr : expression) =
  let pos_for_hashing = loc.loc_start in
  let loc = { loc with loc_ghost = true } in
  let expr = loc_ghoster#expression expr in
  let { Ppx_css_syntax.dont_hash
      ; css_string
      ; dont_hash_prefixes
      ; lazy_loading_optimization
      }
    =
    Ppx_css_syntax.parse_stylesheet_exn expr
  in
  let anonymous_declarations = Anonymous_declarations.For_stylesheet.create css_string in
  generate_struct_from_css_string_and_options
    ~expansion_kind:Stylesheet
    ~loc
    ~pos_for_hashing
    ~lazy_loading_optimization
    ~dont_hash
    ~original_css_string:css_string.css_string
    ~css_string:
      (Anonymous_declarations.For_stylesheet.to_stylesheet_string anonymous_declarations)
    ~dont_hash_prefixes
    ~stylesheet_location:css_string.string_loc
    ~inferred_do_not_hash:String.Set.empty
    ~always_hash:
      (Anonymous_declarations.For_stylesheet.always_hash anonymous_declarations)
    ~anonymous_variables:
      (Anonymous_declarations.For_stylesheet.anonymous_variables anonymous_declarations)
    ~disable_hashing
;;

let generate_expression_from_css_declarations_and_options
  ~loc
  ~pos_for_hashing
  ~dont_hash
  ~lazy_loading_optimization
  ~anonymous_declarations
  ~dont_hash_prefixes
  ~original_css_string
  ~stylesheet_location
  ~disable_hashing
  : expression With_hoisted_expression.t
  =
  let open (val Ast_builder.make loc) in
  let inferred_do_not_hash =
    Anonymous_declarations.inferred_do_not_hash anonymous_declarations
  in
  let dont_hash = Set.union dont_hash (String.Set.of_list inferred_do_not_hash) in
  let { With_hoisted_expression.txt = module_
      ; hoisted_structure_items
      ; css_string_for_testing
      }
    =
    let inferred_do_not_hash = String.Set.of_list inferred_do_not_hash in
    generate_struct_from_css_string_and_options
      ~expansion_kind:(Styled_component anonymous_declarations)
      ~original_css_string
      ~pos_for_hashing
      ~loc
      ~lazy_loading_optimization
      ~css_string:(Anonymous_declarations.to_stylesheet_string anonymous_declarations)
      ~dont_hash
      ~dont_hash_prefixes
      ~stylesheet_location
      ~inferred_do_not_hash
      ~always_hash:(Anonymous_declarations.always_hash anonymous_declarations)
      ~anonymous_variables:
        (Anonymous_declarations.anonymous_variables anonymous_declarations)
      ~disable_hashing
  in
  let style_module_name = gen_symbol ~prefix:"Ppx_css_anonymous_style" () in
  let body =
    pexp_ident
      (Located.mk
         (Ldot (Lident style_module_name, Anonymous_declarations.anonymous_class_name loc)))
  in
  let structure = pmod_structure module_ in
  { With_hoisted_expression.txt =
      pexp_letmodule
        (Located.mk (Some style_module_name))
        { structure with pmod_attributes = structure.pmod_attributes }
        body
  ; hoisted_structure_items
  ; css_string_for_testing
  }
;;

let generate_inline_expression ~loc (expr : expression) ~disable_hashing
  : expression With_hoisted_expression.t
  =
  let pos_for_hashing = loc.loc_start in
  let open (val Ast_builder.make loc) in
  let loc = { loc with loc_ghost = true } in
  let expr = loc_ghoster#expression expr in
  let { Ppx_css_syntax.dont_hash
      ; css_string
      ; dont_hash_prefixes
      ; lazy_loading_optimization
      }
    =
    Ppx_css_syntax.parse_inline_expression_exn expr
  in
  let anonymous_declarations = Anonymous_declarations.create css_string in
  generate_expression_from_css_declarations_and_options
    ~loc
    ~pos_for_hashing
    ~lazy_loading_optimization
    ~dont_hash
    ~anonymous_declarations
    ~dont_hash_prefixes
    ~original_css_string:css_string.css_string
    ~stylesheet_location:css_string.string_loc
    ~disable_hashing
;;

let create_sig_from_idents ~loc ~identifiers =
  let open (val Ast_builder.make loc) in
  validate_no_collisions_after_warnings_and_rewrites ~loc ~identifiers;
  let basic_sig = module_type_of_identifiers ~loc ~identifiers in
  pmty_signature
    ([ [%sigi: module type S = [%m pmty_signature basic_sig]]
     ; [%sigi: type t = (module S)]
     ; [%sigi: val default : t]
     ]
     @ basic_sig)
;;

module For_css_inliner = struct
  type result =
    { ml_file : string
    ; css_string_for_testing : string Lazy.t
    }

  let gen_struct
    ~dont_hash
    ~css_string
    ~dont_hash_prefixes
    ~stylesheet_location
    ~lazy_loading_optimization
    ~disable_hashing
    =
    let buffer = Buffer.create 1024 in
    let loc = Location.none in
    let%tydi { txt = module_expr; hoisted_structure_items; css_string_for_testing } =
      generate_struct_from_css_string_and_options
        ~lazy_loading_optimization
        ~pos_for_hashing:stylesheet_location.loc_start
          (* Have to pass in the [loc_start] of the stylesheet as it's the only location we
           have *)
        ~expansion_kind:Stylesheet
        ~original_css_string:css_string
        ~loc
        ~dont_hash
        ~css_string
        ~dont_hash_prefixes
        ~stylesheet_location
        ~inferred_do_not_hash:String.Set.empty
        ~always_hash:String.Set.empty
        ~anonymous_variables:Anonymous_variable.Collection.empty
        ~disable_hashing
    in
    let () =
      List.iter hoisted_structure_items ~f:(fun structure_item ->
        Hoister.register ~structure_item)
    in
    let module_expr =
      let open (val Ast_builder.make loc) in
      pmod_structure (Hoister.create_hoisted_module ~loc :: module_expr)
    in
    Pprintast.module_expr (Format.formatter_of_buffer buffer) module_expr;
    let ml_file = Buffer.contents buffer in
    { ml_file; css_string_for_testing }
  ;;

  open Traverse_css

  let gen_sig ~stylesheet_location css =
    let buffer = Buffer.create 1024 in
    let stylesheet =
      Css_parser.parse_stylesheet
        ~parsing_config:Css_parser.Parsing_config.raise_on_recoverable_errors
        ~pos:stylesheet_location.loc_start
        css
    in
    let identifiers =
      get_all_identifiers (Tuple2.map_fst stylesheet ~f:Rule_id.identify_stylesheet)
    in
    let mli_as_an_ast =
      (* interpolation is currently not allowed in the css inliner so all variables are
         user variables. *)
      create_sig_from_idents ~loc:Location.none ~identifiers
    in
    Pprintast.module_type (Format.formatter_of_buffer buffer) mli_as_an_ast;
    Buffer.contents buffer
  ;;
end

let ml_extension_fn ~loc (expression : expression) =
  let%tydi { txt = module_; hoisted_structure_items; _ } =
    generate_struct ~loc expression ~disable_hashing:false
  in
  List.iter hoisted_structure_items ~f:(fun structure_item ->
    Hoister.register ~structure_item);
  let open (val Ast_builder.make loc) in
  pmod_structure module_
;;

let ml_extension =
  Extension.declare
    "css"
    Extension.Context.module_expr
    Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path:_ (expression : expression) -> ml_extension_fn ~loc expression)
;;

let inline_extension_fn ~loc (expression : expression) =
  let%tydi { txt = expression; hoisted_structure_items; _ } =
    generate_inline_expression ~loc expression ~disable_hashing:false
  in
  List.iter hoisted_structure_items ~f:(fun structure_item ->
    Hoister.register ~structure_item);
  expression
;;

let inline_extension =
  Extension.declare
    "css"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path:_ expression -> inline_extension_fn ~loc expression)
;;

let attempt_to_put_structure_item_after_the_ppx_module_timer_start
  ~structure_item
  ~structure
  =
  (* NOTE: Registering the CSS at the top of files means that the registry happens
     before the call to Ppx_module_timer_runtime.record_start. This makes us lose
     profiling information on CSS registration, which unfortunately, has also
     historically been one of the slowest things that happens on startup (7/8th's worth
     of time for some apps). This function attempts to put the ppx_css registry after the
     timer starts so that the work done by ppx_css is timed.
  *)
  match structure with
  | ([%stri
       let () = Ppx_module_timer_runtime.record_start Ppx_module_timer_runtime.__MODULE__]
     as first)
    :: remaining ->
    first :: structure_item :: remaining
    (* Conservatively match on ppx_module_timer_runtime being the first item. If it
       is, we'll place our hoister after it so that we can include the hoister in
       the module timer *)
  | _ ->
    (* Default to putting the hoisted module at the very front of the structure *)
    structure_item :: structure
;;

(** We want [ppx_css] to run after [ppx_html] has transformed style nodes into [ppx_css].
    We then want the hoisted module to be inserted at the top of the file so that the
    modules are registered and available to the rest of the file. *)
let register_hoisted_css =
  Driver.Instrument.make ~position:Driver.Instrument.After (fun structure ->
    match Hoister.is_empty (), structure with
    | false, [] ->
      Location.raise_errorf
        ~loc:{ loc_start = [%here]; loc_end = [%here]; loc_ghost = true }
        "BUG in ppx_css, somehow generated side effects from empty file"
    | true, _ -> structure
    | false, first :: _ ->
      let hoister_structure_item =
        Hoister.create_hoisted_module ~loc:{ first.pstr_loc with loc_ghost = true }
      in
      attempt_to_put_structure_item_after_the_ppx_module_timer_start
        ~structure_item:hoister_structure_item
        ~structure)
;;

let () =
  Driver.register_transformation
    "css"
    ~extensions:[ inline_extension; ml_extension ]
    ~instrument:register_hoisted_css
;;

module For_testing = struct
  let generate_css_stylesheet_string ~loc ~disable_hashing expression =
    let%tydi { css_string_for_testing; txt = _; hoisted_structure_items = _ } =
      generate_struct ~loc expression ~disable_hashing
    in
    force css_string_for_testing
  ;;

  let generate_css_inline_string ~loc ~disable_hashing expression =
    let%tydi { css_string_for_testing; txt = _; hoisted_structure_items = _ } =
      generate_inline_expression ~loc expression ~disable_hashing
    in
    force css_string_for_testing
  ;;

  let generate_struct ~loc ~disable_hashing expression =
    generate_struct ~loc ~disable_hashing expression
  ;;

  let generate_inline_expression ~loc ~disable_hashing expression =
    generate_inline_expression ~disable_hashing expression ~loc
  ;;

  let map_style_sheet = Traverse_css.For_testing.map_style_sheet

  let reset_anonymous_variable_identifiers =
    Anonymous_variable.For_testing.restart_identifiers
  ;;

  let create_should_hash_identifier = create_should_hash_identifier

  module Traverse_css = Traverse_css

  let ppx_css_expression_to_structure_item = Hoister.ppx_css_expression_to_structure_item
end
