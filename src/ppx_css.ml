open! Core
open! Ppxlib

let disable_warning_32 ~loc =
  let open (val Ast_builder.make loc) in
  attribute
    ~name:(Located.mk "ocaml.warning")
    ~payload:(PStr [ pstr_eval (estring "-32") [] ])
;;

let module_type_of_identifiers ~loc ~identifiers =
  let open (val Ast_builder.make loc) in
  identifiers
  (* Sort because the ml and mli might have their identifiers in
     different orders, but we still want the [module type S] to present
     them in the same order. *)
  |> List.dedup_and_sort ~compare:String.compare
  |> List.map ~f:(fun ident ->
    let type_ = [%type: string] in
    let name = Located.mk ident in
    psig_value (value_description ~name ~type_ ~prim:[]))
;;

let generate_struct ~loc ~path:_ (expr : expression) =
  let loc = { loc with loc_ghost = true } in
  let open (val Ast_builder.make loc) in
  (* The [Some ""] means that the string will use the multiline string literal
     syntax, but with no termination identifier. *)
  let string_constant l = pexp_constant (Pconst_string (l, loc, Some "")) in
  let css_string, { Options.dont_hash = dont_hash_these } = Options.parse expr ~loc in
  let { Traverse_css.css_string; mapping } =
    Traverse_css.transform ~pos:loc.loc_start css_string ~dont_hash_these
  in
  let mapping = Hashtbl.to_alist mapping in
  let register =
    [%stri let () = Inline_css.Private.append [%e string_constant css_string]]
  in
  let t_sig =
    module_type_of_identifiers ~loc ~identifiers:(List.map mapping ~f:fst)
    |> pmty_signature
  in
  let t_module =
    mapping
    |> List.map ~f:(fun (k, v) ->
      [%stri let [%p ppat_var (Located.mk k)] = [%e string_constant v]])
    |> pmod_structure
  in
  pmod_structure
    [ pstr_attribute (disable_warning_32 ~loc)
    ; register
    ; [%stri module type S = [%m t_sig]]
    ; [%stri type t = (module S)]
    ; [%stri module Default = [%m t_module]]
    ; [%stri include Default]
    ; [%stri let default : t = (module Default)]
    ]
;;

let generate_sig ~loc ~path:_ payload =
  let loc = { loc with loc_ghost = true } in
  let open (val Ast_builder.make loc) in
  match payload with
  | PTyp (type_ : core_type) ->
    let all_idents =
      object
        inherit [string list] Ast_traverse.fold as super

        method! core_type e acc =
          let acc = super#core_type e acc in
          match e.ptyp_desc with
          | Ptyp_constr ({ txt = Lident identifier; _ }, _) -> identifier :: acc
          | _ -> acc
      end
    in
    let identifiers = all_idents#core_type type_ [] in
    let basic_sig = module_type_of_identifiers ~loc ~identifiers in
    pmty_signature
      ([ [%sigi: module type S = [%m pmty_signature basic_sig]]
       ; [%sigi: type t = (module S)]
       ; [%sigi: val default : t]
       ]
       @ basic_sig)
  | PStr _ | PSig _ | PPat _ ->
    Location.raise_errorf
      ~loc
      "you must pass %%css a space-separated sequence of identifiers which were used in \
       the css string"
;;

let ml_extension =
  Extension.declare
    "css.raw"
    Extension.Context.module_expr
    Ast_pattern.(single_expr_payload __)
    generate_struct
;;

let mli_extension =
  Extension.declare "css.raw" Extension.Context.module_type Ast_pattern.(__) generate_sig
;;

let () = Driver.register_transformation "css" ~extensions:[ ml_extension; mli_extension ]

module For_testing = struct
  let generate_struct = generate_struct ~loc:Location.none ~path:()
  let generate_sig typ = generate_sig ~loc:Location.none ~path:() (PTyp typ)
end
