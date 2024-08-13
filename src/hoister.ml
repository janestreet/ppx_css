open! Core
open Ppxlib

let registry : expression Reversed_list.t ref = ref Reversed_list.[]

let register ~ppx_css_string_expression =
  registry := ppx_css_string_expression :: !registry
;;

let ppx_css_expression_to_structure_item ~loc expression =
  [%stri let () = Inline_css.Private.append_but_do_not_update [%e expression]]
;;

let create_hoisted_module ~loc =
  let open (val Ast_builder.make loc) in
  let modules =
    Reversed_list.rev !registry
    |> List.map ~f:(fun ppx_css_string_expression ->
      [%stri
        let () =
          Inline_css.Private.append_but_do_not_update [%e ppx_css_string_expression]
        ;;])
    |> pmod_structure
  in
  pstr_module (module_binding ~name:(Located.mk None) ~expr:modules)
;;

let is_empty () = Reversed_list.is_empty !registry
