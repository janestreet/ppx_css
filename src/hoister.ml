open! Core
open Ppxlib

let registry : structure_item Reversed_list.t ref = ref Reversed_list.[]
let register ~structure_item = registry := structure_item :: !registry

let ppx_css_expression_to_structure_item ~loc expression =
  [%stri let () = Inline_css.Private.append [%e expression]]
;;

let create_hoisted_module ~loc =
  let open (val Ast_builder.make loc) in
  let modules = Reversed_list.rev !registry |> pmod_structure in
  let module_ =
    pstr_module
      (module_binding
         ~name:(Located.mk (Some "Ppx_css_hoister_do_not_collide"))
         ~expr:modules)
  in
  [%stri
    open struct
      (* This warning is at the `open struct` level, so it should not affect the warnings
     thrown by the values within the module itself. We've also added error throwing that's 
     a bit more specific within the lazy sheets code, so this should be safe to do *)
      [@@@ocaml.warning "-60"]

      [%%i module_]
    end]
;;

let is_empty () = Reversed_list.is_empty !registry
