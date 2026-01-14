open! Core
open! Inline_css

(* Because the top-level side effects require the module to be linkable, we introduce this
   call to [Callback.register] which forces the lib to be linked.

   In a real application, you wouldn't need to do this, because you'd be using the
   components in your app. *)
let () =
  (Callback.register [@ocaml.alert "-unsafe_multidomain"])
    "keeping this value alive"
    Inline_css_example_lib.app
;;

let to_string () =
  Js_of_ocaml.Js.Unsafe.js_expr
    {js|
     (function () {
       return [...document.adoptedStyleSheets].map((sheet) => sheet.text).join("\n");
     }()
     )
  |js}
  |> Js_of_ocaml.Js.to_string
;;

let () =
  let regex = Re.Str.regexp "_hash_\\([a-z0-9]+\\)*" in
  to_string () |> Re.Str.global_replace regex "_hash_replaced_in_test" |> print_endline
;;
