open! Core
open! Inline_css

(* Because the top-level side effects require the module 
   to be linkable, we introduce this call to [Callback.register]
   which forces the lib to be linked.  

   In a real application, you wouldn't need to do this, because you'd
   be using the components in your app. *)
let () = Callback.register "keeping this value alive" Inline_css_example_lib.app

let () =
  let regex = Re.Str.regexp "_hash_\\([a-z0-9]+\\)*" in
  Inline_css.For_testing.to_string ()
  |> Re.Str.global_replace regex "_hash_replaced_in_test"
  |> print_endline
;;
