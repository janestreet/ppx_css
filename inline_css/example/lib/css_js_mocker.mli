open! Core

(* This function exists so that this file is not dead-code eliminated. The side-effect in
   this file need to run before the ppx_css hoister functions run. Without them, this test
   does not function properly *)
val get_id : (unit -> unit) lazy_t
