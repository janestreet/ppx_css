open! Core
open Ppxlib

(** This module contains the mutable state required to do things like:

    {[ let f () = [%css {|background-color: tomato|}] ]}

    to instead of registering the css string within [f]: 

    {[ 
      let f () =
        let () = Inline_css.Private.append {|...|} in
        ()
    ]}

    it lets us instead put the registrations at the bottom of the file:


    {[ 
      let f () =
        ...
        ()

      (* bottom of file... *)
      let () = Inline_css.Private.append {|...|}
    ]}
*)

val register : structure_item:structure_item -> unit
val create_hoisted_module : loc:location -> structure_item
val ppx_css_expression_to_structure_item : loc:location -> expression -> structure_item
val is_empty : unit -> bool
