open! Core
open! Virtual_dom

(* This file is solely an end-to-end test for ppx_css expanding inline expressions. *)

let height = `Rem 1.0

let _ : Vdom.Attr.t =
  [%css
    {|
      background-color: tomato;
      height: %{height#Css_gen.Length};
      width: %{"2px"};

      .i-should-not-be-hashed {
        background-color: rebaccapurple;
      }
    |}]
;;
