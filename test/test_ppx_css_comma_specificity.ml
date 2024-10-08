open! Core
open Ppxlib

let loc = Location.none

(* This is a test case showing that PPX_CSS behaves correctly when there are many
   selectors attached to the same block of properties and that ppx_css hands off any
   specificity battles to the browser however the user writes the battles. Importantly
   ppx_css doesn't do any merging/re-organizing of seletors that might affect specificity.
*)

let%expect_test "Duplicated version" =
  Test_util.test_struct
    [%expr
      stylesheet
        {|
      .a:hover::after {
        content: "";
        background: tomato;
        position: absolute;
        border: 3px solid grey;
        border-radius: 10px;
        pointer-events: none;
        top: 3%;
        left: 3%;
        width: 94%;
        height: 44%;
      }
      .b:hover::after {
        content: "";
        background: tomato;
        position: absolute;
        border: 3px solid grey;
        border-radius: 10px;
        pointer-events: none;
        top: 53%;
        left: 3%;
        width: 94%;
        height: 44%;
      }
      .c:hover::after {
        content: "";
        background: tomato;
        position: absolute;
        border: 3px solid grey;
        border-radius: 10px;
        pointer-events: none;
        top: 3%;
        left: 3%;
        width: 44%;
        height: 94%;
      }
      .d:hover::after {
        content: "";
        background: tomato;
        position: absolute;
        border: 3px solid grey;
        border-radius: 10px;
        pointer-events: none;
        top: 3%;
        left: 53%;
        width: 44%;
        height: 94%;
      }
  |}];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let __type_info_for_ppx_css :
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
    module type S  =
      sig
        module For_referencing :
        sig val a : string val b : string val c : string val d : string end
        val a : Virtual_dom.Vdom.Attr.t
        val b : Virtual_dom.Vdom.Attr.t
        val c : Virtual_dom.Vdom.Attr.t
        val d : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing =
          struct
            let d = {|d_hash_044f510dea|}
            let c = {|c_hash_044f510dea|}
            let b = {|b_hash_044f510dea|}
            let a = {|a_hash_044f510dea|}
          end
        let d = Virtual_dom.Vdom.Attr.class_ {|d_hash_044f510dea|}
        let c = Virtual_dom.Vdom.Attr.class_ {|c_hash_044f510dea|}
        let b = Virtual_dom.Vdom.Attr.class_ {|b_hash_044f510dea|}
        let a = Virtual_dom.Vdom.Attr.class_ {|a_hash_044f510dea|}
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.a_hash_044f510dea:hover::after {
     content:"";
     background:tomato;
     position:absolute;
     border:3px solid grey;
     border-radius:10px;
     pointer-events:none;
     top:3%;
     left:3%;
     width:94%;
     height:44%
    }

    *.b_hash_044f510dea:hover::after {
     content:"";
     background:tomato;
     position:absolute;
     border:3px solid grey;
     border-radius:10px;
     pointer-events:none;
     top:53%;
     left:3%;
     width:94%;
     height:44%
    }

    *.c_hash_044f510dea:hover::after {
     content:"";
     background:tomato;
     position:absolute;
     border:3px solid grey;
     border-radius:10px;
     pointer-events:none;
     top:3%;
     left:3%;
     width:44%;
     height:94%
    }

    *.d_hash_044f510dea:hover::after {
     content:"";
     background:tomato;
     position:absolute;
     border:3px solid grey;
     border-radius:10px;
     pointer-events:none;
     top:3%;
     left:53%;
     width:44%;
     height:94%
    }|}
    |xxx}]
;;

let%expect_test "Merged version - with a specificity battle later" =
  (* NOTE: This is OK and expected, it will follow whatever specificity the CSS engine
     has the order + selectors are preserved. *)
  Test_util.test_struct
    [%expr
      stylesheet
        {|
      .a:hover::after,
      .b:hover::after,
      .c:hover::after,
      .d:hover::after {
        content: "";
        background: tomato;
        position: absolute;
        border: 3px solid grey;
        border-radius: 10px;
        pointer-events: none;
        top: 3%;
        left: 53%;
        width: 44%;
        height: 94%;
      }

      .a:hover::after {
        top: 100%;
      }
  |}];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let __type_info_for_ppx_css :
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
    module type S  =
      sig
        module For_referencing :
        sig val a : string val b : string val c : string val d : string end
        val a : Virtual_dom.Vdom.Attr.t
        val b : Virtual_dom.Vdom.Attr.t
        val c : Virtual_dom.Vdom.Attr.t
        val d : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing =
          struct
            let d = {|d_hash_677c1e8bce|}
            let c = {|c_hash_677c1e8bce|}
            let b = {|b_hash_677c1e8bce|}
            let a = {|a_hash_677c1e8bce|}
          end
        let d = Virtual_dom.Vdom.Attr.class_ {|d_hash_677c1e8bce|}
        let c = Virtual_dom.Vdom.Attr.class_ {|c_hash_677c1e8bce|}
        let b = Virtual_dom.Vdom.Attr.class_ {|b_hash_677c1e8bce|}
        let a = Virtual_dom.Vdom.Attr.class_ {|a_hash_677c1e8bce|}
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.a_hash_677c1e8bce:hover::after,*.b_hash_677c1e8bce:hover::after,*.c_hash_677c1e8bce:hover::after,*.d_hash_677c1e8bce:hover::after {
     content:"";
     background:tomato;
     position:absolute;
     border:3px solid grey;
     border-radius:10px;
     pointer-events:none;
     top:3%;
     left:53%;
     width:44%;
     height:94%
    }

    *.a_hash_677c1e8bce:hover::after {
     top:100%
    }|}
    |xxx}]
;;
