open! Core
open Ppxlib

let loc = Test_util.loc_with_mock_name
let () = Ppx_css_syntax.Preprocess_arguments.set_lazy_loading_optimization (Some true)

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
            let a = {|a_hash_1b742bd012|}
            let b = {|b_hash_1b742bd012|}
            let c = {|c_hash_1b742bd012|}
            let d = {|d_hash_1b742bd012|}
          end
        let a =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__001___1b742bd012__group_2;
                Virtual_dom.Vdom.Attr.class_ {|a_hash_1b742bd012|}))
        let b =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__001___1b742bd012__group_3;
                Virtual_dom.Vdom.Attr.class_ {|b_hash_1b742bd012|}))
        let c =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__001___1b742bd012__group_0;
                Virtual_dom.Vdom.Attr.class_ {|c_hash_1b742bd012|}))
        let d =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__001___1b742bd012__group_1;
                Virtual_dom.Vdom.Attr.class_ {|d_hash_1b742bd012|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__001___1b742bd012__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__001___1b742bd012__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__001___1b742bd012__2 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__001___1b742bd012__3 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__001___1b742bd012__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__001___1b742bd012__2
           {|
    /* app/foo/foo.ml */

    .c_hash_1b742bd012:hover::after {
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
    }|})
    let update_sheet_lazy_fn_x__001___1b742bd012__group_1 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__001___1b742bd012__3
           {|
    /* app/foo/foo.ml */

    .d_hash_1b742bd012:hover::after {
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
    }|})
    let update_sheet_lazy_fn_x__001___1b742bd012__group_2 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__001___1b742bd012__0
           {|
    /* app/foo/foo.ml */

    .a_hash_1b742bd012:hover::after {
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
    }|})
    let update_sheet_lazy_fn_x__001___1b742bd012__group_3 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__001___1b742bd012__1
           {|
    /* app/foo/foo.ml */

    .b_hash_1b742bd012:hover::after {
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
    }|})
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
            let a = {|a_hash_2cacf009b1|}
            let b = {|b_hash_2cacf009b1|}
            let c = {|c_hash_2cacf009b1|}
            let d = {|d_hash_2cacf009b1|}
          end
        let a =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__002___2cacf009b1__group_0;
                Virtual_dom.Vdom.Attr.class_ {|a_hash_2cacf009b1|}))
        let b =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__002___2cacf009b1__group_0;
                Virtual_dom.Vdom.Attr.class_ {|b_hash_2cacf009b1|}))
        let c =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__002___2cacf009b1__group_0;
                Virtual_dom.Vdom.Attr.class_ {|c_hash_2cacf009b1|}))
        let d =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__002___2cacf009b1__group_0;
                Virtual_dom.Vdom.Attr.class_ {|d_hash_2cacf009b1|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__002___2cacf009b1__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__002___2cacf009b1__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__002___2cacf009b1__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__002___2cacf009b1__0
           {|
    /* app/foo/foo.ml */

    .a_hash_2cacf009b1:hover::after, .b_hash_2cacf009b1:hover::after, .c_hash_2cacf009b1:hover::after, .d_hash_2cacf009b1:hover::after {
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
    }|};
         Inline_css.Private.update_stylesheet sheet_x__002___2cacf009b1__1
           {|
    /* app/foo/foo.ml */

    .a_hash_2cacf009b1:hover::after {
      top: 100%;
    }|})
    |xxx}]
;;
