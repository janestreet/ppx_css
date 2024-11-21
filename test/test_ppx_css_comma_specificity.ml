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
            let a = {|a_hash_5975ba6d79|}
            let b = {|b_hash_5975ba6d79|}
            let c = {|c_hash_5975ba6d79|}
            let d = {|d_hash_5975ba6d79|}
          end
        let a =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__001___5975ba6d79__group_2;
                Virtual_dom.Vdom.Attr.class_ {|a_hash_5975ba6d79|}))
        let b =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__001___5975ba6d79__group_3;
                Virtual_dom.Vdom.Attr.class_ {|b_hash_5975ba6d79|}))
        let c =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__001___5975ba6d79__group_0;
                Virtual_dom.Vdom.Attr.class_ {|c_hash_5975ba6d79|}))
        let d =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__001___5975ba6d79__group_1;
                Virtual_dom.Vdom.Attr.class_ {|d_hash_5975ba6d79|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__001___5975ba6d79__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__001___5975ba6d79__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__001___5975ba6d79__2 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__001___5975ba6d79__3 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__001___5975ba6d79__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__001___5975ba6d79__2
           {|
    /* _none_ */

    *.c_hash_5975ba6d79:hover::after {
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
    }|})
    let update_sheet_lazy_fn_x__001___5975ba6d79__group_1 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__001___5975ba6d79__3
           {|
    /* _none_ */

    *.d_hash_5975ba6d79:hover::after {
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
    }|})
    let update_sheet_lazy_fn_x__001___5975ba6d79__group_2 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__001___5975ba6d79__0
           {|
    /* _none_ */

    *.a_hash_5975ba6d79:hover::after {
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
    }|})
    let update_sheet_lazy_fn_x__001___5975ba6d79__group_3 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__001___5975ba6d79__1
           {|
    /* _none_ */

    *.b_hash_5975ba6d79:hover::after {
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
            let a = {|a_hash_02042c0574|}
            let b = {|b_hash_02042c0574|}
            let c = {|c_hash_02042c0574|}
            let d = {|d_hash_02042c0574|}
          end
        let a =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__002___02042c0574__group_0;
                Virtual_dom.Vdom.Attr.class_ {|a_hash_02042c0574|}))
        let b =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__002___02042c0574__group_0;
                Virtual_dom.Vdom.Attr.class_ {|b_hash_02042c0574|}))
        let c =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__002___02042c0574__group_0;
                Virtual_dom.Vdom.Attr.class_ {|c_hash_02042c0574|}))
        let d =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__002___02042c0574__group_0;
                Virtual_dom.Vdom.Attr.class_ {|d_hash_02042c0574|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__002___02042c0574__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__002___02042c0574__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__002___02042c0574__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__002___02042c0574__0
           {|
    /* _none_ */

    *.a_hash_02042c0574:hover::after,*.b_hash_02042c0574:hover::after,*.c_hash_02042c0574:hover::after,*.d_hash_02042c0574:hover::after {
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
    }|};
         Inline_css.Private.update_stylesheet sheet_x__002___02042c0574__1
           {|
    /* _none_ */

    *.a_hash_02042c0574:hover::after {
     top:100%
    }|})
    |xxx}]
;;
