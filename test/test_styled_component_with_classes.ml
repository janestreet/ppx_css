open! Core
open Ppxlib

let test = Test_util.test_expression
let loc = Test_util.loc_with_mock_name
let () = Ppx_css_syntax.Preprocess_arguments.set_lazy_loading_optimization (Some true)

let%expect_test "with classes" =
  test
    [%expr
      {|
       &.foo {
         background-color: tomato;
       }
|}];
  [%expect
    {xxx|
    Expression context:
    -------------------
    let module Ppx_css_anonymous_style__002_ =
      struct
        include
          struct
            let foo__inline_class =
              Virtual_dom.Vdom.Attr.lazy_
                (lazy
                   (Inline_css.Ppx_css_runtime.force
                      Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__001___bfaee5db74__group_0;
                    Virtual_dom.Vdom.Attr.class_
                      {|foo__inline_class_hash_bfaee5db74|}))
          end
      end in Ppx_css_anonymous_style__002_.foo__inline_class
    Hoisted context:
    ----------------
    let sheet_x__001___bfaee5db74__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__001___bfaee5db74__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__001___bfaee5db74__0
           {|
    /* app/foo/foo.ml */

    .foo__inline_class_hash_bfaee5db74 {
      &.foo {
        background-color: tomato;
      }
    }|})
    |xxx}]
;;

let%expect_test "with classes - dont hash is supplied" =
  test
    [%expr
      {|
       &.foo {
         background-color: tomato;
       }
|}
        ~dont_hash:[ "foo" ]];
  [%expect
    {xxx| ~dont_hash is a no-op as classes and ids in the *inline* ppx_css syntax are not hashed. |xxx}]
;;

let%expect_test "with classes - dont hash prefixes is provided" =
  (* In non-stylesheet [%css] invocations, identifiers aren't hashed, so prefixes are
     always unused. *)
  test
    [%expr
      {|
       &.foo {
         background-color: tomato;
       }
|}
        ~dont_hash_prefixes:[ "" ]];
  [%expect
    {xxx| ~dont_hash_prefixes is a no-op as classes and ids in the *inline* ppx_css syntax are not hashed. |xxx}]
;;

let%expect_test "with ids" =
  test
    [%expr
      {|
       &#foo {
         background-color: tomato;
       }
|}];
  [%expect
    {xxx|
    Expression context:
    -------------------
    let module Ppx_css_anonymous_style__004_ =
      struct
        include
          struct
            let foo__inline_class =
              Virtual_dom.Vdom.Attr.lazy_
                (lazy
                   (Inline_css.Ppx_css_runtime.force
                      Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__003___1e7f0d9be6__group_0;
                    Virtual_dom.Vdom.Attr.class_
                      {|foo__inline_class_hash_1e7f0d9be6|}))
          end
      end in Ppx_css_anonymous_style__004_.foo__inline_class
    Hoisted context:
    ----------------
    let sheet_x__003___1e7f0d9be6__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__003___1e7f0d9be6__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__003___1e7f0d9be6__0
           {|
    /* app/foo/foo.ml */

    .foo__inline_class_hash_1e7f0d9be6 {
      &#foo {
        background-color: tomato;
      }
    }|})
    |xxx}]
;;

let%expect_test "with variables" =
  (* NOTE: This behavior is correct. *)
  test
    [%expr
      {|
       & {
         background-color: var(--foo);
       }
|}];
  [%expect
    {xxx|
    Expression context:
    -------------------
    let module Ppx_css_anonymous_style__006_ =
      struct
        include
          struct
            let foo__inline_class =
              Virtual_dom.Vdom.Attr.lazy_
                (lazy
                   (Inline_css.Ppx_css_runtime.force
                      Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__005___6895ae2863__group_0;
                    Virtual_dom.Vdom.Attr.class_
                      {|foo__inline_class_hash_6895ae2863|}))
          end
      end in Ppx_css_anonymous_style__006_.foo__inline_class
    Hoisted context:
    ----------------
    let sheet_x__005___6895ae2863__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__005___6895ae2863__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__005___6895ae2863__0
           {|
    /* app/foo/foo.ml */

    .foo__inline_class_hash_6895ae2863 {
      & {
        background-color: var(--foo);
      }
    }|})
    |xxx}]
;;

let%expect_test {|ppx css does structure item generation check does not work with identifiers with '-' in them|}
  =
  test
    [%expr
      {|
         .cm-editor {
           overflow: auto;
           max-height: 90vh;
         }
        |}];
  [%expect
    {xxx|
    Expression context:
    -------------------
    let module Ppx_css_anonymous_style__008_ =
      struct
        include
          struct
            let foo__inline_class =
              Virtual_dom.Vdom.Attr.lazy_
                (lazy
                   (Inline_css.Ppx_css_runtime.force
                      Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__007___c455637f30__group_0;
                    Virtual_dom.Vdom.Attr.class_
                      {|foo__inline_class_hash_c455637f30|}))
          end
      end in Ppx_css_anonymous_style__008_.foo__inline_class
    Hoisted context:
    ----------------
    let sheet_x__007___c455637f30__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__007___c455637f30__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__007___c455637f30__0
           {|
    /* app/foo/foo.ml */

    .foo__inline_class_hash_c455637f30 {
      .cm-editor {
        overflow: auto;
        max-height: 90vh;
      }
    }|})
    |xxx}]
;;

let%expect_test {|Sanitizing hyphens vs underscores are respected.|} =
  test
    [%expr
      {|
         .a-b_c-d {
           overflow: auto;
           max-height: 90vh;
         }
        |}];
  [%expect
    {xxx|
    Expression context:
    -------------------
    let module Ppx_css_anonymous_style__010_ =
      struct
        include
          struct
            let foo__inline_class =
              Virtual_dom.Vdom.Attr.lazy_
                (lazy
                   (Inline_css.Ppx_css_runtime.force
                      Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__009___64fffabf77__group_0;
                    Virtual_dom.Vdom.Attr.class_
                      {|foo__inline_class_hash_64fffabf77|}))
          end
      end in Ppx_css_anonymous_style__010_.foo__inline_class
    Hoisted context:
    ----------------
    let sheet_x__009___64fffabf77__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__009___64fffabf77__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__009___64fffabf77__0
           {|
    /* app/foo/foo.ml */

    .foo__inline_class_hash_64fffabf77 {
      .a-b_c-d {
        overflow: auto;
        max-height: 90vh;
      }
    }|})
    |xxx}]
;;
