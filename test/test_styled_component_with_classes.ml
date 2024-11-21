open! Core
open Ppxlib

let test = Test_util.test_expression
let loc = Location.none

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
            let inline_class =
              Virtual_dom.Vdom.Attr.lazy_
                (lazy
                   (Inline_css.Ppx_css_runtime.force
                      Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__001___f4478b7634__group_0;
                    Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_f4478b7634|}))
          end
      end in Ppx_css_anonymous_style__002_.inline_class
    Hoisted context:
    ----------------
    let sheet_x__001___f4478b7634__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__001___f4478b7634__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__001___f4478b7634__0
           {|
    /* _none_ */

    *.inline_class_hash_f4478b7634 {
     *&.foo {
      background-color:tomato
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
     always unused.*)
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
            let inline_class =
              Virtual_dom.Vdom.Attr.lazy_
                (lazy
                   (Inline_css.Ppx_css_runtime.force
                      Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__003___dfcdc41e57__group_0;
                    Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_dfcdc41e57|}))
          end
      end in Ppx_css_anonymous_style__004_.inline_class
    Hoisted context:
    ----------------
    let sheet_x__003___dfcdc41e57__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__003___dfcdc41e57__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__003___dfcdc41e57__0
           {|
    /* _none_ */

    *.inline_class_hash_dfcdc41e57 {
     *&#foo {
      background-color:tomato
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
            let inline_class =
              Virtual_dom.Vdom.Attr.lazy_
                (lazy
                   (Inline_css.Ppx_css_runtime.force
                      Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__005___35ceafd5a7__group_0;
                    Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_35ceafd5a7|}))
          end
      end in Ppx_css_anonymous_style__006_.inline_class
    Hoisted context:
    ----------------
    let sheet_x__005___35ceafd5a7__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__005___35ceafd5a7__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__005___35ceafd5a7__0
           {|
    /* _none_ */

    *.inline_class_hash_35ceafd5a7 {
     *& {
      background-color:var(--foo)
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
            let inline_class =
              Virtual_dom.Vdom.Attr.lazy_
                (lazy
                   (Inline_css.Ppx_css_runtime.force
                      Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__007___acd9311382__group_0;
                    Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_acd9311382|}))
          end
      end in Ppx_css_anonymous_style__008_.inline_class
    Hoisted context:
    ----------------
    let sheet_x__007___acd9311382__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__007___acd9311382__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__007___acd9311382__0
           {|
    /* _none_ */

    *.inline_class_hash_acd9311382 {
     *.cm-editor {
      overflow:auto;
      max-height:90vh
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
            let inline_class =
              Virtual_dom.Vdom.Attr.lazy_
                (lazy
                   (Inline_css.Ppx_css_runtime.force
                      Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__009___0a2848787d__group_0;
                    Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_0a2848787d|}))
          end
      end in Ppx_css_anonymous_style__010_.inline_class
    Hoisted context:
    ----------------
    let sheet_x__009___0a2848787d__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__009___0a2848787d__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__009___0a2848787d__0
           {|
    /* _none_ */

    *.inline_class_hash_0a2848787d {
     *.a-b_c-d {
      overflow:auto;
      max-height:90vh
     }

    }|})
    |xxx}]
;;
