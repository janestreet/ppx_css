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
    let module Ppx_css_anonymous_style__001_ =
      struct
        include
          struct
            let ppx_css_anonymous_class =
              Virtual_dom.Vdom.Attr.class_
                {|ppx_css_anonymous_class_hash_2cd13a7a14|}
          end
      end in Ppx_css_anonymous_style__001_.ppx_css_anonymous_class
    Hoisted context:
    ----------------
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.ppx_css_anonymous_class_hash_2cd13a7a14 {
     *&.foo {
      background-color:tomato
     }

    }|}
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

let%expect_test "with classes - rewrite is supplied" =
  test
    [%expr
      {|
       &.foo {
         background-color: tomato;
       }
|}
        ~rewrite:[ "foo", "foo123" ]];
  [%expect
    {xxx|
    Expression context:
    -------------------
    let module Ppx_css_anonymous_style__002_ =
      struct
        include
          struct
            let ppx_css_anonymous_class =
              Virtual_dom.Vdom.Attr.class_
                {|ppx_css_anonymous_class_hash_2cd13a7a14|}
          end
      end in Ppx_css_anonymous_style__002_.ppx_css_anonymous_class
    Hoisted context:
    ----------------
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.ppx_css_anonymous_class_hash_2cd13a7a14 {
     *&.foo123 {
      background-color:tomato
     }

    }|}
    |xxx}]
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
    let module Ppx_css_anonymous_style__003_ =
      struct
        include
          struct
            let ppx_css_anonymous_class =
              Virtual_dom.Vdom.Attr.class_
                {|ppx_css_anonymous_class_hash_8259a5645c|}
          end
      end in Ppx_css_anonymous_style__003_.ppx_css_anonymous_class
    Hoisted context:
    ----------------
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.ppx_css_anonymous_class_hash_8259a5645c {
     *&#foo {
      background-color:tomato
     }

    }|}
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
    let module Ppx_css_anonymous_style__004_ =
      struct
        include
          struct
            let ppx_css_anonymous_class =
              Virtual_dom.Vdom.Attr.class_
                {|ppx_css_anonymous_class_hash_a2effd2713|}
          end
      end in Ppx_css_anonymous_style__004_.ppx_css_anonymous_class
    Hoisted context:
    ----------------
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.ppx_css_anonymous_class_hash_a2effd2713 {
     *& {
      background-color:var(--foo)
     }

    }|}
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
    let module Ppx_css_anonymous_style__005_ =
      struct
        include
          struct
            let ppx_css_anonymous_class =
              Virtual_dom.Vdom.Attr.class_
                {|ppx_css_anonymous_class_hash_5dddd95aa0|}
          end
      end in Ppx_css_anonymous_style__005_.ppx_css_anonymous_class
    Hoisted context:
    ----------------
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.ppx_css_anonymous_class_hash_5dddd95aa0 {
     *.cm-editor {
      overflow:auto;
      max-height:90vh
     }

    }|}
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
    let module Ppx_css_anonymous_style__006_ =
      struct
        include
          struct
            let ppx_css_anonymous_class =
              Virtual_dom.Vdom.Attr.class_
                {|ppx_css_anonymous_class_hash_795fb9f5fa|}
          end
      end in Ppx_css_anonymous_style__006_.ppx_css_anonymous_class
    Hoisted context:
    ----------------
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.ppx_css_anonymous_class_hash_795fb9f5fa {
     *.a-b_c-d {
      overflow:auto;
      max-height:90vh
     }

    }|}
    |xxx}]
;;
