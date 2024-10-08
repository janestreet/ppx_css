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
            let inline_class =
              Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_eb6fc2da0b|}
          end
      end in Ppx_css_anonymous_style__001_.inline_class
    Hoisted context:
    ----------------
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.inline_class_hash_eb6fc2da0b {
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
    let module Ppx_css_anonymous_style__002_ =
      struct
        include
          struct
            let inline_class =
              Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_64b5b54d28|}
          end
      end in Ppx_css_anonymous_style__002_.inline_class
    Hoisted context:
    ----------------
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.inline_class_hash_64b5b54d28 {
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
    let module Ppx_css_anonymous_style__003_ =
      struct
        include
          struct
            let inline_class =
              Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_b533eea165|}
          end
      end in Ppx_css_anonymous_style__003_.inline_class
    Hoisted context:
    ----------------
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.inline_class_hash_b533eea165 {
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
    let module Ppx_css_anonymous_style__004_ =
      struct
        include
          struct
            let inline_class =
              Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_a073221f2b|}
          end
      end in Ppx_css_anonymous_style__004_.inline_class
    Hoisted context:
    ----------------
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.inline_class_hash_a073221f2b {
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
    let module Ppx_css_anonymous_style__005_ =
      struct
        include
          struct
            let inline_class =
              Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_0550d7d305|}
          end
      end in Ppx_css_anonymous_style__005_.inline_class
    Hoisted context:
    ----------------
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.inline_class_hash_0550d7d305 {
     *.a-b_c-d {
      overflow:auto;
      max-height:90vh
     }

    }|}
    |xxx}]
;;
