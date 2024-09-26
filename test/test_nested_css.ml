open! Core
open! Ppxlib
open! Ppx_css

let loc = Location.none

let%test_module "styled components tests" =
  (module struct
    let test = Test_util.test_expression

    let%expect_test "single class" =
      test
        [%expr
          {|
        background-color: tomato;

        .foo {

        }
      |}];
      (* .foo is hashed: *)
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
                    {|ppx_css_anonymous_class_hash_0700c30dd7|}
              end
          end in Ppx_css_anonymous_style__001_.ppx_css_anonymous_class
        Hoisted context:
        ----------------
        let () =
          Inline_css.Private.append_but_do_not_update
            {|
        /* _none_ */

        *.ppx_css_anonymous_class_hash_0700c30dd7 {
         background-color:tomato;
         *.foo {

         }

        }|}
        |xxx}]
    ;;

    let%expect_test "single class - not hashed" =
      test
        [%expr
          {|
        background-color: tomato;

        .foo {

        }
      |}
            ~dont_hash:[ "foo" ]];
      [%expect
        {xxx| ~dont_hash is a no-op as classes and ids in the *inline* ppx_css syntax are not hashed. |xxx}]
    ;;

    let%expect_test "single class - not hashed prefixes" =
      (* NOTE: This test demonstrates that hashing is turned off in the css for
         classnames as they are otherwise not accessible from the styled component syntax.
      *)
      test
        [%expr
          {|
        background-color: tomato;

        .foo {

        }
      |}
            ~dont_hash_prefixes:[ "f" ]];
      [%expect
        {xxx| ~dont_hash_prefixes is a no-op as classes and ids in the *inline* ppx_css syntax are not hashed. |xxx}]
    ;;

    let%expect_test "single class - unused warning" =
      test
        [%expr
          {|
        background-color: tomato;

        .foo {

        }
      |}
            ~dont_hash:[ "bar" ]];
      [%expect
        {xxx| ~dont_hash is a no-op as classes and ids in the *inline* ppx_css syntax are not hashed. |xxx}];
      test
        [%expr
          {|
        background-color: tomato;

        .foo {

        }
      |}
            ~dont_hash_prefixes:[ "bar" ]];
      [%expect
        {xxx| ~dont_hash_prefixes is a no-op as classes and ids in the *inline* ppx_css syntax are not hashed. |xxx}]
    ;;
  end)
;;

let%test_module "stylesheet components tests" =
  (module struct
    let test = Test_util.test_struct

    let%expect_test "single class" =
      test
        [%expr
          stylesheet
            {|
  .a {
    background-color: tomato;

    .foo {

    }
  }
      |}];
      (* .foo is hashed: *)
      [%expect
        {xxx|
        [@@@ocaml.warning "-32"]
        let __type_info_for_ppx_css :
          ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
          = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
        module type S  =
          sig
            module For_referencing : sig val a : string val foo : string end
            val a : Virtual_dom.Vdom.Attr.t
            val foo : Virtual_dom.Vdom.Attr.t
          end
        type t = (module S)
        module Default : S =
          struct
            module For_referencing =
              struct
                let foo = {|foo_hash_87d2b3d354|}
                let a = {|a_hash_87d2b3d354|}
              end
            let foo = Virtual_dom.Vdom.Attr.class_ {|foo_hash_87d2b3d354|}
            let a = Virtual_dom.Vdom.Attr.class_ {|a_hash_87d2b3d354|}
          end
        include Default
        let default : t = (module Default)
        let () =
          Inline_css.Private.append_but_do_not_update
            {|
        /* _none_ */

        *.a_hash_87d2b3d354 {
         background-color:tomato;
         *.foo_hash_87d2b3d354 {

         }

        }|}
        |xxx}]
    ;;

    let%expect_test "single class - renamed" =
      test
        [%expr
          stylesheet
            {|
     .a {
       background-color: tomato;

       .foo {

       }
     }
       |}
            ~rewrite:[ "foo", "i-am-renamed" ]];
      [%expect
        {xxx|
        The use of ~rewrite is deprecated, please use ~dont_hash or ~dont_hash_prefixes
               instead. Alternatively, consider writing all of your CSS in the same %css
               stylesheet incantation. Deprecating ~rewrite allows for more optimiztions in ppx_css,
               at the expense of expressibility. We've audited bonsai apps and believe this expressibility
               was unused so we've removed it. If this conflicts with your use case please reach out.
        |xxx}]
    ;;

    let%expect_test "single class - not hashed" =
      test
        [%expr
          stylesheet
            {|
     .a {
       background-color: tomato;

       .foo {

       }
     }
       |}
            ~dont_hash:[ "foo" ]];
      [%expect
        {xxx|
        [@@@ocaml.warning "-32"]
        let __type_info_for_ppx_css :
          ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
          = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
        module type S  =
          sig
            module For_referencing : sig val a : string val foo : string end
            val a : Virtual_dom.Vdom.Attr.t
            val foo : Virtual_dom.Vdom.Attr.t
          end
        type t = (module S)
        module Default : S =
          struct
            module For_referencing =
              struct let foo = {|foo|}
                     let a = {|a_hash_87d2b3d354|} end
            let foo = Virtual_dom.Vdom.Attr.class_ {|foo|}
            let a = Virtual_dom.Vdom.Attr.class_ {|a_hash_87d2b3d354|}
          end
        include Default
        let default : t = (module Default)
        let () =
          Inline_css.Private.append_but_do_not_update
            {|
        /* _none_ */

        *.a_hash_87d2b3d354 {
         background-color:tomato;
         *.foo {

         }

        }|}
        |xxx}]
    ;;

    let%expect_test "single class - not hashed prefixes" =
      test
        [%expr
          stylesheet
            {|
    .a {
       background-color: tomato;

       .foo {

       }
    }
       |}
            ~dont_hash_prefixes:[ "f" ]];
      [%expect
        {xxx|
        [@@@ocaml.warning "-32"]
        let __type_info_for_ppx_css :
          ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
          = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
        module type S  =
          sig
            module For_referencing : sig val a : string val foo : string end
            val a : Virtual_dom.Vdom.Attr.t
            val foo : Virtual_dom.Vdom.Attr.t
          end
        type t = (module S)
        module Default : S =
          struct
            module For_referencing =
              struct let foo = {|foo|}
                     let a = {|a_hash_87d2b3d354|} end
            let foo = Virtual_dom.Vdom.Attr.class_ {|foo|}
            let a = Virtual_dom.Vdom.Attr.class_ {|a_hash_87d2b3d354|}
          end
        include Default
        let default : t = (module Default)
        let () =
          Inline_css.Private.append_but_do_not_update
            {|
        /* _none_ */

        *.a_hash_87d2b3d354 {
         background-color:tomato;
         *.foo {

         }

        }|}
        |xxx}]
    ;;

    let%expect_test "single class - unused warning" =
      test
        [%expr
          stylesheet
            {|
     .a {
       background-color: tomato;

       .foo {

       }
     }
       |}
            ~dont_hash:[ "bar" ]];
      [%expect {xxx| Unused keys: (bar) |xxx}];
      test
        [%expr
          stylesheet
            {|
     .a {
       background-color: tomato;

       .foo {

       }
     }
       |}
            ~dont_hash_prefixes:[ "bar" ]];
      [%expect {xxx| Unused prefixes: (bar) |xxx}]
    ;;
  end)
;;
