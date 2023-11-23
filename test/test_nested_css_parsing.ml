open! Core
open Ppxlib
open Ppx_css

let loc = Location.none

let catch_location_error ~f =
  try f () with
  | ex ->
    (match Location.Error.of_exn ex with
     | Some error -> print_endline (Location.Error.message error)
     | None -> raise ex)
;;

let print_heading s =
  let bar = String.init (String.length s) ~f:(Fn.const '-') in
  print_endline s;
  print_endline bar
;;

let%test_module "styled component parsing tests" =
  (module struct
    let test expr =
      catch_location_error ~f:(fun () ->
        let%tydi { txt = expression; ppx_css_string_expression } =
          expr |> For_testing.generate_inline_expression
        in
        print_heading "Expression context:";
        expression |> Pprintast.string_of_expression |> print_endline;
        print_heading "Hoisted context:";
        print_endline
          (Pprintast.string_of_structure
             [ For_testing.ppx_css_expression_to_structure_item
                 ~loc
                 ppx_css_string_expression
             ]))
    ;;

    let%expect_test "basic" =
      test
        [%expr
          {|
        background-color: tomato;

        &:hover {
          background-color: black;
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
                {|ppx_css_anonymous_class_hash_06c0c801b4|}
          end
      end in Ppx_css_anonymous_style__001_.ppx_css_anonymous_class
    Hoisted context:
    ----------------
    let () =
      Inline_css.Private.append
        {|
    /* _none_ */

    *.ppx_css_anonymous_class_hash_06c0c801b4 {
     background-color:tomato;
     *&:hover {
      background-color:black
     }

    }|} |xxx}]
    ;;

    let%expect_test "multiple" =
      test
        [%expr
          {|
        background-color: tomato;

        &:foo { };

        &:bar { };
      |}];
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
                {|ppx_css_anonymous_class_hash_1f33a9ce80|}
          end
      end in Ppx_css_anonymous_style__002_.ppx_css_anonymous_class
    Hoisted context:
    ----------------
    let () =
      Inline_css.Private.append
        {|
    /* _none_ */

    *.ppx_css_anonymous_class_hash_1f33a9ce80 {
     background-color:tomato;
     *&:foo {

     }
     ;
     *&:bar {

     }

    }|} |xxx}]
    ;;

    let%expect_test "multiple without a separating semicolon" =
      test
        [%expr
          {|
        background-color: tomato;

        &:foo { }

        &:bar { }
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
                {|ppx_css_anonymous_class_hash_1f33a9ce80|}
          end
      end in Ppx_css_anonymous_style__003_.ppx_css_anonymous_class
    Hoisted context:
    ----------------
    let () =
      Inline_css.Private.append
        {|
    /* _none_ */

    *.ppx_css_anonymous_class_hash_1f33a9ce80 {
     background-color:tomato;
     *&:foo {

     }
     ;
     *&:bar {

     }

    }|} |xxx}]
    ;;

    let%expect_test "multiple without a separating semicolon, but mixed with ones that \
                     actually need a semicolon"
      =
      test
        [%expr
          {| background-color: tomato;

        &:foo { }

        background-color: hotpink;

        &:bar { }
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
                {|ppx_css_anonymous_class_hash_4526837167|}
          end
      end in Ppx_css_anonymous_style__004_.ppx_css_anonymous_class
    Hoisted context:
    ----------------
    let () =
      Inline_css.Private.append
        {|
    /* _none_ */

    *.ppx_css_anonymous_class_hash_4526837167 {
     background-color:tomato;
     *&:foo {

     }
     ;
     background-color:hotpink;
     *&:bar {

     }

    }|} |xxx}];
      test
        [%expr
          {|
        background-color: tomato;

        &:foo { }

        background-color: hotpink

        &:bar { }
      |}];
      [%expect {| Parse error while reading token '&' |}]
    ;;

    let%expect_test "parsing regression test" =
      (* The bug was that we ate up the whitespace between the & and then '*' *)
      test [%expr {|
    & * { }
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
                    {|ppx_css_anonymous_class_hash_0c03167bcf|}
              end
          end in Ppx_css_anonymous_style__005_.ppx_css_anonymous_class
        Hoisted context:
        ----------------
        let () =
          Inline_css.Private.append
            {|
        /* _none_ */

        *.ppx_css_anonymous_class_hash_0c03167bcf {
         *& * {

         }

        }|} |xxx}];
      test [%expr {|
    & & { }
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
                    {|ppx_css_anonymous_class_hash_1dd86cc423|}
              end
          end in Ppx_css_anonymous_style__006_.ppx_css_anonymous_class
        Hoisted context:
        ----------------
        let () =
          Inline_css.Private.append
            {|
        /* _none_ */

        *.ppx_css_anonymous_class_hash_1dd86cc423 {
         *& *& {

         }

        }|} |xxx}];
      test [%expr {|
    & .foo .foo { }
      |}];
      [%expect
        {xxx|
        Expression context:
        -------------------
        let module Ppx_css_anonymous_style__007_ =
          struct
            include
              struct
                let ppx_css_anonymous_class =
                  Virtual_dom.Vdom.Attr.class_
                    {|ppx_css_anonymous_class_hash_37b9d59af4|}
                let foo = Virtual_dom.Vdom.Attr.class_ {|foo_hash_37b9d59af4|}
              end
          end in Ppx_css_anonymous_style__007_.ppx_css_anonymous_class
        Hoisted context:
        ----------------
        let () =
          Inline_css.Private.append
            {|
        /* _none_ */

        *.ppx_css_anonymous_class_hash_37b9d59af4 {
         *& *.foo_hash_37b9d59af4 *.foo_hash_37b9d59af4 {

         }

        }|} |xxx}];
      (* This shows that the behavior for '+' is currently identical to the behavior for
         '&', which while the behavior for + is correct, the behavior for & is incorrect.

         The bug was that & was printed as a delimeter which is wrong. The fix was to
         instead parse ampersands into their own thing.
      *)
      test [%expr {|
    + .foo .foo { }
      |}];
      [%expect
        {xxx|
        Expression context:
        -------------------
        let module Ppx_css_anonymous_style__008_ =
          struct
            include
              struct
                let ppx_css_anonymous_class =
                  Virtual_dom.Vdom.Attr.class_
                    {|ppx_css_anonymous_class_hash_c4d4e6073e|}
                let foo = Virtual_dom.Vdom.Attr.class_ {|foo_hash_c4d4e6073e|}
              end
          end in Ppx_css_anonymous_style__008_.ppx_css_anonymous_class
        Hoisted context:
        ----------------
        let () =
          Inline_css.Private.append
            {|
        /* _none_ */

        *.ppx_css_anonymous_class_hash_c4d4e6073e {
         +*.foo_hash_c4d4e6073e *.foo_hash_c4d4e6073e {

         }

        }|} |xxx}]
    ;;

    let%expect_test "& :hover is distinct from &:hover" =
      test [%expr {|
        & :hover {

        }
      |}];
      [%expect
        {xxx|
        Expression context:
        -------------------
        let module Ppx_css_anonymous_style__009_ =
          struct
            include
              struct
                let ppx_css_anonymous_class =
                  Virtual_dom.Vdom.Attr.class_
                    {|ppx_css_anonymous_class_hash_7d3ff84693|}
              end
          end in Ppx_css_anonymous_style__009_.ppx_css_anonymous_class
        Hoisted context:
        ----------------
        let () =
          Inline_css.Private.append
            {|
        /* _none_ */

        *.ppx_css_anonymous_class_hash_7d3ff84693 {
         *& *:hover {

         }

        }|} |xxx}];
      test [%expr {|
        &:hover {

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
                let ppx_css_anonymous_class =
                  Virtual_dom.Vdom.Attr.class_
                    {|ppx_css_anonymous_class_hash_1d06b4b6e2|}
              end
          end in Ppx_css_anonymous_style__010_.ppx_css_anonymous_class
        Hoisted context:
        ----------------
        let () =
          Inline_css.Private.append
            {|
        /* _none_ */

        *.ppx_css_anonymous_class_hash_1d06b4b6e2 {
         *&:hover {

         }

        }|} |xxx}]
    ;;

    let%expect_test "nested & within functions work" =
      test [%expr {|
        & :has(&) {

        }
      |}];
      [%expect
        {xxx|
        Expression context:
        -------------------
        let module Ppx_css_anonymous_style__011_ =
          struct
            include
              struct
                let ppx_css_anonymous_class =
                  Virtual_dom.Vdom.Attr.class_
                    {|ppx_css_anonymous_class_hash_3a6c7e4ea2|}
              end
          end in Ppx_css_anonymous_style__011_.ppx_css_anonymous_class
        Hoisted context:
        ----------------
        let () =
          Inline_css.Private.append
            {|
        /* _none_ */

        *.ppx_css_anonymous_class_hash_3a6c7e4ea2 {
         *& *:has(&) {

         }

        }|} |xxx}]
    ;;

    let%expect_test "Nested functions allow for ident starting identifier" =
      test [%expr {|
        & :has(foo) {

        }
      |}];
      [%expect
        {xxx|
        Expression context:
        -------------------
        let module Ppx_css_anonymous_style__012_ =
          struct
            include
              struct
                let ppx_css_anonymous_class =
                  Virtual_dom.Vdom.Attr.class_
                    {|ppx_css_anonymous_class_hash_0585b06e67|}
              end
          end in Ppx_css_anonymous_style__012_.ppx_css_anonymous_class
        Hoisted context:
        ----------------
        let () =
          Inline_css.Private.append
            {|
        /* _none_ */

        *.ppx_css_anonymous_class_hash_0585b06e67 {
         *& *:has(foo) {

         }

        }|} |xxx}]
    ;;

    let%expect_test "Complex nested functions" =
      test
        [%expr
          {|
        & :has(& :has - & + & > & #foo> & .foo&:has(& :has(& .foo &))) {

        }
      |}];
      [%expect
        {xxx|
        Expression context:
        -------------------
        let module Ppx_css_anonymous_style__013_ =
          struct
            include
              struct
                let ppx_css_anonymous_class =
                  Virtual_dom.Vdom.Attr.class_
                    {|ppx_css_anonymous_class_hash_1deb4e5862|}
                let foo = Virtual_dom.Vdom.Attr.empty
                let foo_class = Virtual_dom.Vdom.Attr.class_ {|foo_hash_1deb4e5862|}
                let foo_id = Virtual_dom.Vdom.Attr.id {|foo_hash_1deb4e5862|}
              end
          end in Ppx_css_anonymous_style__013_.ppx_css_anonymous_class
        Hoisted context:
        ----------------
        let () =
          Inline_css.Private.append
            {|
        /* _none_ */

        *.ppx_css_anonymous_class_hash_1deb4e5862 {
         *& *:has(& *:has-*&+*&>*& *#foo_hash_1deb4e5862>*& *.foo_hash_1deb4e5862 &:has(& *:has(& *.foo_hash_1deb4e5862 *&))) {

         }

        }|} |xxx}]
    ;;

    let%expect_test "different kinds of delimiters" =
      (* NOTE: This test is weird, but shows right behavior. ~,-,+,> are delimeters
         that are fine to remove the whitespace, but other identifiers are not
         fine to remove the white space. This output shows correct behavior to my
         understanding. *)
      test
        [%expr {|
        & :has(foo) + .bar + & ~ & > & - & #foo {

        }
      |}];
      [%expect
        {xxx|
        Expression context:
        -------------------
        let module Ppx_css_anonymous_style__014_ =
          struct
            include
              struct
                let ppx_css_anonymous_class =
                  Virtual_dom.Vdom.Attr.class_
                    {|ppx_css_anonymous_class_hash_7f6fcf5e26|}
                let foo = Virtual_dom.Vdom.Attr.id {|foo_hash_7f6fcf5e26|}
                let bar = Virtual_dom.Vdom.Attr.class_ {|bar_hash_7f6fcf5e26|}
              end
          end in Ppx_css_anonymous_style__014_.ppx_css_anonymous_class
        Hoisted context:
        ----------------
        let () =
          Inline_css.Private.append
            {|
        /* _none_ */

        *.ppx_css_anonymous_class_hash_7f6fcf5e26 {
         *& *:has(foo)+*.bar_hash_7f6fcf5e26+*&~*&>*&-*& *#foo_hash_7f6fcf5e26 {

         }

        }|} |xxx}]
    ;;

    let%expect_test "more complex & interactions" =
      test [%expr {|
        & :has(.foo), &:hover {

        }
      |}];
      [%expect
        {xxx|
        Expression context:
        -------------------
        let module Ppx_css_anonymous_style__015_ =
          struct
            include
              struct
                let ppx_css_anonymous_class =
                  Virtual_dom.Vdom.Attr.class_
                    {|ppx_css_anonymous_class_hash_8a70b922be|}
                let foo = Virtual_dom.Vdom.Attr.class_ {|foo_hash_8a70b922be|}
              end
          end in Ppx_css_anonymous_style__015_.ppx_css_anonymous_class
        Hoisted context:
        ----------------
        let () =
          Inline_css.Private.append
            {|
        /* _none_ */

        *.ppx_css_anonymous_class_hash_8a70b922be {
         *& *:has(.foo_hash_8a70b922be),*&:hover {

         }

        }|} |xxx}]
    ;;
  end)
;;

let%test_module "stylesheet parsing tests" =
  (module struct
    let test expr =
      catch_location_error ~f:(fun () ->
        let%tydi { txt = structure; ppx_css_string_expression } =
          For_testing.generate_struct expr
        in
        let structure =
          structure
          @ [ For_testing.ppx_css_expression_to_structure_item
                ~loc:Location.none
                ppx_css_string_expression
            ]
        in
        print_endline (Pprintast.string_of_structure structure))
    ;;

    let%expect_test "top-level ampersand is disallowed" =
      test [%expr stylesheet {| & {} |}];
      [%expect {xxx|
        Parse error while reading token '&' |xxx}]
    ;;

    let%expect_test "nested ampersand is allowed" =
      test [%expr stylesheet {|.foo { & {} } |}];
      [%expect
        {xxx|
        [@@@ocaml.warning "-32"]
        let __type_info_for_ppx_css :
          ?rewrite:(string * string) list ->
            ?dont_hash:string list ->
              ?dont_hash_prefixes:string list -> string -> unit
          = fun ?rewrite:_ ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
        module type S  =
          sig
            module For_referencing : sig val foo : string end
            val foo : Virtual_dom.Vdom.Attr.t
          end
        type t = (module S)
        module Default : S =
          struct
            module For_referencing = struct let foo = {|foo_hash_ed1292223b|} end
            let foo = Virtual_dom.Vdom.Attr.class_ {|foo_hash_ed1292223b|}
          end
        include Default
        let default : t = (module Default)
        let () =
          Inline_css.Private.append
            {|
        /* _none_ */

        *.foo_hash_ed1292223b {
         *& {

         }

        }|} |xxx}]
    ;;

    let%expect_test "nested ampersand is allowed (all the way down)" =
      test [%expr stylesheet {|.foo { & { &.bar { &.baz { } } } } |}];
      [%expect
        {xxx|
      [@@@ocaml.warning "-32"]
      let __type_info_for_ppx_css :
        ?rewrite:(string * string) list ->
          ?dont_hash:string list ->
            ?dont_hash_prefixes:string list -> string -> unit
        = fun ?rewrite:_ ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
      module type S  =
        sig
          module For_referencing :
          sig val bar : string val baz : string val foo : string end
          val bar : Virtual_dom.Vdom.Attr.t
          val baz : Virtual_dom.Vdom.Attr.t
          val foo : Virtual_dom.Vdom.Attr.t
        end
      type t = (module S)
      module Default : S =
        struct
          module For_referencing =
            struct
              let foo = {|foo_hash_66de0ac9e5|}
              let baz = {|baz_hash_66de0ac9e5|}
              let bar = {|bar_hash_66de0ac9e5|}
            end
          let foo = Virtual_dom.Vdom.Attr.class_ {|foo_hash_66de0ac9e5|}
          let baz = Virtual_dom.Vdom.Attr.class_ {|baz_hash_66de0ac9e5|}
          let bar = Virtual_dom.Vdom.Attr.class_ {|bar_hash_66de0ac9e5|}
        end
      include Default
      let default : t = (module Default)
      let () =
        Inline_css.Private.append
          {|
      /* _none_ */

      *.foo_hash_66de0ac9e5 {
       *& {
        *&.bar_hash_66de0ac9e5 {
         *&.baz_hash_66de0ac9e5 {

         }

        }

       }

      }|} |xxx}]
    ;;
  end)
;;
