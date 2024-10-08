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

module%test [@name "styled component parsing tests"] _ = struct
  let test expr =
    catch_location_error ~f:(fun () ->
      let%tydi { txt = expression; ppx_css_string_expression; _ } =
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
              let inline_class =
                Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_e0029d6e83|}
            end
        end in Ppx_css_anonymous_style__001_.inline_class
      Hoisted context:
      ----------------
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* _none_ */

      *.inline_class_hash_e0029d6e83 {
       background-color:tomato;
       *&:hover {
        background-color:black
       }

      }|}
      |xxx}]
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
              let inline_class =
                Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_a2f863bcb8|}
            end
        end in Ppx_css_anonymous_style__002_.inline_class
      Hoisted context:
      ----------------
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* _none_ */

      *.inline_class_hash_a2f863bcb8 {
       background-color:tomato;
       *&:foo {

       }
       ;
       *&:bar {

       }

      }|}
      |xxx}]
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
              let inline_class =
                Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_a2f863bcb8|}
            end
        end in Ppx_css_anonymous_style__003_.inline_class
      Hoisted context:
      ----------------
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* _none_ */

      *.inline_class_hash_a2f863bcb8 {
       background-color:tomato;
       *&:foo {

       }
       ;
       *&:bar {

       }

      }|}
      |xxx}]
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
              let inline_class =
                Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_0d9bd9b2c6|}
            end
        end in Ppx_css_anonymous_style__004_.inline_class
      Hoisted context:
      ----------------
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* _none_ */

      *.inline_class_hash_0d9bd9b2c6 {
       background-color:tomato;
       *&:foo {

       }
       ;
       background-color:hotpink;
       *&:bar {

       }

      }|}
      |xxx}];
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
    test
      [%expr
        {|
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
              let inline_class =
                Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_5ad9002e7e|}
            end
        end in Ppx_css_anonymous_style__005_.inline_class
      Hoisted context:
      ----------------
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* _none_ */

      *.inline_class_hash_5ad9002e7e {
       *& * {

       }

      }|}
      |xxx}];
    test
      [%expr
        {|
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
              let inline_class =
                Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_03384ca697|}
            end
        end in Ppx_css_anonymous_style__006_.inline_class
      Hoisted context:
      ----------------
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* _none_ */

      *.inline_class_hash_03384ca697 {
       *& *& {

       }

      }|}
      |xxx}];
    test
      [%expr
        {|
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
              let inline_class =
                Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_fe8ac9a659|}
            end
        end in Ppx_css_anonymous_style__007_.inline_class
      Hoisted context:
      ----------------
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* _none_ */

      *.inline_class_hash_fe8ac9a659 {
       *& *.foo *.foo {

       }

      }|}
      |xxx}];
    (* This shows that the behavior for '+' is currently identical to the behavior for
         '&', which while the behavior for + is correct, the behavior for & is incorrect.

         The bug was that & was printed as a delimeter which is wrong. The fix was to
         instead parse ampersands into their own thing.
    *)
    test
      [%expr
        {|
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
              let inline_class =
                Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_f18c7c7a1f|}
            end
        end in Ppx_css_anonymous_style__008_.inline_class
      Hoisted context:
      ----------------
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* _none_ */

      *.inline_class_hash_f18c7c7a1f {
       +*.foo *.foo {

       }

      }|}
      |xxx}]
  ;;

  let%expect_test "& :hover is distinct from &:hover" =
    test
      [%expr
        {|
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
              let inline_class =
                Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_215df9d72e|}
            end
        end in Ppx_css_anonymous_style__009_.inline_class
      Hoisted context:
      ----------------
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* _none_ */

      *.inline_class_hash_215df9d72e {
       *& *:hover {

       }

      }|}
      |xxx}];
    test
      [%expr
        {|
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
              let inline_class =
                Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_5f24d3547a|}
            end
        end in Ppx_css_anonymous_style__010_.inline_class
      Hoisted context:
      ----------------
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* _none_ */

      *.inline_class_hash_5f24d3547a {
       *&:hover {

       }

      }|}
      |xxx}]
  ;;

  let%expect_test "nested & within functions work" =
    test
      [%expr
        {|
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
              let inline_class =
                Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_e9e021c0ab|}
            end
        end in Ppx_css_anonymous_style__011_.inline_class
      Hoisted context:
      ----------------
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* _none_ */

      *.inline_class_hash_e9e021c0ab {
       *& *:has(&) {

       }

      }|}
      |xxx}]
  ;;

  let%expect_test "Nested functions allow for ident starting identifier" =
    test
      [%expr
        {|
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
              let inline_class =
                Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_3d84461335|}
            end
        end in Ppx_css_anonymous_style__012_.inline_class
      Hoisted context:
      ----------------
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* _none_ */

      *.inline_class_hash_3d84461335 {
       *& *:has(foo) {

       }

      }|}
      |xxx}]
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
              let inline_class =
                Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_afb54ad500|}
            end
        end in Ppx_css_anonymous_style__013_.inline_class
      Hoisted context:
      ----------------
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* _none_ */

      *.inline_class_hash_afb54ad500 {
       *& *:has(& *:has-*&+*&>*& *#foo>*& *.foo &:has(& *:has(& *.foo *&))) {

       }

      }|}
      |xxx}]
  ;;

  let%expect_test "different kinds of delimiters" =
    (* NOTE: This test is weird, but shows right behavior. ~,-,+,> are delimeters
         that are fine to remove the whitespace, but other identifiers are not
         fine to remove the white space. This output shows correct behavior to my
         understanding. *)
    test
      [%expr
        {|
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
              let inline_class =
                Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_881fcfebfc|}
            end
        end in Ppx_css_anonymous_style__014_.inline_class
      Hoisted context:
      ----------------
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* _none_ */

      *.inline_class_hash_881fcfebfc {
       *& *:has(foo)+*.bar+*&~*&>*&-*& *#foo {

       }

      }|}
      |xxx}]
  ;;

  let%expect_test "more complex & interactions" =
    test
      [%expr
        {|
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
              let inline_class =
                Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_011588f4ed|}
            end
        end in Ppx_css_anonymous_style__015_.inline_class
      Hoisted context:
      ----------------
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* _none_ */

      *.inline_class_hash_011588f4ed {
       *& *:has(.foo),*&:hover {

       }

      }|}
      |xxx}]
  ;;
end

module%test [@name "stylesheet parsing tests"] _ = struct
  let test expr =
    catch_location_error ~f:(fun () ->
      let%tydi { txt = structure; ppx_css_string_expression; _ } =
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
    [%expect {xxx| Parse error while reading token '&' |xxx}]
  ;;

  let%expect_test "nested ampersand is allowed" =
    test [%expr stylesheet {|.foo { & {} } |}];
    [%expect
      {xxx|
      [@@@ocaml.warning "-32"]
      let __type_info_for_ppx_css :
        ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
        = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
      module type S  =
        sig
          module For_referencing : sig val foo : string end
          val foo : Virtual_dom.Vdom.Attr.t
        end
      type t = (module S)
      module Default : S =
        struct
          module For_referencing = struct let foo = {|foo_hash_400d77f021|} end
          let foo = Virtual_dom.Vdom.Attr.class_ {|foo_hash_400d77f021|}
        end
      include Default
      let default : t = (module Default)
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* _none_ */

      *.foo_hash_400d77f021 {
       *& {

       }

      }|}
      |xxx}]
  ;;

  let%expect_test "nested ampersand is allowed (all the way down)" =
    test [%expr stylesheet {|.foo { & { &.bar { &.baz { } } } } |}];
    [%expect
      {xxx|
      [@@@ocaml.warning "-32"]
      let __type_info_for_ppx_css :
        ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
        = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
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
              let foo = {|foo_hash_c8ffad4dfc|}
              let baz = {|baz_hash_c8ffad4dfc|}
              let bar = {|bar_hash_c8ffad4dfc|}
            end
          let foo = Virtual_dom.Vdom.Attr.class_ {|foo_hash_c8ffad4dfc|}
          let baz = Virtual_dom.Vdom.Attr.class_ {|baz_hash_c8ffad4dfc|}
          let bar = Virtual_dom.Vdom.Attr.class_ {|bar_hash_c8ffad4dfc|}
        end
      include Default
      let default : t = (module Default)
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* _none_ */

      *.foo_hash_c8ffad4dfc {
       *& {
        *&.bar_hash_c8ffad4dfc {
         *&.baz_hash_c8ffad4dfc {

         }

        }

       }

      }|}
      |xxx}]
  ;;
end
