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
                {|ppx_css_anonymous_class_hash_6f540be812|}
          end
      end in Ppx_css_anonymous_style__001_.ppx_css_anonymous_class
    Hoisted context:
    ----------------
    let () =
      Inline_css.Private.append
        {|
    /* _none_ */

    *.ppx_css_anonymous_class_hash_6f540be812 {
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
                {|ppx_css_anonymous_class_hash_9ac3741b10|}
          end
      end in Ppx_css_anonymous_style__002_.ppx_css_anonymous_class
    Hoisted context:
    ----------------
    let () =
      Inline_css.Private.append
        {|
    /* _none_ */

    *.ppx_css_anonymous_class_hash_9ac3741b10 {
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
                {|ppx_css_anonymous_class_hash_9ac3741b10|}
          end
      end in Ppx_css_anonymous_style__003_.ppx_css_anonymous_class
    Hoisted context:
    ----------------
    let () =
      Inline_css.Private.append
        {|
    /* _none_ */

    *.ppx_css_anonymous_class_hash_9ac3741b10 {
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
                {|ppx_css_anonymous_class_hash_c80b17e59f|}
          end
      end in Ppx_css_anonymous_style__004_.ppx_css_anonymous_class
    Hoisted context:
    ----------------
    let () =
      Inline_css.Private.append
        {|
    /* _none_ */

    *.ppx_css_anonymous_class_hash_c80b17e59f {
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
        let (__type_info_for_ppx_css :
          ?rewrite:(string * string) list ->
            ?dont_hash:string list ->
              ?dont_hash_prefixes:string list -> string -> unit)
          = fun ?rewrite:_ ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
        module type S  =
          sig
            module For_referencing : sig val foo : string end
            val foo : Virtual_dom.Vdom.Attr.t
          end
        type t = (module S)
        module Default : S =
          struct
            module For_referencing = struct let foo = {|foo_hash_d9e32c54d2|} end
            let foo = Virtual_dom.Vdom.Attr.class_ {|foo_hash_d9e32c54d2|}
          end
        include Default
        let default : t = (module Default)
        let () =
          Inline_css.Private.append
            {|
        /* _none_ */

        *.foo_hash_d9e32c54d2 {
         *& {

         }

        }|} |xxx}]
    ;;

    let%expect_test "nested ampersand is allowed (all the way down)" =
      test [%expr stylesheet {|.foo { & { &.bar { &.baz { } } } } |}];
      [%expect
        {xxx|
      [@@@ocaml.warning "-32"]
      let (__type_info_for_ppx_css :
        ?rewrite:(string * string) list ->
          ?dont_hash:string list ->
            ?dont_hash_prefixes:string list -> string -> unit)
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
              let foo = {|foo_hash_5834a7cd4a|}
              let baz = {|baz_hash_5834a7cd4a|}
              let bar = {|bar_hash_5834a7cd4a|}
            end
          let foo = Virtual_dom.Vdom.Attr.class_ {|foo_hash_5834a7cd4a|}
          let baz = Virtual_dom.Vdom.Attr.class_ {|baz_hash_5834a7cd4a|}
          let bar = Virtual_dom.Vdom.Attr.class_ {|bar_hash_5834a7cd4a|}
        end
      include Default
      let default : t = (module Default)
      let () =
        Inline_css.Private.append
          {|
      /* _none_ */

      *.foo_hash_5834a7cd4a {
       *& {
        *&.bar_hash_5834a7cd4a {
         *&.baz_hash_5834a7cd4a {

         }

        }

       }

      }|} |xxx}]
    ;;
  end)
;;
