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
      let () =
        Ppx_css_syntax.Preprocess_arguments.set_lazy_loading_optimization (Some true)
      in
      let%tydi { txt = expression; hoisted_structure_items; _ } =
        expr |> For_testing.generate_inline_expression
      in
      print_heading "Expression context:";
      expression |> Pprintast.string_of_expression |> print_endline;
      print_heading "Hoisted context:";
      print_endline (Pprintast.string_of_structure hoisted_structure_items))
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
      let module Ppx_css_anonymous_style__002_ =
        struct
          include
            struct
              let inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__001___efb2142b4d__group_0;
                      Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_efb2142b4d|}))
            end
        end in Ppx_css_anonymous_style__002_.inline_class
      Hoisted context:
      ----------------
      let sheet_x__001___efb2142b4d__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__001___efb2142b4d__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__001___efb2142b4d__0
             {|
      /* _none_ */

      *.inline_class_hash_efb2142b4d {
       background-color:tomato;
       *&:hover {
        background-color:black
       }

      }|})
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
      let module Ppx_css_anonymous_style__004_ =
        struct
          include
            struct
              let inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__003___9eb1e9feb7__group_0;
                      Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_9eb1e9feb7|}))
            end
        end in Ppx_css_anonymous_style__004_.inline_class
      Hoisted context:
      ----------------
      let sheet_x__003___9eb1e9feb7__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__003___9eb1e9feb7__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__003___9eb1e9feb7__0
             {|
      /* _none_ */

      *.inline_class_hash_9eb1e9feb7 {
       background-color:tomato;
       *&:foo {

       }
       ;
       *&:bar {

       }

      }|})
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
      let module Ppx_css_anonymous_style__006_ =
        struct
          include
            struct
              let inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__005___9eb1e9feb7__group_0;
                      Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_9eb1e9feb7|}))
            end
        end in Ppx_css_anonymous_style__006_.inline_class
      Hoisted context:
      ----------------
      let sheet_x__005___9eb1e9feb7__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__005___9eb1e9feb7__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__005___9eb1e9feb7__0
             {|
      /* _none_ */

      *.inline_class_hash_9eb1e9feb7 {
       background-color:tomato;
       *&:foo {

       }
       ;
       *&:bar {

       }

      }|})
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
      let module Ppx_css_anonymous_style__008_ =
        struct
          include
            struct
              let inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__007___e220285b85__group_0;
                      Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_e220285b85|}))
            end
        end in Ppx_css_anonymous_style__008_.inline_class
      Hoisted context:
      ----------------
      let sheet_x__007___e220285b85__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__007___e220285b85__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__007___e220285b85__0
             {|
      /* _none_ */

      *.inline_class_hash_e220285b85 {
       background-color:tomato;
       *&:foo {

       }
       ;
       background-color:hotpink;
       *&:bar {

       }

      }|})
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
      let module Ppx_css_anonymous_style__010_ =
        struct
          include
            struct
              let inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__009___428cbc12a0__group_0;
                      Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_428cbc12a0|}))
            end
        end in Ppx_css_anonymous_style__010_.inline_class
      Hoisted context:
      ----------------
      let sheet_x__009___428cbc12a0__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__009___428cbc12a0__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__009___428cbc12a0__0
             {|
      /* _none_ */

      *.inline_class_hash_428cbc12a0 {
       *& * {

       }

      }|})
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
      let module Ppx_css_anonymous_style__012_ =
        struct
          include
            struct
              let inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__011___7071faf49d__group_0;
                      Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_7071faf49d|}))
            end
        end in Ppx_css_anonymous_style__012_.inline_class
      Hoisted context:
      ----------------
      let sheet_x__011___7071faf49d__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__011___7071faf49d__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__011___7071faf49d__0
             {|
      /* _none_ */

      *.inline_class_hash_7071faf49d {
       *& *& {

       }

      }|})
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
      let module Ppx_css_anonymous_style__014_ =
        struct
          include
            struct
              let inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__013___73fe818673__group_0;
                      Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_73fe818673|}))
            end
        end in Ppx_css_anonymous_style__014_.inline_class
      Hoisted context:
      ----------------
      let sheet_x__013___73fe818673__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__013___73fe818673__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__013___73fe818673__0
             {|
      /* _none_ */

      *.inline_class_hash_73fe818673 {
       *& *.foo *.foo {

       }

      }|})
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
      let module Ppx_css_anonymous_style__016_ =
        struct
          include
            struct
              let inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__015___9b5c2545de__group_0;
                      Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_9b5c2545de|}))
            end
        end in Ppx_css_anonymous_style__016_.inline_class
      Hoisted context:
      ----------------
      let sheet_x__015___9b5c2545de__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__015___9b5c2545de__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__015___9b5c2545de__0
             {|
      /* _none_ */

      *.inline_class_hash_9b5c2545de {
       +*.foo *.foo {

       }

      }|})
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
      let module Ppx_css_anonymous_style__018_ =
        struct
          include
            struct
              let inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__017___097c67c7c8__group_0;
                      Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_097c67c7c8|}))
            end
        end in Ppx_css_anonymous_style__018_.inline_class
      Hoisted context:
      ----------------
      let sheet_x__017___097c67c7c8__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__017___097c67c7c8__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__017___097c67c7c8__0
             {|
      /* _none_ */

      *.inline_class_hash_097c67c7c8 {
       *& *:hover {

       }

      }|})
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
      let module Ppx_css_anonymous_style__020_ =
        struct
          include
            struct
              let inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__019___cdc0762c8b__group_0;
                      Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_cdc0762c8b|}))
            end
        end in Ppx_css_anonymous_style__020_.inline_class
      Hoisted context:
      ----------------
      let sheet_x__019___cdc0762c8b__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__019___cdc0762c8b__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__019___cdc0762c8b__0
             {|
      /* _none_ */

      *.inline_class_hash_cdc0762c8b {
       *&:hover {

       }

      }|})
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
      let module Ppx_css_anonymous_style__022_ =
        struct
          include
            struct
              let inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__021___b788b580c1__group_0;
                      Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_b788b580c1|}))
            end
        end in Ppx_css_anonymous_style__022_.inline_class
      Hoisted context:
      ----------------
      let sheet_x__021___b788b580c1__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__021___b788b580c1__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__021___b788b580c1__0
             {|
      /* _none_ */

      *.inline_class_hash_b788b580c1 {
       *& *:has(&) {

       }

      }|})
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
      let module Ppx_css_anonymous_style__024_ =
        struct
          include
            struct
              let inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__023___7b85d320af__group_0;
                      Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_7b85d320af|}))
            end
        end in Ppx_css_anonymous_style__024_.inline_class
      Hoisted context:
      ----------------
      let sheet_x__023___7b85d320af__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__023___7b85d320af__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__023___7b85d320af__0
             {|
      /* _none_ */

      *.inline_class_hash_7b85d320af {
       *& *:has(foo) {

       }

      }|})
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
      let module Ppx_css_anonymous_style__026_ =
        struct
          include
            struct
              let inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__025___772b62a264__group_0;
                      Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_772b62a264|}))
            end
        end in Ppx_css_anonymous_style__026_.inline_class
      Hoisted context:
      ----------------
      let sheet_x__025___772b62a264__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__025___772b62a264__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__025___772b62a264__0
             {|
      /* _none_ */

      *.inline_class_hash_772b62a264 {
       *& *:has(& *:has-*&+*&>*& *#foo>*& *.foo &:has(& *:has(& *.foo *&))) {

       }

      }|})
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
      let module Ppx_css_anonymous_style__028_ =
        struct
          include
            struct
              let inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__027___e521bf186c__group_0;
                      Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_e521bf186c|}))
            end
        end in Ppx_css_anonymous_style__028_.inline_class
      Hoisted context:
      ----------------
      let sheet_x__027___e521bf186c__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__027___e521bf186c__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__027___e521bf186c__0
             {|
      /* _none_ */

      *.inline_class_hash_e521bf186c {
       *& *:has(foo)+*.bar+*&~*&>*&-*& *#foo {

       }

      }|})
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
      let module Ppx_css_anonymous_style__030_ =
        struct
          include
            struct
              let inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__029___4cf7a52354__group_0;
                      Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_4cf7a52354|}))
            end
        end in Ppx_css_anonymous_style__030_.inline_class
      Hoisted context:
      ----------------
      let sheet_x__029___4cf7a52354__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__029___4cf7a52354__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__029___4cf7a52354__0
             {|
      /* _none_ */

      *.inline_class_hash_4cf7a52354 {
       *& *:has(.foo),*&:hover {

       }

      }|})
      |xxx}]
  ;;
end

module%test [@name "stylesheet parsing tests"] _ = struct
  let test expr =
    catch_location_error ~f:(fun () ->
      let%tydi { txt = structure; hoisted_structure_items; _ } =
        For_testing.generate_struct expr
      in
      let structure = structure @ hoisted_structure_items in
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
          module For_referencing = struct let foo = {|foo_hash_9c4935ac11|} end
          let foo =
            Virtual_dom.Vdom.Attr.lazy_
              (lazy
                 (Inline_css.Ppx_css_runtime.force
                    Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__031___9c4935ac11__group_0;
                  Virtual_dom.Vdom.Attr.class_ {|foo_hash_9c4935ac11|}))
        end
      include Default
      let default : t = (module Default)
      let sheet_x__031___9c4935ac11__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__031___9c4935ac11__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__031___9c4935ac11__0
             {|
      /* _none_ */

      *.foo_hash_9c4935ac11 {
       *& {

       }

      }|})
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
              let bar = {|bar_hash_3fb1a12a14|}
              let baz = {|baz_hash_3fb1a12a14|}
              let foo = {|foo_hash_3fb1a12a14|}
            end
          let bar =
            Virtual_dom.Vdom.Attr.lazy_
              (lazy
                 (Inline_css.Ppx_css_runtime.force
                    Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__032___3fb1a12a14__group_0;
                  Virtual_dom.Vdom.Attr.class_ {|bar_hash_3fb1a12a14|}))
          let baz =
            Virtual_dom.Vdom.Attr.lazy_
              (lazy
                 (Inline_css.Ppx_css_runtime.force
                    Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__032___3fb1a12a14__group_0;
                  Virtual_dom.Vdom.Attr.class_ {|baz_hash_3fb1a12a14|}))
          let foo =
            Virtual_dom.Vdom.Attr.lazy_
              (lazy
                 (Inline_css.Ppx_css_runtime.force
                    Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__032___3fb1a12a14__group_0;
                  Virtual_dom.Vdom.Attr.class_ {|foo_hash_3fb1a12a14|}))
        end
      include Default
      let default : t = (module Default)
      let sheet_x__032___3fb1a12a14__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__032___3fb1a12a14__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__032___3fb1a12a14__0
             {|
      /* _none_ */

      *.foo_hash_3fb1a12a14 {
       *& {
        *&.bar_hash_3fb1a12a14 {
         *&.baz_hash_3fb1a12a14 {

         }

        }

       }

      }|})
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
              let bar = {|bar_hash_3fb1a12a14|}
              let baz = {|baz_hash_3fb1a12a14|}
              let foo = {|foo_hash_3fb1a12a14|}
            end
          let bar =
            Virtual_dom.Vdom.Attr.lazy_
              (lazy
                 (Inline_css.Ppx_css_runtime.force
                    Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__033___3fb1a12a14__group_0;
                  Virtual_dom.Vdom.Attr.class_ {|bar_hash_3fb1a12a14|}))
          let baz =
            Virtual_dom.Vdom.Attr.lazy_
              (lazy
                 (Inline_css.Ppx_css_runtime.force
                    Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__033___3fb1a12a14__group_0;
                  Virtual_dom.Vdom.Attr.class_ {|baz_hash_3fb1a12a14|}))
          let foo =
            Virtual_dom.Vdom.Attr.lazy_
              (lazy
                 (Inline_css.Ppx_css_runtime.force
                    Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__033___3fb1a12a14__group_0;
                  Virtual_dom.Vdom.Attr.class_ {|foo_hash_3fb1a12a14|}))
        end
      include Default
      let default : t = (module Default)
      let sheet_x__033___3fb1a12a14__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__033___3fb1a12a14__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__033___3fb1a12a14__0
             {|
      /* _none_ */

      *.foo_hash_3fb1a12a14 {
       *& {
        *&.bar_hash_3fb1a12a14 {
         *&.baz_hash_3fb1a12a14 {

         }

        }

       }

      }|})
      |xxx}]
  ;;
end
