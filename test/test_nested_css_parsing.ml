open! Core
open Ppxlib
open Ppx_css

let loc = Test_util.loc_with_mock_name
let () = Ppx_css_syntax.Preprocess_arguments.set_lazy_loading_optimization (Some true)

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
        expr |> For_testing.generate_inline_expression ~loc ~disable_hashing:false
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
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__001___c128c9497d__group_0;
                      Virtual_dom.Vdom.Attr.class_
                        {|foo__inline_class_hash_c128c9497d|}))
            end
        end in Ppx_css_anonymous_style__002_.foo__inline_class
      Hoisted context:
      ----------------
      let sheet_x__001___c128c9497d__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__001___c128c9497d__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__001___c128c9497d__0
             {|
      /* app/foo/foo.ml */

      .foo__inline_class_hash_c128c9497d {
        background-color: tomato;
        &:hover {
          background-color: black;
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
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__003___73250f0682__group_0;
                      Virtual_dom.Vdom.Attr.class_
                        {|foo__inline_class_hash_73250f0682|}))
            end
        end in Ppx_css_anonymous_style__004_.foo__inline_class
      Hoisted context:
      ----------------
      let sheet_x__003___73250f0682__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__003___73250f0682__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__003___73250f0682__0
             {|
      /* app/foo/foo.ml */

      .foo__inline_class_hash_73250f0682 {
        background-color: tomato;
        &:foo {
        }
        &:bar {
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
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__005___fc9c10ff80__group_0;
                      Virtual_dom.Vdom.Attr.class_
                        {|foo__inline_class_hash_fc9c10ff80|}))
            end
        end in Ppx_css_anonymous_style__006_.foo__inline_class
      Hoisted context:
      ----------------
      let sheet_x__005___fc9c10ff80__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__005___fc9c10ff80__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__005___fc9c10ff80__0
             {|
      /* app/foo/foo.ml */

      .foo__inline_class_hash_fc9c10ff80 {
        background-color: tomato;
        &:foo {
        }
        &:bar {
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
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__007___376eb61af5__group_0;
                      Virtual_dom.Vdom.Attr.class_
                        {|foo__inline_class_hash_376eb61af5|}))
            end
        end in Ppx_css_anonymous_style__008_.foo__inline_class
      Hoisted context:
      ----------------
      let sheet_x__007___376eb61af5__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__007___376eb61af5__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__007___376eb61af5__0
             {|
      /* app/foo/foo.ml */

      .foo__inline_class_hash_376eb61af5 {
        background-color: tomato;
        &:foo {
        }
        background-color: hotpink;
        &:bar {
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
    [%expect {xxx| Declaration must end with a semicolon. |xxx}]
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
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__009___90cbcaaec0__group_0;
                      Virtual_dom.Vdom.Attr.class_
                        {|foo__inline_class_hash_90cbcaaec0|}))
            end
        end in Ppx_css_anonymous_style__010_.foo__inline_class
      Hoisted context:
      ----------------
      let sheet_x__009___90cbcaaec0__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__009___90cbcaaec0__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__009___90cbcaaec0__0
             {|
      /* app/foo/foo.ml */

      .foo__inline_class_hash_90cbcaaec0 {
        & * {
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
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__011___7f3c176a13__group_0;
                      Virtual_dom.Vdom.Attr.class_
                        {|foo__inline_class_hash_7f3c176a13|}))
            end
        end in Ppx_css_anonymous_style__012_.foo__inline_class
      Hoisted context:
      ----------------
      let sheet_x__011___7f3c176a13__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__011___7f3c176a13__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__011___7f3c176a13__0
             {|
      /* app/foo/foo.ml */

      .foo__inline_class_hash_7f3c176a13 {
        & & {
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
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__013___5ddb2fd957__group_0;
                      Virtual_dom.Vdom.Attr.class_
                        {|foo__inline_class_hash_5ddb2fd957|}))
            end
        end in Ppx_css_anonymous_style__014_.foo__inline_class
      Hoisted context:
      ----------------
      let sheet_x__013___5ddb2fd957__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__013___5ddb2fd957__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__013___5ddb2fd957__0
             {|
      /* app/foo/foo.ml */

      .foo__inline_class_hash_5ddb2fd957 {
        & .foo .foo {
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
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__015___d5872c5f9a__group_0;
                      Virtual_dom.Vdom.Attr.class_
                        {|foo__inline_class_hash_d5872c5f9a|}))
            end
        end in Ppx_css_anonymous_style__016_.foo__inline_class
      Hoisted context:
      ----------------
      let sheet_x__015___d5872c5f9a__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__015___d5872c5f9a__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__015___d5872c5f9a__0
             {|
      /* app/foo/foo.ml */

      .foo__inline_class_hash_d5872c5f9a {
        + .foo .foo {
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
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__017___6ac0ce3144__group_0;
                      Virtual_dom.Vdom.Attr.class_
                        {|foo__inline_class_hash_6ac0ce3144|}))
            end
        end in Ppx_css_anonymous_style__018_.foo__inline_class
      Hoisted context:
      ----------------
      let sheet_x__017___6ac0ce3144__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__017___6ac0ce3144__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__017___6ac0ce3144__0
             {|
      /* app/foo/foo.ml */

      .foo__inline_class_hash_6ac0ce3144 {
        & :hover {
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
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__019___25407932d4__group_0;
                      Virtual_dom.Vdom.Attr.class_
                        {|foo__inline_class_hash_25407932d4|}))
            end
        end in Ppx_css_anonymous_style__020_.foo__inline_class
      Hoisted context:
      ----------------
      let sheet_x__019___25407932d4__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__019___25407932d4__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__019___25407932d4__0
             {|
      /* app/foo/foo.ml */

      .foo__inline_class_hash_25407932d4 {
        &:hover {
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
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__021___fa8d487308__group_0;
                      Virtual_dom.Vdom.Attr.class_
                        {|foo__inline_class_hash_fa8d487308|}))
            end
        end in Ppx_css_anonymous_style__022_.foo__inline_class
      Hoisted context:
      ----------------
      let sheet_x__021___fa8d487308__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__021___fa8d487308__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__021___fa8d487308__0
             {|
      /* app/foo/foo.ml */

      .foo__inline_class_hash_fa8d487308 {
        & :has(&) {
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
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__023___12b7200694__group_0;
                      Virtual_dom.Vdom.Attr.class_
                        {|foo__inline_class_hash_12b7200694|}))
            end
        end in Ppx_css_anonymous_style__024_.foo__inline_class
      Hoisted context:
      ----------------
      let sheet_x__023___12b7200694__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__023___12b7200694__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__023___12b7200694__0
             {|
      /* app/foo/foo.ml */

      .foo__inline_class_hash_12b7200694 {
        & :has(foo) {
        }
      }|})
      |xxx}]
  ;;

  let%expect_test "Complex nested functions" =
    test
      [%expr
        {|
        & :has(& :has ~ & + & > & #foo> & .foo&:has(& :has(& .foo &))) {

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
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__025___0e3138c57e__group_0;
                      Virtual_dom.Vdom.Attr.class_
                        {|foo__inline_class_hash_0e3138c57e|}))
            end
        end in Ppx_css_anonymous_style__026_.foo__inline_class
      Hoisted context:
      ----------------
      let sheet_x__025___0e3138c57e__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__025___0e3138c57e__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__025___0e3138c57e__0
             {|
      /* app/foo/foo.ml */

      .foo__inline_class_hash_0e3138c57e {
        & :has(& :has ~ & + & > & #foo > & .foo&:has(& :has(& .foo &))) {
        }
      }|})
      |xxx}]
  ;;

  let%expect_test "different kinds of delimiters" =
    (* NOTE: This test is weird, but shows right behavior. ~,-,+,> are delimeters that are
       fine to remove the whitespace, but other identifiers are not fine to remove the
       white space. This output shows correct behavior to my understanding. *)
    test
      [%expr
        {|
        & :has(foo) + .bar + & ~ & > & + & #foo {

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
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__027___a53de17d04__group_0;
                      Virtual_dom.Vdom.Attr.class_
                        {|foo__inline_class_hash_a53de17d04|}))
            end
        end in Ppx_css_anonymous_style__028_.foo__inline_class
      Hoisted context:
      ----------------
      let sheet_x__027___a53de17d04__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__027___a53de17d04__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__027___a53de17d04__0
             {|
      /* app/foo/foo.ml */

      .foo__inline_class_hash_a53de17d04 {
        & :has(foo) + .bar + & ~ & > & + & #foo {
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
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__029___c5b177d83e__group_0;
                      Virtual_dom.Vdom.Attr.class_
                        {|foo__inline_class_hash_c5b177d83e|}))
            end
        end in Ppx_css_anonymous_style__030_.foo__inline_class
      Hoisted context:
      ----------------
      let sheet_x__029___c5b177d83e__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__029___c5b177d83e__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__029___c5b177d83e__0
             {|
      /* app/foo/foo.ml */

      .foo__inline_class_hash_c5b177d83e {
        & :has(.foo), &:hover {
        }
      }|})
      |xxx}]
  ;;
end

module%test [@name "stylesheet parsing tests"] _ = struct
  let test expr =
    catch_location_error ~f:(fun () ->
      let%tydi { txt = structure; hoisted_structure_items; _ } =
        For_testing.generate_struct ~loc ~disable_hashing:false expr
      in
      let structure = structure @ hoisted_structure_items in
      print_endline (Pprintast.string_of_structure structure))
  ;;

  let%expect_test "top-level ampersand is allowed" =
    test [%expr stylesheet {| & {} |}];
    [%expect
      {xxx|
      [@@@ocaml.warning "-32"]
      let __type_info_for_ppx_css :
        ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
        = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
      module type S  = sig module For_referencing : sig  end end
      type t = (module S)
      module Default : S = struct module For_referencing = struct  end end
      include Default
      let default : t = (module Default)
      let sheet_x__031___9be7bfaafa__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let () =
        Inline_css.Private.update_stylesheet sheet_x__031___9be7bfaafa__0
          {|
      /* app/foo/foo.ml */

      & {
      }|}
      |xxx}]
  ;;

  let%expect_test "nested ampersand is allowed" =
    test [%expr stylesheet {|.foo { & {} } |}];
    [%expect
      {xxx|
      [@@@ocaml.warning "-32"]
      let __type_info_for_ppx_css :
        ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
        = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
      module type S  =
        sig
          module For_referencing : sig val foo : string end
          val foo : Virtual_dom.Vdom.Attr.t
        end
      type t = (module S)
      module Default : S =
        struct
          module For_referencing = struct let foo = {|foo_hash_e9a6abc84b|} end
          let foo =
            Virtual_dom.Vdom.Attr.lazy_
              (lazy
                 (Inline_css.Ppx_css_runtime.force
                    Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__032___e9a6abc84b__group_0;
                  Virtual_dom.Vdom.Attr.class_ {|foo_hash_e9a6abc84b|}))
        end
      include Default
      let default : t = (module Default)
      let sheet_x__032___e9a6abc84b__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__032___e9a6abc84b__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__032___e9a6abc84b__0
             {|
      /* app/foo/foo.ml */

      .foo_hash_e9a6abc84b {
        & {
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
        = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
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
              let bar = {|bar_hash_04d8734ca2|}
              let baz = {|baz_hash_04d8734ca2|}
              let foo = {|foo_hash_04d8734ca2|}
            end
          let bar =
            Virtual_dom.Vdom.Attr.lazy_
              (lazy
                 (Inline_css.Ppx_css_runtime.force
                    Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__033___04d8734ca2__group_0;
                  Virtual_dom.Vdom.Attr.class_ {|bar_hash_04d8734ca2|}))
          let baz =
            Virtual_dom.Vdom.Attr.lazy_
              (lazy
                 (Inline_css.Ppx_css_runtime.force
                    Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__033___04d8734ca2__group_0;
                  Virtual_dom.Vdom.Attr.class_ {|baz_hash_04d8734ca2|}))
          let foo =
            Virtual_dom.Vdom.Attr.lazy_
              (lazy
                 (Inline_css.Ppx_css_runtime.force
                    Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__033___04d8734ca2__group_0;
                  Virtual_dom.Vdom.Attr.class_ {|foo_hash_04d8734ca2|}))
        end
      include Default
      let default : t = (module Default)
      let sheet_x__033___04d8734ca2__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__033___04d8734ca2__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__033___04d8734ca2__0
             {|
      /* app/foo/foo.ml */

      .foo_hash_04d8734ca2 {
        & {
          &.bar_hash_04d8734ca2 {
            &.baz_hash_04d8734ca2 {
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
        = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
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
              let bar = {|bar_hash_04d8734ca2|}
              let baz = {|baz_hash_04d8734ca2|}
              let foo = {|foo_hash_04d8734ca2|}
            end
          let bar =
            Virtual_dom.Vdom.Attr.lazy_
              (lazy
                 (Inline_css.Ppx_css_runtime.force
                    Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__034___04d8734ca2__group_0;
                  Virtual_dom.Vdom.Attr.class_ {|bar_hash_04d8734ca2|}))
          let baz =
            Virtual_dom.Vdom.Attr.lazy_
              (lazy
                 (Inline_css.Ppx_css_runtime.force
                    Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__034___04d8734ca2__group_0;
                  Virtual_dom.Vdom.Attr.class_ {|baz_hash_04d8734ca2|}))
          let foo =
            Virtual_dom.Vdom.Attr.lazy_
              (lazy
                 (Inline_css.Ppx_css_runtime.force
                    Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__034___04d8734ca2__group_0;
                  Virtual_dom.Vdom.Attr.class_ {|foo_hash_04d8734ca2|}))
        end
      include Default
      let default : t = (module Default)
      let sheet_x__034___04d8734ca2__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__034___04d8734ca2__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__034___04d8734ca2__0
             {|
      /* app/foo/foo.ml */

      .foo_hash_04d8734ca2 {
        & {
          &.bar_hash_04d8734ca2 {
            &.baz_hash_04d8734ca2 {
            }
          }
        }
      }|})
      |xxx}]
  ;;
end
