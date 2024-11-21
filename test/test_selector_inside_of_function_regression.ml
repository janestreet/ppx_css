open! Core
open Ppxlib

(* This test is a regression test for a bug on ppx_css where selectors inside of selector
   functions were altered incorrectly. *)

let loc = Location.none

let%expect_test "Selectors inside selector functions were incorrectly transformed." =
  Test_util.test_struct [%expr stylesheet {|:has(.a .b) {}|}];
  (* The bug was that the space in between the selectors is ignored, turning `.a .b` into
     `.a.b` - changing behavior. *)
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let __type_info_for_ppx_css :
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
    module type S  =
      sig
        module For_referencing : sig val a : string val b : string end
        val a : Virtual_dom.Vdom.Attr.t
        val b : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing =
          struct let a = {|a_hash_f40d9e8892|}
                 let b = {|b_hash_f40d9e8892|} end
        let a =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__001___f40d9e8892__group_0;
                Virtual_dom.Vdom.Attr.class_ {|a_hash_f40d9e8892|}))
        let b =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__001___f40d9e8892__group_0;
                Virtual_dom.Vdom.Attr.class_ {|b_hash_f40d9e8892|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__001___f40d9e8892__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__001___f40d9e8892__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__001___f40d9e8892__0
           {|
    /* _none_ */

    *:has(.a_hash_f40d9e8892 *.b_hash_f40d9e8892) {

    }|})
    |xxx}];
  (* Nested CSS was always fine - only the top-level selector in this situation was affected. *)
  Test_util.test_struct [%expr stylesheet {|:has(.a .b) { :has(.a .b) { } }|}];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let __type_info_for_ppx_css :
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
    module type S  =
      sig
        module For_referencing : sig val a : string val b : string end
        val a : Virtual_dom.Vdom.Attr.t
        val b : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing =
          struct let a = {|a_hash_a3450c6ec7|}
                 let b = {|b_hash_a3450c6ec7|} end
        let a =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__002___a3450c6ec7__group_0;
                Virtual_dom.Vdom.Attr.class_ {|a_hash_a3450c6ec7|}))
        let b =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__002___a3450c6ec7__group_0;
                Virtual_dom.Vdom.Attr.class_ {|b_hash_a3450c6ec7|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__002___a3450c6ec7__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__002___a3450c6ec7__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__002___a3450c6ec7__0
           {|
    /* _none_ */

    *:has(.a_hash_a3450c6ec7 *.b_hash_a3450c6ec7) {
     *:has(.a_hash_a3450c6ec7 *.b_hash_a3450c6ec7) {

     }

    }|})
    |xxx}]
;;

let%expect_test "Selectors inside of styled components - seem fine" =
  Test_util.test_expression [%expr {| & { :has(.a .b) {} } |}];
  (* Nested CSS in the styled component syntax is fine. *)
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
                      Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__003___f3dff972a4__group_0;
                    Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_f3dff972a4|}))
          end
      end in Ppx_css_anonymous_style__004_.inline_class
    Hoisted context:
    ----------------
    let sheet_x__003___f3dff972a4__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__003___f3dff972a4__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__003___f3dff972a4__0
           {|
    /* _none_ */

    *.inline_class_hash_f3dff972a4 {
     *& {
      *:has(.a *.b) {

      }

     }

    }|})
    |xxx}]
;;

let test s =
  print_endline (Css_jane.Stylesheet.to_string_hum (Css_jane.Stylesheet.of_string s))
;;

(* This is the same test suite as above, but showing that the bug is present at the
   css-parser level. *)
module%test Parser_test = struct
  let%expect_test "Spaces inside of selector are ignored" =
    test {|:has(.a .b) {}|};
    [%expect
      {|
      *:has(.a *.b) {

      }
      |}];
    test {|:has(.a .b) { :has(.c .d) { } }|};
    [%expect
      {|
      *:has(.a *.b) {
       *:has(.c *.d) {

       }

      }
      |}]
  ;;

  let%expect_test "more complex selector" =
    test {|:foo(:has(.a .b) + .c > .d:hover .e + .f  .g.h) {}|};
    [%expect
      {|
      *:foo(:has(.a.b)+.c>.d:hover.e+.f.g.h) {

      }
      |}];
    test {|:not(:has(.a .b) + .c > .d:hover .e + .f  .g.h) {}|};
    [%expect
      {|
      *:not(:has(.a *.b)+*.c>*.d:hover *.e+*.f *.g.h) {

      }
      |}]
  ;;

  let%expect_test "Known selectors that parse differently" =
    test
      {|
    :dir(ltr) {} :lang(en) {} :state(checked) {}
    :dir(.a .b) {} :lang(.a .b) {} :state(.a .b) {}
    |};
    (* These do not add the '*'. *)
    [%expect
      {|
      *:dir(ltr) {

      }

      *:lang(en) {

      }

      *:state(checked) {

      }

      *:dir(.a.b) {

      }

      *:lang(.a.b) {

      }

      *:state(.a.b) {

      }
      |}]
  ;;

  let%expect_test "Unknown selector functions do not respect whitespace" =
    test {|:foo(.a .b) {}|};
    [%expect
      {|
      *:foo(.a.b) {

      }
      |}]
  ;;

  let%expect_test "Known selectors that parse as subselectors" =
    test
      {|
    :has(.a .b) {}
    :is(.a .b) {}
    :where(.a .b) {}
    :not(.a .b) {}
    :host(.a .b) {}
    :host-context(.a .b) {}
    |};
    [%expect
      {|
      *:has(.a *.b) {

      }

      *:is(.a *.b) {

      }

      *:where(.a *.b) {

      }

      *:not(.a *.b) {

      }

      *:host(.a *.b) {

      }

      *:host-context(.a *.b) {

      }
      |}]
  ;;

  let%expect_test "WEIRD: paren block + bracket blocks" =
    test {|(.a .b) {}|};
    [%expect
      {|
      (.a.b) {

      }
      |}];
    test {|[ .a .b ] {}|};
    [%expect
      {|
      [.a.b] {

      }
      |}]
  ;;
end

module%test Actual_bug_in_handling_of_scope = struct
  let%expect_test "Scope selectors in at-rules have their whitespaces removed" =
    test {| @scope (.a .b) to (figure) { } |};
    [%expect
      {|
      @scope (.a.b) to (figure){

      }
      |}]
  ;;

  let%expect_test "nth-child is broken" =
    test {| div:nth-child(2n + 1) {} |};
    [%expect
      {|
      div:nth-child(2 n+1) {

      }
      |}]
  ;;
end
