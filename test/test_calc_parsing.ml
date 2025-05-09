open! Core
open Test_util
open Ppxlib

let loc = Test_util.loc_with_mock_name

let%expect_test "Bug, whitespace is required around '-'." =
  test_expression
    [%expr
      {|
    width: calc(100vw - %{width#Css_gen.Length});
  |}];
  (* This case is OK. *)
  [%expect
    {xxx|
    Expression context:
    -------------------
    let module Ppx_css_anonymous_style__004_ =
      struct
        include
          struct
            let ppx_css__internal_anonymous_variables__001_ =
              let ppx_css_temp_variable__003_ =
                (((Css_gen.Length.to_string_css width)[@merlin.focus ]) :
                string) in
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
                [({|--ppx_css_anonymous_var_1_hash_d6328e8b40|},
                   ppx_css_temp_variable__003_)]
            let foo__inline_class =
              ((Virtual_dom.Vdom.Attr.combine
                  (Virtual_dom.Vdom.Attr.class_
                     {|foo__inline_class_hash_d6328e8b40|})
                  ppx_css__internal_anonymous_variables__001_)
              [@merlin.focus ])
          end
      end in Ppx_css_anonymous_style__004_.foo__inline_class
    Hoisted context:
    ----------------
    let sheet_x__002___d6328e8b40__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__002___d6328e8b40__0
        {|
    /* app/foo/foo.ml */

    .foo__inline_class_hash_d6328e8b40 {
      width: calc(100vw - var(--ppx_css_anonymous_var_1_hash_d6328e8b40));
    }|}
    |xxx}];
  (* The bug here is that we are not setting the css variable for [width]. *)
  test_expression
    [%expr
      {|
    width: calc(100vw-%{width#Css_gen.Length});
  |}];
  [%expect
    {xxx|
    Expression context:
    -------------------
    let module Ppx_css_anonymous_style__007_ =
      struct
        include
          struct
            let ppx_css__internal_anonymous_variables__005_ =
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs []
            let foo__inline_class =
              ((Virtual_dom.Vdom.Attr.combine
                  (Virtual_dom.Vdom.Attr.class_
                     {|foo__inline_class_hash_72f96e86b0|})
                  ppx_css__internal_anonymous_variables__005_)
              [@merlin.focus ])
          end
      end in Ppx_css_anonymous_style__007_.foo__inline_class
    Hoisted context:
    ----------------
    let sheet_x__006___72f96e86b0__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__006___72f96e86b0__0
        {|
    /* app/foo/foo.ml */

    .foo__inline_class_hash_72f96e86b0 {
      width: calc(100vw-var(--ppx_css_anonymous_var_2));
    }|}
    |xxx}];
  test_expression
    [%expr
      {|
    width: calc(100vw -%{width#Css_gen.Length});
  |}];
  [%expect
    {xxx|
    Expression context:
    -------------------
    let module Ppx_css_anonymous_style__010_ =
      struct
        include
          struct
            let ppx_css__internal_anonymous_variables__008_ =
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs []
            let foo__inline_class =
              ((Virtual_dom.Vdom.Attr.combine
                  (Virtual_dom.Vdom.Attr.class_
                     {|foo__inline_class_hash_f3953363de|})
                  ppx_css__internal_anonymous_variables__008_)
              [@merlin.focus ])
          end
      end in Ppx_css_anonymous_style__010_.foo__inline_class
    Hoisted context:
    ----------------
    let sheet_x__009___f3953363de__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__009___f3953363de__0
        {|
    /* app/foo/foo.ml */

    .foo__inline_class_hash_f3953363de {
      width: calc(100vw -var(--ppx_css_anonymous_var_3));
    }|}
    |xxx}]
;;
