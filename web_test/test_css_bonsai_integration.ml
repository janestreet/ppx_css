open! Core
open! Bonsai_web.Proc
open! Bonsai_web_test

let%expect_test "css censoring" =
  let module Style =
    [%css
    stylesheet
      {|
        .foo {
        }
        |}]
  in
  let handle =
    Handle.create
      (Result_spec.vdom Fn.id)
      (Bonsai.const (Vdom.Node.div ~attrs:[ Style.foo ] []))
  in
  Handle.show handle;
  [%expect {| <div class="foo_hash_replaced_in_test"> </div> |}];
  let handle =
    Handle.create
      (Result_spec.vdom ~censor_hash:false Fn.id)
      (Bonsai.const (Vdom.Node.div ~attrs:[ Style.foo ] []))
  in
  Handle.show handle;
  [%expect {| <div class="foo_hash_791cc55311"> </div> |}]
;;

module%test [@name "inline css"] _ = struct
  let test attr =
    let handle =
      Handle.create
        (Result_spec.vdom Fn.id)
        (Bonsai.const (Vdom.Node.div ~attrs:[ attr ] []))
    in
    Handle.show handle
  ;;

  let%expect_test "normal case" =
    test [%css {|background-color: blue;|}];
    [%expect
      {| <div class="test_css_bonsai_integration__inline_class_hash_replaced_in_test"> </div> |}]
  ;;

  let%expect_test "single variable interpolation" =
    let color = "tomato" in
    test [%css {|background-color: %{color};|}];
    [%expect
      {|
      <div class="test_css_bonsai_integration__inline_class_hash_replaced_in_test"
           custom-css-vars=((--ppx_css_anonymous_var_1_hash_replaced_in_test tomato))> </div>
      |}]
  ;;

  let%expect_test "single variable with module interpolation" =
    let color = `Hex "#ff0000" in
    test [%css {|background-color: %{color#Css_gen.Color};|}];
    [%expect
      {|
      <div class="test_css_bonsai_integration__inline_class_hash_replaced_in_test"
           custom-css-vars=((--ppx_css_anonymous_var_2_hash_replaced_in_test #ff0000))> </div>
      |}]
  ;;

  let%expect_test "user variables _and_ interpolation variables" =
    let color = `Hex "#ff0000" in
    test
      [%css
        {|
          background-color: %{color#Css_gen.Color};
          background-color: var(--background-color1);
        |}];
    [%expect
      {|
      <div class="test_css_bonsai_integration__inline_class_hash_replaced_in_test"
           custom-css-vars=((--ppx_css_anonymous_var_3_hash_replaced_in_test #ff0000))> </div>
      |}]
  ;;

  let%expect_test "expression with side effects" =
    let color name =
      print_endline name;
      `Hex "#ff0000"
    in
    test
      [%css
        {|
          background-color: %{color "first"#Css_gen.Color};
          background-color: %{color "second"#Css_gen.Color};
          background-color: %{color "third"#Css_gen.Color};
          background-color: %{color "fourth"#Css_gen.Color};
          background-color: %{color "fifth"#Css_gen.Color};
          background-color: %{color "sixth"#Css_gen.Color};
        |}];
    (* side effects occur in the order that they pop-up on-screen. *)
    [%expect
      {|
      first
      second
      third
      fourth
      fifth
      sixth
      <div class="test_css_bonsai_integration__inline_class_hash_replaced_in_test"
           custom-css-vars=((--ppx_css_anonymous_var_4_hash_replaced_in_test #ff0000)(--ppx_css_anonymous_var_5_hash_replaced_in_test #ff0000)(--ppx_css_anonymous_var_6_hash_replaced_in_test #ff0000)(--ppx_css_anonymous_var_7_hash_replaced_in_test #ff0000)(--ppx_css_anonymous_var_8_hash_replaced_in_test #ff0000)(--ppx_css_anonymous_var_9_hash_replaced_in_test #ff0000))> </div>
      |}]
  ;;
end

module%test [@name "interpolated stylesheet css"] _ = struct
  let test attr =
    let handle =
      Handle.create
        (Result_spec.vdom Fn.id)
        (Bonsai.const (Vdom.Node.div ~attrs:[ attr ] []))
    in
    Handle.show handle
  ;;

  let%expect_test "single variable" =
    let color = "tomato" in
    let module Style =
      [%css
      stylesheet
        {|
          .foo {
            background-color: %{color};
          }
          |}]
    in
    test Style.foo;
    [%expect
      {|
      <div class="foo_hash_replaced_in_test"
           custom-css-vars=((--ppx_css_anonymous_var_10_hash_replaced_in_test tomato))> </div>
      |}]
  ;;

  let%expect_test "many variable interpolation" =
    let color = "tomato" in
    let color2 = "hotpink" in
    let module Style =
      [%css
      stylesheet
        {|
          .foo {
            background-color: %{color};
          }

          .foo.bar.baz {
            background-color: %{color2};
          }
          |}]
    in
    test (Vdom.Attr.many [ Style.foo; Style.bar; Style.baz ]);
    [%expect
      {|
      <div class="bar_hash_replaced_in_test baz_hash_replaced_in_test foo_hash_replaced_in_test"
           custom-css-vars=((--ppx_css_anonymous_var_11_hash_replaced_in_test tomato)(--ppx_css_anonymous_var_12_hash_replaced_in_test hotpink)(--ppx_css_anonymous_var_11_hash_replaced_in_test tomato)(--ppx_css_anonymous_var_12_hash_replaced_in_test hotpink)(--ppx_css_anonymous_var_11_hash_replaced_in_test tomato)(--ppx_css_anonymous_var_12_hash_replaced_in_test hotpink))> </div>
      |}]
  ;;

  let%expect_test "many variable interpolation with modules" =
    let color = `Name "tomato" in
    let color2 = `Hex "#FF00FF" in
    let width = `Px 1 in
    let module Style =
      [%css
      stylesheet
        {|
          .foo {
            background-color: %{color#Css_gen.Color};
          }

          .foo.bar.baz {
            background-color: %{color2#Css_gen.Color};
            width: %{width#Css_gen.Length};
          }
          |}]
    in
    test (Vdom.Attr.many [ Style.foo; Style.bar; Style.baz ]);
    [%expect
      {|
      <div class="bar_hash_replaced_in_test baz_hash_replaced_in_test foo_hash_replaced_in_test"
           custom-css-vars=((--ppx_css_anonymous_var_13_hash_replaced_in_test tomato)(--ppx_css_anonymous_var_14_hash_replaced_in_test #FF00FF)(--ppx_css_anonymous_var_15_hash_replaced_in_test 1px)(--ppx_css_anonymous_var_13_hash_replaced_in_test tomato)(--ppx_css_anonymous_var_14_hash_replaced_in_test #FF00FF)(--ppx_css_anonymous_var_15_hash_replaced_in_test 1px)(--ppx_css_anonymous_var_13_hash_replaced_in_test tomato)(--ppx_css_anonymous_var_14_hash_replaced_in_test #FF00FF)(--ppx_css_anonymous_var_15_hash_replaced_in_test 1px))> </div>
      |}]
  ;;

  let%expect_test "user variables _and_ anonymous variables" =
    let color = `Name "tomato" in
    let module Style =
      [%css
      stylesheet
        {|
          .foo {
            background-color: %{color#Css_gen.Color};
          }

          .foo.bar.baz {
            background-color: var(--color);
          }
          |}]
    in
    test (Vdom.Attr.many [ Style.foo; Style.bar; Style.baz ]);
    [%expect
      {|
      <div class="bar_hash_replaced_in_test baz_hash_replaced_in_test foo_hash_replaced_in_test"
           custom-css-vars=((--ppx_css_anonymous_var_16_hash_replaced_in_test tomato)(--ppx_css_anonymous_var_16_hash_replaced_in_test tomato)(--ppx_css_anonymous_var_16_hash_replaced_in_test tomato))> </div>
      |}]
  ;;

  let%expect_test "interpolated expressions with side effects" =
    let color name =
      print_endline name;
      `Name "tomato"
    in
    let module Style =
      [%css
      stylesheet
        {|
          .foo {
            background-color: %{color "first"#Css_gen.Color};
          }

          .foo.bar.baz {
            background-color: %{color "second"#Css_gen.Color};
            background-color: %{color "third"#Css_gen.Color};
            background-color: var(--color);
            background-color: %{color "fourth"#Css_gen.Color};
          }
          |}]
    in
    test (Vdom.Attr.many [ Style.foo; Style.bar; Style.baz ]);
    [%expect
      {|
      first
      second
      third
      fourth
      <div class="bar_hash_replaced_in_test baz_hash_replaced_in_test foo_hash_replaced_in_test"
           custom-css-vars=((--ppx_css_anonymous_var_17_hash_replaced_in_test tomato)(--ppx_css_anonymous_var_18_hash_replaced_in_test tomato)(--ppx_css_anonymous_var_19_hash_replaced_in_test tomato)(--ppx_css_anonymous_var_20_hash_replaced_in_test tomato)(--ppx_css_anonymous_var_17_hash_replaced_in_test tomato)(--ppx_css_anonymous_var_18_hash_replaced_in_test tomato)(--ppx_css_anonymous_var_19_hash_replaced_in_test tomato)(--ppx_css_anonymous_var_20_hash_replaced_in_test tomato)(--ppx_css_anonymous_var_17_hash_replaced_in_test tomato)(--ppx_css_anonymous_var_18_hash_replaced_in_test tomato)(--ppx_css_anonymous_var_19_hash_replaced_in_test tomato)(--ppx_css_anonymous_var_20_hash_replaced_in_test tomato))> </div>
      |}]
  ;;
end
