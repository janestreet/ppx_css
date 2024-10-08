open! Core
open Ppxlib

let loc = Test_util.loc_with_mock_name

module%test [@name "basic"] _ = struct
  let test = Test_util.test_expression

  let%expect_test "basic" =
    test
      [%expr
        {|
        background-color: tomato;
      |}];
    [%expect
      {xxx|
      Expression context:
      -------------------
      let module Ppx_css_anonymous_style__001_ =
        struct
          include
            struct
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.class_ {|foo__inline_class_hash_b135c791dd|}
            end
        end in Ppx_css_anonymous_style__001_.inline_class
      Hoisted context:
      ----------------
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* app/foo/foo.ml */

      *.foo__inline_class_hash_b135c791dd {
       background-color:tomato
      }|}
      |xxx}]
  ;;

  let%expect_test "basic with interpolation" =
    test
      [%expr
        {|
        background-color: %{color};
      |}];
    [%expect
      {xxx|
      Expression context:
      -------------------
      let module Ppx_css_anonymous_style__004_ =
        struct
          include
            struct
              let ppx_css__internal_anonymous_variables__002_ =
                let ppx_css_temp_variable__003_ = (((color)
                  [@merlin.focus ]) : string) in
                Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
                  [({|--ppx_css_anonymous_var_1_hash_d5e42a463c|},
                     ppx_css_temp_variable__003_)]
              let foo__inline_class =
                ((Virtual_dom.Vdom.Attr.combine
                    (Virtual_dom.Vdom.Attr.class_
                       {|foo__inline_class_hash_d5e42a463c|})
                    ppx_css__internal_anonymous_variables__002_)
                [@merlin.focus ])
            end
        end in Ppx_css_anonymous_style__004_.inline_class
      Hoisted context:
      ----------------
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* app/foo/foo.ml */

      *.foo__inline_class_hash_d5e42a463c {
       background-color:var(--ppx_css_anonymous_var_1_hash_d5e42a463c)
      }|}
      |xxx}]
  ;;

  let%expect_test "basic with modul-based interpolation" =
    test
      [%expr
        {|
        background-color: %{color#Mod.Foo};
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
                let ppx_css_temp_variable__006_ = (((Mod.Foo.to_string_css color)
                  [@merlin.focus ]) : string) in
                Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
                  [({|--ppx_css_anonymous_var_2_hash_4ce9566131|},
                     ppx_css_temp_variable__006_)]
              let foo__inline_class =
                ((Virtual_dom.Vdom.Attr.combine
                    (Virtual_dom.Vdom.Attr.class_
                       {|foo__inline_class_hash_4ce9566131|})
                    ppx_css__internal_anonymous_variables__005_)
                [@merlin.focus ])
            end
        end in Ppx_css_anonymous_style__007_.inline_class
      Hoisted context:
      ----------------
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* app/foo/foo.ml */

      *.foo__inline_class_hash_4ce9566131 {
       background-color:var(--ppx_css_anonymous_var_2_hash_4ce9566131)
      }|}
      |xxx}]
  ;;

  let%expect_test "many with interpolation" =
    test
      [%expr
        {|
        background-color: %{color1};
        background: %{color2};
        background-color: %{color3};
      |}];
    [%expect
      {xxx|
      Expression context:
      -------------------
      let module Ppx_css_anonymous_style__012_ =
        struct
          include
            struct
              let ppx_css__internal_anonymous_variables__008_ =
                let ppx_css_temp_variable__009_ = (((color1)
                  [@merlin.focus ]) : string) in
                let ppx_css_temp_variable__010_ = (((color2)
                  [@merlin.focus ]) : string) in
                let ppx_css_temp_variable__011_ = (((color3)
                  [@merlin.focus ]) : string) in
                Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
                  [({|--ppx_css_anonymous_var_3_hash_66cf474dc9|},
                     ppx_css_temp_variable__009_);
                  ({|--ppx_css_anonymous_var_4_hash_66cf474dc9|},
                    ppx_css_temp_variable__010_);
                  ({|--ppx_css_anonymous_var_5_hash_66cf474dc9|},
                    ppx_css_temp_variable__011_)]
              let foo__inline_class =
                ((Virtual_dom.Vdom.Attr.combine
                    (Virtual_dom.Vdom.Attr.class_
                       {|foo__inline_class_hash_66cf474dc9|})
                    ppx_css__internal_anonymous_variables__008_)
                [@merlin.focus ])
            end
        end in Ppx_css_anonymous_style__012_.inline_class
      Hoisted context:
      ----------------
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* app/foo/foo.ml */

      *.foo__inline_class_hash_66cf474dc9 {
       background-color:var(--ppx_css_anonymous_var_3_hash_66cf474dc9);
       background:var(--ppx_css_anonymous_var_4_hash_66cf474dc9);
       background-color:var(--ppx_css_anonymous_var_5_hash_66cf474dc9)
      }|}
      |xxx}]
  ;;

  let%expect_test "duplicate functions" =
    test
      [%expr
        {|
        background-color: %{f ()};
        background: %{g ()};
        background-color: %{f ()};
      |}];
    [%expect
      {xxx|
      Expression context:
      -------------------
      let module Ppx_css_anonymous_style__017_ =
        struct
          include
            struct
              let ppx_css__internal_anonymous_variables__013_ =
                let ppx_css_temp_variable__014_ = (((f ())
                  [@merlin.focus ]) : string) in
                let ppx_css_temp_variable__015_ = (((g ())
                  [@merlin.focus ]) : string) in
                let ppx_css_temp_variable__016_ = (((f ())
                  [@merlin.focus ]) : string) in
                Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
                  [({|--ppx_css_anonymous_var_6_hash_a54529b646|},
                     ppx_css_temp_variable__014_);
                  ({|--ppx_css_anonymous_var_7_hash_a54529b646|},
                    ppx_css_temp_variable__015_);
                  ({|--ppx_css_anonymous_var_8_hash_a54529b646|},
                    ppx_css_temp_variable__016_)]
              let foo__inline_class =
                ((Virtual_dom.Vdom.Attr.combine
                    (Virtual_dom.Vdom.Attr.class_
                       {|foo__inline_class_hash_a54529b646|})
                    ppx_css__internal_anonymous_variables__013_)
                [@merlin.focus ])
            end
        end in Ppx_css_anonymous_style__017_.inline_class
      Hoisted context:
      ----------------
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* app/foo/foo.ml */

      *.foo__inline_class_hash_a54529b646 {
       background-color:var(--ppx_css_anonymous_var_6_hash_a54529b646);
       background:var(--ppx_css_anonymous_var_7_hash_a54529b646);
       background-color:var(--ppx_css_anonymous_var_8_hash_a54529b646)
      }|}
      |xxx}]
  ;;

  let%expect_test "interpolation order" =
    test
      [%expr
        {|
        background-color: %{first ()};
        background: %{second ()};
        background-color: %{third ()};
        background-color: %{fourth ()};
      |}];
    (* NOTE: Importantly, a let binding chain is created to enforce evaluation order...
    *)
    [%expect
      {xxx|
      Expression context:
      -------------------
      let module Ppx_css_anonymous_style__023_ =
        struct
          include
            struct
              let ppx_css__internal_anonymous_variables__018_ =
                let ppx_css_temp_variable__019_ = (((first ())
                  [@merlin.focus ]) : string) in
                let ppx_css_temp_variable__020_ = (((second ())
                  [@merlin.focus ]) : string) in
                let ppx_css_temp_variable__021_ = (((third ())
                  [@merlin.focus ]) : string) in
                let ppx_css_temp_variable__022_ = (((fourth ())
                  [@merlin.focus ]) : string) in
                Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
                  [({|--ppx_css_anonymous_var_9_hash_fa7f7028a6|},
                     ppx_css_temp_variable__019_);
                  ({|--ppx_css_anonymous_var_10_hash_fa7f7028a6|},
                    ppx_css_temp_variable__020_);
                  ({|--ppx_css_anonymous_var_11_hash_fa7f7028a6|},
                    ppx_css_temp_variable__021_);
                  ({|--ppx_css_anonymous_var_12_hash_fa7f7028a6|},
                    ppx_css_temp_variable__022_)]
              let foo__inline_class =
                ((Virtual_dom.Vdom.Attr.combine
                    (Virtual_dom.Vdom.Attr.class_
                       {|foo__inline_class_hash_fa7f7028a6|})
                    ppx_css__internal_anonymous_variables__018_)
                [@merlin.focus ])
            end
        end in Ppx_css_anonymous_style__023_.inline_class
      Hoisted context:
      ----------------
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* app/foo/foo.ml */

      *.foo__inline_class_hash_fa7f7028a6 {
       background-color:var(--ppx_css_anonymous_var_9_hash_fa7f7028a6);
       background:var(--ppx_css_anonymous_var_10_hash_fa7f7028a6);
       background-color:var(--ppx_css_anonymous_var_11_hash_fa7f7028a6);
       background-color:var(--ppx_css_anonymous_var_12_hash_fa7f7028a6)
      }|}
      |xxx}]
  ;;

  let%expect_test "user-variables are not hashed." =
    (* Importantly, --foo is not hashed. *)
    test
      [%expr
        {|
        background-color: var(--foo);
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
                Virtual_dom.Vdom.Attr.class_ {|foo__inline_class_hash_2e4462614c|}
            end
        end in Ppx_css_anonymous_style__024_.inline_class
      Hoisted context:
      ----------------
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* app/foo/foo.ml */

      *.foo__inline_class_hash_2e4462614c {
       background-color:var(--foo)
      }|}
      |xxx}]
  ;;

  let%expect_test "no-op dont-hash hashing" =
    (* NOTE: --foo remains unhashed as it would've been be default. Sending in [--foo]
         has no effect. Technically we could raise that [dont_hash] is unused, although
         that would imply that [--foo] is hashed which is not and might be a confusing
         error message, so no error is raised, giving the user the illusion of having
         done something that would've happened regardless. *)
    test
      [%expr
        {|
        background-color: var(--foo);
      |}
          ~dont_hash:[ "--foo" ]];
    [%expect
      {xxx| ~dont_hash is a no-op as classes and ids in the *inline* ppx_css syntax are not hashed. |xxx}]
  ;;

  let%expect_test "no op prefix hashing is ignored" =
    (* NOTE: This is a bit subtle, but here we do decide to raise for variables.
         The reason why is that the anonymous variables are _always_ hashed and
         can't be disabled. This results in awkwardness for attempting to not hash
         [--foo]. *)
    test
      [%expr
        {|
        background-color: var(--foo);
      |}
          ~dont_hash_prefixes:[ "--" ]];
    [%expect
      {xxx| ~dont_hash_prefixes is a no-op as classes and ids in the *inline* ppx_css syntax are not hashed. |xxx}]
  ;;

  let%expect_test "no op prefix hashing is ignored, attempting to not hash the anonymous \
                   variables is disallowed"
    =
    (* The variable created by the anonymous css inliner is _always_ hashed; attempting
         to not hash it is an error. *)
    test
      [%expr
        {|
        background-color: var(--foo);
        background-color: %{color};
      |}
          ~dont_hash_prefixes:[ "--" ]];
    [%expect
      {xxx| ~dont_hash_prefixes is a no-op as classes and ids in the *inline* ppx_css syntax are not hashed. |xxx}];
    test
      [%expr
        {|
        background-color: var(--foo);
      |}
          ~dont_hash_prefixes:[ "" ]];
    [%expect
      {xxx| ~dont_hash_prefixes is a no-op as classes and ids in the *inline* ppx_css syntax are not hashed. |xxx}];
    test
      [%expr
        {|
        background-color: %{color};
        color: %{foo};
      |}
          ~dont_hash_prefixes:[ "" ]];
    [%expect
      {xxx| ~dont_hash_prefixes is a no-op as classes and ids in the *inline* ppx_css syntax are not hashed. |xxx}]
  ;;

  let%expect_test "attempting to stop the anonymous class from hashing results in \
                   failure."
    =
    test
      [%expr
        {|
        background-color: %{color};
        color: %{foo};
      |}
          ~dont_hash_prefixes:[ "ppx_css_anonymous_class" ]];
    [%expect
      {xxx| ~dont_hash_prefixes is a no-op as classes and ids in the *inline* ppx_css syntax are not hashed. |xxx}];
    test
      [%expr
        {|
        background-color: %{color};
        color: %{foo};
      |}
          ~dont_hash:[ "ppx_css_anonymous_class" ]];
    [%expect
      {xxx| ~dont_hash is a no-op as classes and ids in the *inline* ppx_css syntax are not hashed. |xxx}]
  ;;

  let%expect_test "user variables are _not_ hashed, while anonymous variable _are_ hashed"
    =
    (* NOTE: This tests against a regression that ppx_css had during development. *)
    test
      [%expr
        {|
   background-color: %{foo#Css_gen.Color};
   color: var(--background-color1);
   |}];
    [%expect
      {xxx|
      Expression context:
      -------------------
      let module Ppx_css_anonymous_style__027_ =
        struct
          include
            struct
              let ppx_css__internal_anonymous_variables__025_ =
                let ppx_css_temp_variable__026_ =
                  (((Css_gen.Color.to_string_css foo)[@merlin.focus ]) : string) in
                Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
                  [({|--ppx_css_anonymous_var_13_hash_d20abbe46c|},
                     ppx_css_temp_variable__026_)]
              let foo__inline_class =
                ((Virtual_dom.Vdom.Attr.combine
                    (Virtual_dom.Vdom.Attr.class_
                       {|foo__inline_class_hash_d20abbe46c|})
                    ppx_css__internal_anonymous_variables__025_)
                [@merlin.focus ])
            end
        end in Ppx_css_anonymous_style__027_.inline_class
      Hoisted context:
      ----------------
      let () =
        Inline_css.Private.append_but_do_not_update
          {|
      /* app/foo/foo.ml */

      *.foo__inline_class_hash_d20abbe46c {
       background-color:var(--ppx_css_anonymous_var_13_hash_d20abbe46c);
       color:var(--background-color1)
      }|}
      |xxx}]
  ;;

  let%expect_test "parse errors" =
    test [%expr foo];
    [%expect
      {xxx| %css must contain a call to [val {|css string|} : ?rewrite:(string * string) list -> ?dont_hash:string list -> dont_hash_prefixes:string list -> string -> unit] |xxx}];
    test [%expr "" ""];
    [%expect
      {xxx| ppx_css found unexpected arguments. %css must contain a call to [?dont_hash:string list -> dont_hash_prefixes:string list -> string -> unit] |xxx}]
  ;;
end
