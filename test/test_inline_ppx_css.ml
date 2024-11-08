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
      let module Ppx_css_anonymous_style__002_ =
        struct
          include
            struct
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__001___a9ee0785b4__group_0;
                      Virtual_dom.Vdom.Attr.class_
                        {|foo__inline_class_hash_a9ee0785b4|}))
            end
        end in Ppx_css_anonymous_style__002_.inline_class
      Hoisted context:
      ----------------
      let sheet_x__001___a9ee0785b4__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__001___a9ee0785b4__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__001___a9ee0785b4__0
             {|
      /* app/foo/foo.ml */

      *.foo__inline_class_hash_a9ee0785b4 {
       background-color:tomato
      }|})
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
      let module Ppx_css_anonymous_style__006_ =
        struct
          include
            struct
              let ppx_css__internal_anonymous_variables__003_ =
                let ppx_css_temp_variable__005_ = (((color)
                  [@merlin.focus ]) : string) in
                Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
                  [({|--ppx_css_anonymous_var_1_hash_a70fca0095|},
                     ppx_css_temp_variable__005_)]
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__004___a70fca0095__group_0;
                      ((Virtual_dom.Vdom.Attr.combine
                          (Virtual_dom.Vdom.Attr.class_
                             {|foo__inline_class_hash_a70fca0095|})
                          ppx_css__internal_anonymous_variables__003_)
                      [@merlin.focus ])))
            end
        end in Ppx_css_anonymous_style__006_.inline_class
      Hoisted context:
      ----------------
      let sheet_x__004___a70fca0095__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__004___a70fca0095__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__004___a70fca0095__0
             {|
      /* app/foo/foo.ml */

      *.foo__inline_class_hash_a70fca0095 {
       background-color:var(--ppx_css_anonymous_var_1_hash_a70fca0095)
      }|})
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
      let module Ppx_css_anonymous_style__010_ =
        struct
          include
            struct
              let ppx_css__internal_anonymous_variables__007_ =
                let ppx_css_temp_variable__009_ = (((Mod.Foo.to_string_css color)
                  [@merlin.focus ]) : string) in
                Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
                  [({|--ppx_css_anonymous_var_2_hash_e1cd772732|},
                     ppx_css_temp_variable__009_)]
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__008___e1cd772732__group_0;
                      ((Virtual_dom.Vdom.Attr.combine
                          (Virtual_dom.Vdom.Attr.class_
                             {|foo__inline_class_hash_e1cd772732|})
                          ppx_css__internal_anonymous_variables__007_)
                      [@merlin.focus ])))
            end
        end in Ppx_css_anonymous_style__010_.inline_class
      Hoisted context:
      ----------------
      let sheet_x__008___e1cd772732__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__008___e1cd772732__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__008___e1cd772732__0
             {|
      /* app/foo/foo.ml */

      *.foo__inline_class_hash_e1cd772732 {
       background-color:var(--ppx_css_anonymous_var_2_hash_e1cd772732)
      }|})
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
      let module Ppx_css_anonymous_style__016_ =
        struct
          include
            struct
              let ppx_css__internal_anonymous_variables__011_ =
                let ppx_css_temp_variable__013_ = (((color1)
                  [@merlin.focus ]) : string) in
                let ppx_css_temp_variable__014_ = (((color2)
                  [@merlin.focus ]) : string) in
                let ppx_css_temp_variable__015_ = (((color3)
                  [@merlin.focus ]) : string) in
                Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
                  [({|--ppx_css_anonymous_var_3_hash_6b28424761|},
                     ppx_css_temp_variable__013_);
                  ({|--ppx_css_anonymous_var_4_hash_6b28424761|},
                    ppx_css_temp_variable__014_);
                  ({|--ppx_css_anonymous_var_5_hash_6b28424761|},
                    ppx_css_temp_variable__015_)]
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__012___6b28424761__group_0;
                      ((Virtual_dom.Vdom.Attr.combine
                          (Virtual_dom.Vdom.Attr.class_
                             {|foo__inline_class_hash_6b28424761|})
                          ppx_css__internal_anonymous_variables__011_)
                      [@merlin.focus ])))
            end
        end in Ppx_css_anonymous_style__016_.inline_class
      Hoisted context:
      ----------------
      let sheet_x__012___6b28424761__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__012___6b28424761__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__012___6b28424761__0
             {|
      /* app/foo/foo.ml */

      *.foo__inline_class_hash_6b28424761 {
       background-color:var(--ppx_css_anonymous_var_3_hash_6b28424761);
       background:var(--ppx_css_anonymous_var_4_hash_6b28424761);
       background-color:var(--ppx_css_anonymous_var_5_hash_6b28424761)
      }|})
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
      let module Ppx_css_anonymous_style__022_ =
        struct
          include
            struct
              let ppx_css__internal_anonymous_variables__017_ =
                let ppx_css_temp_variable__019_ = (((f ())
                  [@merlin.focus ]) : string) in
                let ppx_css_temp_variable__020_ = (((g ())
                  [@merlin.focus ]) : string) in
                let ppx_css_temp_variable__021_ = (((f ())
                  [@merlin.focus ]) : string) in
                Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
                  [({|--ppx_css_anonymous_var_6_hash_f092e8fa9d|},
                     ppx_css_temp_variable__019_);
                  ({|--ppx_css_anonymous_var_7_hash_f092e8fa9d|},
                    ppx_css_temp_variable__020_);
                  ({|--ppx_css_anonymous_var_8_hash_f092e8fa9d|},
                    ppx_css_temp_variable__021_)]
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__018___f092e8fa9d__group_0;
                      ((Virtual_dom.Vdom.Attr.combine
                          (Virtual_dom.Vdom.Attr.class_
                             {|foo__inline_class_hash_f092e8fa9d|})
                          ppx_css__internal_anonymous_variables__017_)
                      [@merlin.focus ])))
            end
        end in Ppx_css_anonymous_style__022_.inline_class
      Hoisted context:
      ----------------
      let sheet_x__018___f092e8fa9d__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__018___f092e8fa9d__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__018___f092e8fa9d__0
             {|
      /* app/foo/foo.ml */

      *.foo__inline_class_hash_f092e8fa9d {
       background-color:var(--ppx_css_anonymous_var_6_hash_f092e8fa9d);
       background:var(--ppx_css_anonymous_var_7_hash_f092e8fa9d);
       background-color:var(--ppx_css_anonymous_var_8_hash_f092e8fa9d)
      }|})
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
      let module Ppx_css_anonymous_style__029_ =
        struct
          include
            struct
              let ppx_css__internal_anonymous_variables__023_ =
                let ppx_css_temp_variable__025_ = (((first ())
                  [@merlin.focus ]) : string) in
                let ppx_css_temp_variable__026_ = (((second ())
                  [@merlin.focus ]) : string) in
                let ppx_css_temp_variable__027_ = (((third ())
                  [@merlin.focus ]) : string) in
                let ppx_css_temp_variable__028_ = (((fourth ())
                  [@merlin.focus ]) : string) in
                Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
                  [({|--ppx_css_anonymous_var_9_hash_2d2b734001|},
                     ppx_css_temp_variable__025_);
                  ({|--ppx_css_anonymous_var_10_hash_2d2b734001|},
                    ppx_css_temp_variable__026_);
                  ({|--ppx_css_anonymous_var_11_hash_2d2b734001|},
                    ppx_css_temp_variable__027_);
                  ({|--ppx_css_anonymous_var_12_hash_2d2b734001|},
                    ppx_css_temp_variable__028_)]
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__024___2d2b734001__group_0;
                      ((Virtual_dom.Vdom.Attr.combine
                          (Virtual_dom.Vdom.Attr.class_
                             {|foo__inline_class_hash_2d2b734001|})
                          ppx_css__internal_anonymous_variables__023_)
                      [@merlin.focus ])))
            end
        end in Ppx_css_anonymous_style__029_.inline_class
      Hoisted context:
      ----------------
      let sheet_x__024___2d2b734001__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__024___2d2b734001__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__024___2d2b734001__0
             {|
      /* app/foo/foo.ml */

      *.foo__inline_class_hash_2d2b734001 {
       background-color:var(--ppx_css_anonymous_var_9_hash_2d2b734001);
       background:var(--ppx_css_anonymous_var_10_hash_2d2b734001);
       background-color:var(--ppx_css_anonymous_var_11_hash_2d2b734001);
       background-color:var(--ppx_css_anonymous_var_12_hash_2d2b734001)
      }|})
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
      let module Ppx_css_anonymous_style__031_ =
        struct
          include
            struct
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__030___eb0a44be26__group_0;
                      Virtual_dom.Vdom.Attr.class_
                        {|foo__inline_class_hash_eb0a44be26|}))
            end
        end in Ppx_css_anonymous_style__031_.inline_class
      Hoisted context:
      ----------------
      let sheet_x__030___eb0a44be26__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__030___eb0a44be26__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__030___eb0a44be26__0
             {|
      /* app/foo/foo.ml */

      *.foo__inline_class_hash_eb0a44be26 {
       background-color:var(--foo)
      }|})
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
      let module Ppx_css_anonymous_style__035_ =
        struct
          include
            struct
              let ppx_css__internal_anonymous_variables__032_ =
                let ppx_css_temp_variable__034_ =
                  (((Css_gen.Color.to_string_css foo)[@merlin.focus ]) : string) in
                Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
                  [({|--ppx_css_anonymous_var_13_hash_032677e5e6|},
                     ppx_css_temp_variable__034_)]
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__033___032677e5e6__group_0;
                      ((Virtual_dom.Vdom.Attr.combine
                          (Virtual_dom.Vdom.Attr.class_
                             {|foo__inline_class_hash_032677e5e6|})
                          ppx_css__internal_anonymous_variables__032_)
                      [@merlin.focus ])))
            end
        end in Ppx_css_anonymous_style__035_.inline_class
      Hoisted context:
      ----------------
      let sheet_x__033___032677e5e6__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__033___032677e5e6__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__033___032677e5e6__0
             {|
      /* app/foo/foo.ml */

      *.foo__inline_class_hash_032677e5e6 {
       background-color:var(--ppx_css_anonymous_var_13_hash_032677e5e6);
       color:var(--background-color1)
      }|})
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
