open! Core
open! Ppxlib
open! Ppx_css

let loc = Location.none

module%test [@name "styled components tests"] _ = struct
  let test = Test_util.test_expression

  let%expect_test "single class" =
    test
      [%expr
        {|
        background-color: tomato;

        .foo {

        }
      |}];
    (* .foo is hashed: *)
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
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__001___61f7aae6af__group_0;
                      Virtual_dom.Vdom.Attr.class_ {|inline_class_hash_61f7aae6af|}))
            end
        end in Ppx_css_anonymous_style__002_.inline_class
      Hoisted context:
      ----------------
      let sheet_x__001___61f7aae6af__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__001___61f7aae6af__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__001___61f7aae6af__0
             {|
      /* _none_ */

      *.inline_class_hash_61f7aae6af {
       background-color:tomato;
       *.foo {

       }

      }|})
      |xxx}]
  ;;

  let%expect_test "single class - not hashed" =
    test
      [%expr
        {|
        background-color: tomato;

        .foo {

        }
      |}
          ~dont_hash:[ "foo" ]];
    [%expect
      {xxx| ~dont_hash is a no-op as classes and ids in the *inline* ppx_css syntax are not hashed. |xxx}]
  ;;

  let%expect_test "single class - not hashed prefixes" =
    (* NOTE: This test demonstrates that hashing is turned off in the css for
         classnames as they are otherwise not accessible from the styled component syntax.
    *)
    test
      [%expr
        {|
        background-color: tomato;

        .foo {

        }
      |}
          ~dont_hash_prefixes:[ "f" ]];
    [%expect
      {xxx| ~dont_hash_prefixes is a no-op as classes and ids in the *inline* ppx_css syntax are not hashed. |xxx}]
  ;;

  let%expect_test "single class - unused warning" =
    test
      [%expr
        {|
        background-color: tomato;

        .foo {

        }
      |}
          ~dont_hash:[ "bar" ]];
    [%expect
      {xxx| ~dont_hash is a no-op as classes and ids in the *inline* ppx_css syntax are not hashed. |xxx}];
    test
      [%expr
        {|
        background-color: tomato;

        .foo {

        }
      |}
          ~dont_hash_prefixes:[ "bar" ]];
    [%expect
      {xxx| ~dont_hash_prefixes is a no-op as classes and ids in the *inline* ppx_css syntax are not hashed. |xxx}]
  ;;
end

module%test [@name "stylesheet components tests"] _ = struct
  let test = Test_util.test_struct

  let%expect_test "single class" =
    test
      [%expr
        stylesheet
          {|
  .a {
    background-color: tomato;

    .foo {

    }
  }
      |}];
    (* .foo is hashed: *)
    [%expect
      {xxx|
      [@@@ocaml.warning "-32"]
      let __type_info_for_ppx_css :
        ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
        = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
      module type S  =
        sig
          module For_referencing : sig val a : string val foo : string end
          val a : Virtual_dom.Vdom.Attr.t
          val foo : Virtual_dom.Vdom.Attr.t
        end
      type t = (module S)
      module Default : S =
        struct
          module For_referencing =
            struct
              let a = {|a_hash_898b2963c5|}
              let foo = {|foo_hash_898b2963c5|}
            end
          let a =
            Virtual_dom.Vdom.Attr.lazy_
              (lazy
                 (Inline_css.Ppx_css_runtime.force
                    Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__003___898b2963c5__group_0;
                  Virtual_dom.Vdom.Attr.class_ {|a_hash_898b2963c5|}))
          let foo =
            Virtual_dom.Vdom.Attr.lazy_
              (lazy
                 (Inline_css.Ppx_css_runtime.force
                    Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__003___898b2963c5__group_0;
                  Virtual_dom.Vdom.Attr.class_ {|foo_hash_898b2963c5|}))
        end
      include Default
      let default : t = (module Default)

      Hoisted module:

      let sheet_x__003___898b2963c5__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__003___898b2963c5__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__003___898b2963c5__0
             {|
      /* _none_ */

      *.a_hash_898b2963c5 {
       background-color:tomato;
       *.foo_hash_898b2963c5 {

       }

      }|})
      |xxx}]
  ;;

  let%expect_test "single class - renamed" =
    test
      [%expr
        stylesheet
          {|
     .a {
       background-color: tomato;

       .foo {

       }
     }
       |}
          ~rewrite:[ "foo", "i-am-renamed" ]];
    [%expect
      {xxx|
      The use of ~rewrite is deprecated, please use ~dont_hash or ~dont_hash_prefixes
             instead. Alternatively, consider writing all of your CSS in the same %css
             stylesheet incantation. Deprecating ~rewrite allows for more optimiztions in ppx_css,
             at the expense of expressibility. We've audited bonsai apps and believe this expressibility
             was unused so we've removed it. If this conflicts with your use case please reach out.
      |xxx}]
  ;;

  let%expect_test "single class - not hashed" =
    test
      [%expr
        stylesheet
          {|
     .a {
       background-color: tomato;

       .foo {

       }
     }
       |}
          ~dont_hash:[ "foo" ]];
    [%expect
      {xxx|
      [@@@ocaml.warning "-32"]
      let __type_info_for_ppx_css :
        ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
        = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
      module type S  =
        sig
          module For_referencing : sig val a : string val foo : string end
          val a : Virtual_dom.Vdom.Attr.t
          val foo : Virtual_dom.Vdom.Attr.t
        end
      type t = (module S)
      module Default : S =
        struct
          module For_referencing =
            struct let a = {|a_hash_898b2963c5|}
                   let foo = {|foo|} end
          let a =
            Virtual_dom.Vdom.Attr.lazy_
              (lazy
                 (Inline_css.Ppx_css_runtime.force
                    Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__004___898b2963c5__group_0;
                  Virtual_dom.Vdom.Attr.class_ {|a_hash_898b2963c5|}))
          let foo = Virtual_dom.Vdom.Attr.class_ {|foo|}
        end
      include Default
      let default : t = (module Default)

      Hoisted module:

      let sheet_x__004___898b2963c5__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__004___898b2963c5__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__004___898b2963c5__0
             {|
      /* _none_ */

      *.a_hash_898b2963c5 {
       background-color:tomato;
       *.foo {

       }

      }|})
      |xxx}]
  ;;

  let%expect_test "single class - not hashed prefixes" =
    test
      [%expr
        stylesheet
          {|
    .a {
       background-color: tomato;

       .foo {

       }
    }
       |}
          ~dont_hash_prefixes:[ "f" ]];
    [%expect
      {xxx|
      [@@@ocaml.warning "-32"]
      let __type_info_for_ppx_css :
        ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
        = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
      module type S  =
        sig
          module For_referencing : sig val a : string val foo : string end
          val a : Virtual_dom.Vdom.Attr.t
          val foo : Virtual_dom.Vdom.Attr.t
        end
      type t = (module S)
      module Default : S =
        struct
          module For_referencing =
            struct let a = {|a_hash_898b2963c5|}
                   let foo = {|foo|} end
          let a =
            Virtual_dom.Vdom.Attr.lazy_
              (lazy
                 (Inline_css.Ppx_css_runtime.force
                    Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__005___898b2963c5__group_0;
                  Virtual_dom.Vdom.Attr.class_ {|a_hash_898b2963c5|}))
          let foo = Virtual_dom.Vdom.Attr.class_ {|foo|}
        end
      include Default
      let default : t = (module Default)

      Hoisted module:

      let sheet_x__005___898b2963c5__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__005___898b2963c5__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__005___898b2963c5__0
             {|
      /* _none_ */

      *.a_hash_898b2963c5 {
       background-color:tomato;
       *.foo {

       }

      }|})
      |xxx}]
  ;;

  let%expect_test "single class - unused warning" =
    test
      [%expr
        stylesheet
          {|
     .a {
       background-color: tomato;

       .foo {

       }
     }
       |}
          ~dont_hash:[ "bar" ]];
    [%expect {xxx| Unused keys: (bar) |xxx}];
    test
      [%expr
        stylesheet
          {|
     .a {
       background-color: tomato;

       .foo {

       }
     }
       |}
          ~dont_hash_prefixes:[ "bar" ]];
    [%expect {xxx| Unused prefixes: (bar) |xxx}]
  ;;
end

let test = Test_util.test_struct

let%expect_test "Hoisting behavior for :not(.a) { .b {} }" =
  test
    [%expr
      stylesheet
        {|
    :not(.a) {
       .b {

       }
    }
       |}];
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
          struct let a = {|a_hash_f8a6de5bff|}
                 let b = {|b_hash_f8a6de5bff|} end
        let a = Virtual_dom.Vdom.Attr.class_ {|a_hash_f8a6de5bff|}
        let b = Virtual_dom.Vdom.Attr.class_ {|b_hash_f8a6de5bff|}
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__006___f8a6de5bff__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__006___f8a6de5bff__0
        {|
    /* _none_ */

    *:not(.a_hash_f8a6de5bff) {
     *.b_hash_f8a6de5bff {

     }

    }|}
    |xxx}]
;;

let%expect_test "Hoisting behavior for :not(.a) .b { }" =
  test
    [%expr
      stylesheet
        {|
    :not(.a) .b {
    }
       |}];
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
          struct let a = {|a_hash_5c5f4e9512|}
                 let b = {|b_hash_5c5f4e9512|} end
        let a = Virtual_dom.Vdom.Attr.class_ {|a_hash_5c5f4e9512|}
        let b =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__007___5c5f4e9512__group_0;
                Virtual_dom.Vdom.Attr.class_ {|b_hash_5c5f4e9512|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__007___5c5f4e9512__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__007___5c5f4e9512__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__007___5c5f4e9512__0
           {|
    /* _none_ */

    *:not(.a_hash_5c5f4e9512) *.b_hash_5c5f4e9512 {

    }|})
    |xxx}]
;;

let%expect_test "Testing nested CSS with more combinations of [:not], and [:where]." =
  test
    [%expr
      stylesheet
        {|
        .a {
          :not(.a) {
            background-color: tomato
          }
        }
       |}];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let __type_info_for_ppx_css :
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
    module type S  =
      sig
        module For_referencing : sig val a : string end
        val a : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing = struct let a = {|a_hash_a2faeb435a|} end
        let a =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__008___a2faeb435a__group_0;
                Virtual_dom.Vdom.Attr.class_ {|a_hash_a2faeb435a|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__008___a2faeb435a__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__008___a2faeb435a__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__008___a2faeb435a__0
           {|
    /* _none_ */

    *.a_hash_a2faeb435a {
     *:not(.a_hash_a2faeb435a) {
      background-color:tomato
     }

    }|})
    |xxx}];
  test
    [%expr
      stylesheet
        {|
        .a {
          :not(:where(.a)) {
            background-color: tomato
          }
        }
       |}];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let __type_info_for_ppx_css :
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
    module type S  =
      sig
        module For_referencing : sig val a : string end
        val a : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing = struct let a = {|a_hash_d80bf92386|} end
        let a =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__009___d80bf92386__group_0;
                Virtual_dom.Vdom.Attr.class_ {|a_hash_d80bf92386|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__009___d80bf92386__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__009___d80bf92386__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__009___d80bf92386__0
           {|
    /* _none_ */

    *.a_hash_d80bf92386 {
     *:not(:where(.a_hash_d80bf92386)) {
      background-color:tomato
     }

    }|})
    |xxx}];
  test
    [%expr
      stylesheet
        {|
        .a {
          :not(:where(&)) {
            background-color: tomato
          }
        }
       |}];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let __type_info_for_ppx_css :
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
    module type S  =
      sig
        module For_referencing : sig val a : string end
        val a : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing = struct let a = {|a_hash_30b6699210|} end
        let a = Virtual_dom.Vdom.Attr.class_ {|a_hash_30b6699210|}
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__010___30b6699210__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__010___30b6699210__0
        {|
    /* _none_ */

    *.a_hash_30b6699210 {
     *:not(:where(&)) {
      background-color:tomato
     }

    }|}
    |xxx}]
;;
