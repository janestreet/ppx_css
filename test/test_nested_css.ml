open! Core
open! Ppxlib
open! Ppx_css

let loc = Test_util.loc_with_mock_name
let () = Ppx_css_syntax.Preprocess_arguments.set_lazy_loading_optimization (Some true)

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
              let foo__inline_class =
                Virtual_dom.Vdom.Attr.lazy_
                  (lazy
                     (Inline_css.Ppx_css_runtime.force
                        Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__001___d1eeb90a40__group_0;
                      Virtual_dom.Vdom.Attr.class_
                        {|foo__inline_class_hash_d1eeb90a40|}))
            end
        end in Ppx_css_anonymous_style__002_.foo__inline_class
      Hoisted context:
      ----------------
      let sheet_x__001___d1eeb90a40__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__001___d1eeb90a40__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__001___d1eeb90a40__0
             {|
      /* app/foo/foo.ml */

      .foo__inline_class_hash_d1eeb90a40 {
        background-color: tomato;
        .foo {
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
    (* NOTE: This test demonstrates that hashing is turned off in the css for classnames
       as they are otherwise not accessible from the styled component syntax.
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
        = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
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
              let a = {|a_hash_48837b6400|}
              let foo = {|foo_hash_48837b6400|}
            end
          let a =
            Virtual_dom.Vdom.Attr.lazy_
              (lazy
                 (Inline_css.Ppx_css_runtime.force
                    Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__003___48837b6400__group_0;
                  Virtual_dom.Vdom.Attr.class_ {|a_hash_48837b6400|}))
          let foo =
            Virtual_dom.Vdom.Attr.lazy_
              (lazy
                 (Inline_css.Ppx_css_runtime.force
                    Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__003___48837b6400__group_0;
                  Virtual_dom.Vdom.Attr.class_ {|foo_hash_48837b6400|}))
        end
      include Default
      let default : t = (module Default)

      Hoisted module:

      let sheet_x__003___48837b6400__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__003___48837b6400__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__003___48837b6400__0
             {|
      /* app/foo/foo.ml */

      .a_hash_48837b6400 {
        background-color: tomato;
        .foo_hash_48837b6400 {
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
        = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
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
            struct let a = {|a_hash_a954182249|}
                   let foo = {|foo|} end
          let a =
            Virtual_dom.Vdom.Attr.lazy_
              (lazy
                 (Inline_css.Ppx_css_runtime.force
                    Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__004___a954182249__group_0;
                  Virtual_dom.Vdom.Attr.class_ {|a_hash_a954182249|}))
          let foo = Virtual_dom.Vdom.Attr.class_ {|foo|}
        end
      include Default
      let default : t = (module Default)

      Hoisted module:

      let sheet_x__004___a954182249__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__004___a954182249__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__004___a954182249__0
             {|
      /* app/foo/foo.ml */

      .a_hash_a954182249 {
        background-color: tomato;
        .foo {
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
        = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
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
            struct let a = {|a_hash_d505e893c7|}
                   let foo = {|foo|} end
          let a =
            Virtual_dom.Vdom.Attr.lazy_
              (lazy
                 (Inline_css.Ppx_css_runtime.force
                    Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__005___d505e893c7__group_0;
                  Virtual_dom.Vdom.Attr.class_ {|a_hash_d505e893c7|}))
          let foo = Virtual_dom.Vdom.Attr.class_ {|foo|}
        end
      include Default
      let default : t = (module Default)

      Hoisted module:

      let sheet_x__005___d505e893c7__0 =
        let sheet = Inline_css.Private.create_stylesheet () in
        Inline_css.Private.append_stylesheet sheet; sheet
      let update_sheet_lazy_fn_x__005___d505e893c7__group_0 =
        lazy
          (Inline_css.Private.update_stylesheet sheet_x__005___d505e893c7__0
             {|
      /* app/foo/foo.ml */

      .a_hash_d505e893c7 {
        background-color: tomato;
        .foo {
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
      = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
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
          struct let a = {|a_hash_f1b5a8dedb|}
                 let b = {|b_hash_f1b5a8dedb|} end
        let a = Virtual_dom.Vdom.Attr.class_ {|a_hash_f1b5a8dedb|}
        let b = Virtual_dom.Vdom.Attr.class_ {|b_hash_f1b5a8dedb|}
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__006___f1b5a8dedb__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__006___f1b5a8dedb__0
        {|
    /* app/foo/foo.ml */

    :not(.a_hash_f1b5a8dedb) {
      .b_hash_f1b5a8dedb {
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
      = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
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
          struct let a = {|a_hash_8926a2bb59|}
                 let b = {|b_hash_8926a2bb59|} end
        let a = Virtual_dom.Vdom.Attr.class_ {|a_hash_8926a2bb59|}
        let b =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__007___8926a2bb59__group_0;
                Virtual_dom.Vdom.Attr.class_ {|b_hash_8926a2bb59|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__007___8926a2bb59__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__007___8926a2bb59__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__007___8926a2bb59__0
           {|
    /* app/foo/foo.ml */

    :not(.a_hash_8926a2bb59) .b_hash_8926a2bb59 {
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
            background-color: tomato;
          }
        }
       |}];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let __type_info_for_ppx_css :
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
    module type S  =
      sig
        module For_referencing : sig val a : string end
        val a : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing = struct let a = {|a_hash_73765a197b|} end
        let a =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__008___73765a197b__group_0;
                Virtual_dom.Vdom.Attr.class_ {|a_hash_73765a197b|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__008___73765a197b__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__008___73765a197b__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__008___73765a197b__0
           {|
    /* app/foo/foo.ml */

    .a_hash_73765a197b {
      :not(.a_hash_73765a197b) {
        background-color: tomato;
      }
    }|})
    |xxx}];
  test
    [%expr
      stylesheet
        {|
        .a {
          :not(:where(.a)) {
            background-color: tomato;
          }
        }
       |}];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let __type_info_for_ppx_css :
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
    module type S  =
      sig
        module For_referencing : sig val a : string end
        val a : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing = struct let a = {|a_hash_b74f0748da|} end
        let a =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__009___b74f0748da__group_0;
                Virtual_dom.Vdom.Attr.class_ {|a_hash_b74f0748da|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__009___b74f0748da__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__009___b74f0748da__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__009___b74f0748da__0
           {|
    /* app/foo/foo.ml */

    .a_hash_b74f0748da {
      :not(:where(.a_hash_b74f0748da)) {
        background-color: tomato;
      }
    }|})
    |xxx}];
  test
    [%expr
      stylesheet
        {|
        .a {
          :not(:where(&)) {
            background-color: tomato;
          }
        }
       |}];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let __type_info_for_ppx_css :
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
    module type S  =
      sig
        module For_referencing : sig val a : string end
        val a : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing = struct let a = {|a_hash_3764702bf9|} end
        let a = Virtual_dom.Vdom.Attr.class_ {|a_hash_3764702bf9|}
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__010___3764702bf9__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__010___3764702bf9__0
        {|
    /* app/foo/foo.ml */

    .a_hash_3764702bf9 {
      :not(:where(&)) {
        background-color: tomato;
      }
    }|}
    |xxx}]
;;
