open Core
open Ppxlib
open Ppx_css
open Test_util

let loc = Test_util.loc_with_mock_name
let () = Ppx_css_syntax.Preprocess_arguments.set_lazy_loading_optimization (Some true)

let%expect_test "basic class" =
  test_struct
    [%expr
      stylesheet
        {|
     .foo {
       background-color: red;
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
        module For_referencing : sig val foo : string end
        val foo : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing = struct let foo = {|foo_hash_48f0d5a990|} end
        let foo =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__001___48f0d5a990__group_0;
                Virtual_dom.Vdom.Attr.class_ {|foo_hash_48f0d5a990|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__001___48f0d5a990__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__001___48f0d5a990__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__001___48f0d5a990__0
           {|
    /* app/foo/foo.ml */

    .foo_hash_48f0d5a990 {
      background-color: red;
    }|})
    |xxx}]
;;

let%expect_test "charset" =
  test_struct [%expr stylesheet {| @charset "UTF-8"; |}];
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

    Hoisted module:

    let sheet_x__002___ec8e944011__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__002___ec8e944011__0
        {|
    /* app/foo/foo.ml */

    @charset "UTF-8";|}
    |xxx}]
;;

let%expect_test "nested at rule" =
  test_struct
    [%expr
      stylesheet
        {|
@media screen and (min-width: 900px) {

  div#bar {
    color: red;
  }

  @media screen and (min-width: 900px) {
    article.my_foo {
      padding: 1rem 3rem;
    }
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
        module For_referencing : sig val bar : string val my_foo : string end
        val bar : Virtual_dom.Vdom.Attr.t
        val my_foo : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing =
          struct
            let bar = {|bar_hash_47750ccc00|}
            let my_foo = {|my_foo_hash_47750ccc00|}
          end
        let bar = Virtual_dom.Vdom.Attr.id {|bar_hash_47750ccc00|}
        let my_foo = Virtual_dom.Vdom.Attr.class_ {|my_foo_hash_47750ccc00|}
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__003___47750ccc00__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__003___47750ccc00__0
        {|
    /* app/foo/foo.ml */

    @media screen and (min-width: 900px) {
      div#bar_hash_47750ccc00 {
        color: red;
      }
      @media screen and (min-width: 900px) {
        article.my_foo_hash_47750ccc00 {
          padding: 1rem 3rem;
        }
      }
    }|}
    |xxx}]
;;

let%expect_test "at rule" =
  test_struct
    [%expr
      stylesheet
        {|
@media screen and (min-width: 900px) {
  article.my_foo {
    padding: 1rem 3rem;
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
        module For_referencing : sig val my_foo : string end
        val my_foo : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing =
          struct let my_foo = {|my_foo_hash_a834a8e1e6|} end
        let my_foo = Virtual_dom.Vdom.Attr.class_ {|my_foo_hash_a834a8e1e6|}
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__004___a834a8e1e6__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__004___a834a8e1e6__0
        {|
    /* app/foo/foo.ml */

    @media screen and (min-width: 900px) {
      article.my_foo_hash_a834a8e1e6 {
        padding: 1rem 3rem;
      }
    }|}
    |xxx}]
;;

let%expect_test "no mention of stylesheet function" =
  test_struct [%expr {| .a { }|}];
  [%expect
    {xxx| %css must contain a call to [?dont_hash:string list -> dont_hash_prefixes:string list -> string -> unit] |xxx}]
;;

let%expect_test "not a string" =
  test_struct [%expr stylesheet 5];
  [%expect
    {xxx| ppx_css found unexpected arguments. %css must contain a call to [?dont_hash:string list -> dont_hash_prefixes:string list -> string -> unit] |xxx}]
;;

let%expect_test "basic id" =
  test_struct
    [%expr
      stylesheet
        {|
     #foo {
       background-color: red;
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
        module For_referencing : sig val foo : string end
        val foo : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing = struct let foo = {|foo_hash_c4c785a1c3|} end
        let foo =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__005___c4c785a1c3__group_0;
                Virtual_dom.Vdom.Attr.id {|foo_hash_c4c785a1c3|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__005___c4c785a1c3__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__005___c4c785a1c3__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__005___c4c785a1c3__0
           {|
    /* app/foo/foo.ml */

    #foo_hash_c4c785a1c3 {
      background-color: red;
    }|})
    |xxx}]
;;

let%expect_test "missing closing brace" =
  test_struct
    [%expr
      stylesheet
        {|
     #foo {
       background-color: red;
      |}];
  [%expect {xxx| Error while parsing style block. Expected closing right brace |xxx}]
;;

let%expect_test "listed identifiers" =
  test_sig
    {|
    .a {}
    #b {}
    .c {}
|};
  [%expect
    {|
    sig
      module type S  =
        sig
          module For_referencing :
          sig val a : string val b : string val c : string end
          val a : Virtual_dom.Vdom.Attr.t
          val b : Virtual_dom.Vdom.Attr.t
          val c : Virtual_dom.Vdom.Attr.t
        end
      type t = (module S)
      val default : t
      module For_referencing :
      sig val a : string val b : string val c : string end
      val a : Virtual_dom.Vdom.Attr.t
      val b : Virtual_dom.Vdom.Attr.t
      val c : Virtual_dom.Vdom.Attr.t
    |}]
;;

let%expect_test "duplicates sig" =
  test_sig
    {|
    .a {}
    #a {}
    .b {}
  |};
  [%expect
    {|
    sig
      module type S  =
        sig
          module For_referencing : sig val a : string val b : string end
          val a : Virtual_dom.Vdom.Attr.t[@@alert
                                           unsafe
                                             "An id and a class both share the name \"a\" which is ambiguous. Please use \"a_id\" or \"a_class\" instead."]
          val a_id : Virtual_dom.Vdom.Attr.t
          val a_class : Virtual_dom.Vdom.Attr.t
          val b : Virtual_dom.Vdom.Attr.t
        end
      type t = (module S)
      val default : t
      module For_referencing : sig val a : string val b : string end
      val a : Virtual_dom.Vdom.Attr.t[@@alert
                                       unsafe
                                         "An id and a class both share the name \"a\" which is ambiguous. Please use \"a_id\" or \"a_class\" instead."]
      val a_id : Virtual_dom.Vdom.Attr.t
      val a_class : Virtual_dom.Vdom.Attr.t
      val b : Virtual_dom.Vdom.Attr.t
    |}]
;;

let%expect_test "politicians example" =
  test_sig {|.politicians {}|};
  [%expect
    {|
    sig
      module type S  =
        sig
          module For_referencing : sig val politicians : string end
          val politicians : Virtual_dom.Vdom.Attr.t
        end
      type t = (module S)
      val default : t
      module For_referencing : sig val politicians : string end
      val politicians : Virtual_dom.Vdom.Attr.t
    |}]
;;

let%expect_test "variables on signature generation" =
  test_sig
    {|
:root {
  --bg-color: black;
}

.card {
   color: var(--fg-color);
}
|};
  [%expect
    {|
    sig
      module type S  =
        sig
          module Variables :
          sig
            val set :
              ?bg_color:string ->
                ?fg_color:string -> unit -> Virtual_dom.Vdom.Attr.t
            val set_all :
              bg_color:string -> fg_color:string -> Virtual_dom.Vdom.Attr.t
          end
          module For_referencing :
          sig val bg_color : string val card : string val fg_color : string end
          val card : Virtual_dom.Vdom.Attr.t
        end
      type t = (module S)
      val default : t
      module Variables :
      sig
        val set :
          ?bg_color:string -> ?fg_color:string -> unit -> Virtual_dom.Vdom.Attr.t
        val set_all :
          bg_color:string -> fg_color:string -> Virtual_dom.Vdom.Attr.t
      end
      module For_referencing :
      sig val bg_color : string val card : string val fg_color : string end
      val card : Virtual_dom.Vdom.Attr.t
    |}]
;;

let%expect_test "animation" =
  test_struct
    [%expr
      stylesheet
        {|
.spinner {
  animation-name: spin;
  animation-duration: 5000ms;
  animation-iteration-count: infinite;
  animation-timing-function: linear;
}

@keyframes spin {
    from {
        transform:rotate(0deg);
    }
    to {
        transform:rotate(360deg);
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
        module For_referencing : sig val spinner : string end
        val spinner : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing =
          struct let spinner = {|spinner_hash_dfd410041d|} end
        let spinner =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__006___dfd410041d__group_0;
                Virtual_dom.Vdom.Attr.class_ {|spinner_hash_dfd410041d|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__006___dfd410041d__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__006___dfd410041d__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__006___dfd410041d__1
        {|
    /* app/foo/foo.ml */

    @keyframes spin {
      from {
        transform: rotate(0deg);
      }
      to {
        transform: rotate(360deg);
      }
    }|}
    let update_sheet_lazy_fn_x__006___dfd410041d__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__006___dfd410041d__0
           {|
    /* app/foo/foo.ml */

    .spinner_hash_dfd410041d {
      animation-name: spin;
      animation-duration: 5000ms;
      animation-iteration-count: infinite;
      animation-timing-function: linear;
    }|})
    |xxx}]
;;

let%expect_test "dont hash flag" =
  test_struct
    [%expr
      stylesheet
        {|
.hash-me {
  color : black;
 }

.dont-hash-me {
  color: green;
}

#hash-me-id {
  color: red;
}

#dont-hash-me-id {
  color: white;
}
|}
        ~dont_hash:[ "dont-hash-me"; "dont-hash-me-id" ]];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let __type_info_for_ppx_css :
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
    module type S  =
      sig
        module For_referencing :
        sig
          val dont_hash_me : string
          val dont_hash_me_id : string
          val hash_me : string
          val hash_me_id : string
        end
        val dont_hash_me : Virtual_dom.Vdom.Attr.t
        val dont_hash_me_id : Virtual_dom.Vdom.Attr.t
        val hash_me : Virtual_dom.Vdom.Attr.t
        val hash_me_id : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing =
          struct
            let dont_hash_me = {|dont-hash-me|}
            let dont_hash_me_id = {|dont-hash-me-id|}
            let hash_me = {|hash-me_hash_151702aef0|}
            let hash_me_id = {|hash-me-id_hash_151702aef0|}
          end
        let dont_hash_me = Virtual_dom.Vdom.Attr.class_ {|dont-hash-me|}
        let dont_hash_me_id = Virtual_dom.Vdom.Attr.id {|dont-hash-me-id|}
        let hash_me =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__007___151702aef0__group_1;
                Virtual_dom.Vdom.Attr.class_ {|hash-me_hash_151702aef0|}))
        let hash_me_id =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__007___151702aef0__group_0;
                Virtual_dom.Vdom.Attr.id {|hash-me-id_hash_151702aef0|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__007___151702aef0__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__007___151702aef0__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__007___151702aef0__2 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__007___151702aef0__3 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__007___151702aef0__1
        {|
    /* app/foo/foo.ml */

    .dont-hash-me {
      color: green;
    }|};
      Inline_css.Private.update_stylesheet sheet_x__007___151702aef0__3
        {|
    /* app/foo/foo.ml */

    #dont-hash-me-id {
      color: white;
    }|}
    let update_sheet_lazy_fn_x__007___151702aef0__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__007___151702aef0__2
           {|
    /* app/foo/foo.ml */

    #hash-me-id_hash_151702aef0 {
      color: red;
    }|})
    let update_sheet_lazy_fn_x__007___151702aef0__group_1 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__007___151702aef0__0
           {|
    /* app/foo/foo.ml */

    .hash-me_hash_151702aef0 {
      color: black;
    }|})
    |xxx}]
;;

let%expect_test "rewrite deprecated" =
  test_struct [%expr stylesheet {||} ~rewrite:[ "dont-hash-me"; 1 ]];
  [%expect
    {xxx|
    The use of ~rewrite is deprecated, please use ~dont_hash or ~dont_hash_prefixes
           instead. Alternatively, consider writing all of your CSS in the same %css
           stylesheet incantation. Deprecating ~rewrite allows for more optimiztions in ppx_css,
           at the expense of expressibility. We've audited bonsai apps and believe this expressibility
           was unused so we've removed it. If this conflicts with your use case please reach out.
    |xxx}];
  test_struct [%expr stylesheet {||} ~rewrite];
  [%expect
    {xxx|
    The use of ~rewrite is deprecated, please use ~dont_hash or ~dont_hash_prefixes
           instead. Alternatively, consider writing all of your CSS in the same %css
           stylesheet incantation. Deprecating ~rewrite allows for more optimiztions in ppx_css,
           at the expense of expressibility. We've audited bonsai apps and believe this expressibility
           was unused so we've removed it. If this conflicts with your use case please reach out.
    |xxx}];
  test_struct [%expr stylesheet {||} ~rewrite:foo];
  [%expect
    {|
    The use of ~rewrite is deprecated, please use ~dont_hash or ~dont_hash_prefixes
           instead. Alternatively, consider writing all of your CSS in the same %css
           stylesheet incantation. Deprecating ~rewrite allows for more optimiztions in ppx_css,
           at the expense of expressibility. We've audited bonsai apps and believe this expressibility
           was unused so we've removed it. If this conflicts with your use case please reach out.
    |}];
  test_struct [%expr stylesheet {||} ~rewrite:[ function_call "hi" ]];
  [%expect
    {|
    The use of ~rewrite is deprecated, please use ~dont_hash or ~dont_hash_prefixes
           instead. Alternatively, consider writing all of your CSS in the same %css
           stylesheet incantation. Deprecating ~rewrite allows for more optimiztions in ppx_css,
           at the expense of expressibility. We've audited bonsai apps and believe this expressibility
           was unused so we've removed it. If this conflicts with your use case please reach out.
    |}];
  test_struct [%expr stylesheet {||} ?rewrite:None];
  [%expect
    {| ppx_css found unexpected arguments. %css must contain a call to [?dont_hash:string list -> dont_hash_prefixes:string list -> string -> unit] |}]
;;

let%expect_test "Unsafe identifier collision through already existing identifier" =
  test_struct
    [%expr
      stylesheet
        {|
  .hello_world {
    color: white;
  }

  .hello-world {
    color: green;
  }|}];
  [%expect
    {xxx| Unsafe collisions of names. Two different css identifiers map to the same ocaml identifier which might lead to unintended results. Both 'hello_world' and 'hello-world' map to 'hello_world' |xxx}]
;;

let%expect_test "Unsafe identifier collision through newly minted identifiers" =
  test_struct
    [%expr
      stylesheet
        {|
  .hello-world_1 {
    color: white;
  }

  .hello_world-1 {
    color: green;
  }|}];
  [%expect
    {xxx| Unsafe collisions of names. Two different css identifiers map to the same ocaml identifier which might lead to unintended results. Both 'hello-world_1' and 'hello_world-1' map to 'hello_world_1' |xxx}];
  test_struct
    [%expr
      stylesheet
        {|
  :root {
    --hello-world-1: green;
  }

  .hello-world_1 {
    color: white;
  }
  |}];
  [%expect
    {xxx| Unsafe collisions of names. Two different css identifiers map to the same ocaml identifier which might lead to unintended results. Both '--hello-world-1' and 'hello-world_1' map to 'hello_world_1' |xxx}];
  test_sig
    {|
  :root {
    --hello-world-1: green;
  }

  .hello-world_1 {
    color: white;
  }
  |};
  [%expect
    {xxx|
    sig
      module type S  =
        sig
          module Variables :
          sig
            val set : ?hello_world_1:string -> unit -> Virtual_dom.Vdom.Attr.t
            val set_all : hello_world_1:string -> Virtual_dom.Vdom.Attr.t
          end
          module For_referencing : sig val hello_world_1 : string end
          val hello_world_1 : Virtual_dom.Vdom.Attr.t
        end
      type t = (module S)
      val default : t
      module Variables :
      sig
        val set : ?hello_world_1:string -> unit -> Virtual_dom.Vdom.Attr.t
        val set_all : hello_world_1:string -> Virtual_dom.Vdom.Attr.t
      end
      module For_referencing : sig val hello_world_1 : string end
      val hello_world_1 : Virtual_dom.Vdom.Attr.t
    |xxx}]
;;

let%expect_test "rewrite will work and turn into ~dont_hash if it's two constants that \
                 are the same"
  =
  test_struct
    [%expr
      stylesheet
        {| .a { color: green;
    }
  |}
        ~rewrite:[ "a", "a" ]];
  [%expect
    {xxx|
    The use of ~rewrite is deprecated, please use ~dont_hash or ~dont_hash_prefixes
           instead. Alternatively, consider writing all of your CSS in the same %css
           stylesheet incantation. Deprecating ~rewrite allows for more optimiztions in ppx_css,
           at the expense of expressibility. We've audited bonsai apps and believe this expressibility
           was unused so we've removed it. If this conflicts with your use case please reach out.
    |xxx}]
;;

let%expect_test "will rewrite if all values are same" =
  test_struct
    [%expr
      stylesheet
        {| .a { color: green;
    }
  |}
        ~rewrite:[ "a", "a"; "b", "b"; "c", "c"; "d", "different" ]];
  [%expect
    {xxx|
    The use of ~rewrite is deprecated, please use ~dont_hash or ~dont_hash_prefixes
           instead. Alternatively, consider writing all of your CSS in the same %css
           stylesheet incantation. Deprecating ~rewrite allows for more optimiztions in ppx_css,
           at the expense of expressibility. We've audited bonsai apps and believe this expressibility
           was unused so we've removed it. If this conflicts with your use case please reach out.
    |xxx}]
;;

(* NOTE: This test case checks an internal function of ppx_css which is weird, but the
   reason we want to assert that identifiers are iterated in the same order as
   they appeared which is useful for the way the[~reference] parameter. This behavior
   ordering behavior is already tested for in the above expect tests, but this
   is just extra assurance. *)
let%test_unit "ordering of mapping iteration is the same as the css string." =
  let open Quickcheck.Let_syntax in
  let id_generator =
    let%map id = Quickcheck.Generator.small_non_negative_int in
    [%string "class_%{id#Int}"]
  in
  let ids_generator = Quickcheck.Generator.list id_generator in
  Quickcheck.test ids_generator ~sexp_of:[%sexp_of: string list] ~f:(fun ids ->
    let css_string =
      String.concat
        ~sep:"\n"
        (List.map ids ~f:(fun identifier -> [%string ".%{identifier} {  }"]))
    in
    let style_sheet =
      Css_parser.(
        parse_stylesheet
          ~parsing_config:Parsing_config.raise_on_recoverable_errors
          css_string)
      |> Ppx_css.Stable_stylesheet.of_stylesheet
    in
    let traversed_ids = ref Reversed_list.[] in
    Ppx_css.For_testing.map_style_sheet
      style_sheet
      ~f:(fun (Css_identifier.Variable id | Class id | Id id) _ ->
        (traversed_ids := Reversed_list.(id :: !traversed_ids));
        id)
    |> (ignore : Ppx_css.Stable_stylesheet.t -> unit);
    let traversed_ids = Reversed_list.rev !traversed_ids in
    match List.equal String.equal traversed_ids ids with
    | true -> ()
    | false ->
      let ids, traversed_ids =
        Tuple2.map (ids, traversed_ids) ~f:(List.sexp_of_t String.sexp_of_t)
      in
      Expect_test_sexp_diff.print_sexp_diff ids traversed_ids;
      assert false)
;;

let%expect_test "unused [~dont_hash] target warning" =
  test_struct
    [%expr
      stylesheet
        {|
  .a { }
  |}
        ~dont_hash:[ "b" ]];
  [%expect {xxx| Unused keys: (b) |xxx}]
;;

module%test [@name "css_inliner_tests"] _ = struct
  let test_sig_and_struct s =
    let%tydi { ml_file = struct_; css_string_for_testing = _ } =
      For_css_inliner.gen_struct
        ~dont_hash:String.Set.empty
        ~css_string:s
        ~dont_hash_prefixes:[]
        ~stylesheet_location:loc
        ~lazy_loading_optimization:Ppx_css_syntax.Preprocess_arguments.Lazy_graph
        ~disable_hashing:false
    in
    let sig_ = For_css_inliner.gen_sig ~stylesheet_location:loc s in
    print_endline "==struct==";
    print_endline struct_;
    print_endline "==sig==";
    print_endline sig_
  ;;

  let%expect_test "basic generation" =
    test_sig_and_struct
      {|
        .a {
          color: green;
        }

        .b {
          color: white;
        } |};
    [%expect
      {xxx|
      ==struct==
      struct
        open
          struct
            [@@@ocaml.warning "-60"]
            module Ppx_css_hoister_do_not_collide =
              struct
                let sheet_x__008___414385fefe__0 =
                  let sheet = Inline_css.Private.create_stylesheet () in
                  Inline_css.Private.append_stylesheet sheet; sheet
                let sheet_x__008___414385fefe__1 =
                  let sheet = Inline_css.Private.create_stylesheet () in
                  Inline_css.Private.append_stylesheet sheet; sheet
                let update_sheet_lazy_fn_x__008___414385fefe__group_0 =
                  lazy
                    (Inline_css.Private.update_stylesheet
                       sheet_x__008___414385fefe__0
                       {|
      /* app/foo/foo.ml */

      .a_hash_414385fefe {
        color: green;
      }|})
                let update_sheet_lazy_fn_x__008___414385fefe__group_1 =
                  lazy
                    (Inline_css.Private.update_stylesheet
                       sheet_x__008___414385fefe__1
                       {|
      /* app/foo/foo.ml */

      .b_hash_414385fefe {
        color: white;
      }|})
              end
          end
        [@@@ocaml.warning "-32"]
        let __type_info_for_ppx_css :
          ?dont_hash:string list ->
            ?dont_hash_prefixes:string list -> string -> unit
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
              struct let a = {|a_hash_414385fefe|}
                     let b = {|b_hash_414385fefe|} end
            let a =
              Virtual_dom.Vdom.Attr.lazy_
                (lazy
                   (Inline_css.Ppx_css_runtime.force
                      Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__008___414385fefe__group_0;
                    Virtual_dom.Vdom.Attr.class_ {|a_hash_414385fefe|}))
            let b =
              Virtual_dom.Vdom.Attr.lazy_
                (lazy
                   (Inline_css.Ppx_css_runtime.force
                      Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__008___414385fefe__group_1;
                    Virtual_dom.Vdom.Attr.class_ {|b_hash_414385fefe|}))
          end
        include Default
        let default : t = (module Default)
      ==sig==
      sig
        module type S  =
          sig
            module For_referencing : sig val a : string val b : string end
            val a : Virtual_dom.Vdom.Attr.t
            val b : Virtual_dom.Vdom.Attr.t
          end
        type t = (module S)
        val default : t
        module For_referencing : sig val a : string val b : string end
        val a : Virtual_dom.Vdom.Attr.t
        val b : Virtual_dom.Vdom.Attr.t
      |xxx}]
  ;;

  let%expect_test "generation with duplicate names." =
    test_sig_and_struct
      {|
        .a {
          color: green;
        }

        .a {
          color: white;
        }
        |};
    [%expect
      {xxx|
      ==struct==
      struct
        open
          struct
            [@@@ocaml.warning "-60"]
            module Ppx_css_hoister_do_not_collide =
              struct
                let sheet_x__008___414385fefe__0 =
                  let sheet = Inline_css.Private.create_stylesheet () in
                  Inline_css.Private.append_stylesheet sheet; sheet
                let sheet_x__008___414385fefe__1 =
                  let sheet = Inline_css.Private.create_stylesheet () in
                  Inline_css.Private.append_stylesheet sheet; sheet
                let update_sheet_lazy_fn_x__008___414385fefe__group_0 =
                  lazy
                    (Inline_css.Private.update_stylesheet
                       sheet_x__008___414385fefe__0
                       {|
      /* app/foo/foo.ml */

      .a_hash_414385fefe {
        color: green;
      }|})
                let update_sheet_lazy_fn_x__008___414385fefe__group_1 =
                  lazy
                    (Inline_css.Private.update_stylesheet
                       sheet_x__008___414385fefe__1
                       {|
      /* app/foo/foo.ml */

      .b_hash_414385fefe {
        color: white;
      }|})
                let sheet_x__009___c9570a2a37__0 =
                  let sheet = Inline_css.Private.create_stylesheet () in
                  Inline_css.Private.append_stylesheet sheet; sheet
                let sheet_x__009___c9570a2a37__1 =
                  let sheet = Inline_css.Private.create_stylesheet () in
                  Inline_css.Private.append_stylesheet sheet; sheet
                let update_sheet_lazy_fn_x__009___c9570a2a37__group_0 =
                  lazy
                    (Inline_css.Private.update_stylesheet
                       sheet_x__009___c9570a2a37__0
                       {|
      /* app/foo/foo.ml */

      .a_hash_c9570a2a37 {
        color: green;
      }|};
                     Inline_css.Private.update_stylesheet
                       sheet_x__009___c9570a2a37__1
                       {|
      /* app/foo/foo.ml */

      .a_hash_c9570a2a37 {
        color: white;
      }|})
              end
          end
        [@@@ocaml.warning "-32"]
        let __type_info_for_ppx_css :
          ?dont_hash:string list ->
            ?dont_hash_prefixes:string list -> string -> unit
          = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
        module type S  =
          sig
            module For_referencing : sig val a : string end
            val a : Virtual_dom.Vdom.Attr.t
          end
        type t = (module S)
        module Default : S =
          struct
            module For_referencing = struct let a = {|a_hash_c9570a2a37|} end
            let a =
              Virtual_dom.Vdom.Attr.lazy_
                (lazy
                   (Inline_css.Ppx_css_runtime.force
                      Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__009___c9570a2a37__group_0;
                    Virtual_dom.Vdom.Attr.class_ {|a_hash_c9570a2a37|}))
          end
        include Default
        let default : t = (module Default)
      ==sig==
      sig
        module type S  =
          sig
            module For_referencing : sig val a : string end
            val a : Virtual_dom.Vdom.Attr.t
          end
        type t = (module S)
        val default : t
        module For_referencing : sig val a : string end
        val a : Virtual_dom.Vdom.Attr.t
      |xxx}]
  ;;
end

module%test [@name "Variable setter creation"] _ = struct
  let%expect_test "two variables variable creation" =
    test_sig
      {|
.a-class {
  --bg-color: white;
  --fg-color: black;
}
        |};
    [%expect
      {|
      sig
        module type S  =
          sig
            module Variables :
            sig
              val set :
                ?bg_color:string ->
                  ?fg_color:string -> unit -> Virtual_dom.Vdom.Attr.t
              val set_all :
                bg_color:string -> fg_color:string -> Virtual_dom.Vdom.Attr.t
            end
            module For_referencing :
            sig val a_class : string val bg_color : string val fg_color : string
            end
            val a_class : Virtual_dom.Vdom.Attr.t
          end
        type t = (module S)
        val default : t
        module Variables :
        sig
          val set :
            ?bg_color:string -> ?fg_color:string -> unit -> Virtual_dom.Vdom.Attr.t
          val set_all :
            bg_color:string -> fg_color:string -> Virtual_dom.Vdom.Attr.t
        end
        module For_referencing :
        sig val a_class : string val bg_color : string val fg_color : string end
        val a_class : Virtual_dom.Vdom.Attr.t
      |}]
  ;;
end

let%expect_test "unused [~dont_hash] target warning" =
  test_struct
    [%expr
      stylesheet
        {|
    .a { }
  |}
        ~dont_hash:[ "b" ]];
  [%expect {xxx| Unused keys: (b) |xxx}]
;;

let%expect_test "apostrophe syntax" =
  test_struct
    [%expr
      stylesheet
        ~don't_hash:[ "b" ]
        ~don't_hash_prefixes:[ "a" ]
        {|
    .b { }
    .bb { }
    .a { }
    .aa { }
  |}];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let __type_info_for_ppx_css :
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
    module type S  =
      sig
        module For_referencing :
        sig val a : string val aa : string val b : string val bb : string end
        val a : Virtual_dom.Vdom.Attr.t
        val aa : Virtual_dom.Vdom.Attr.t
        val b : Virtual_dom.Vdom.Attr.t
        val bb : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing =
          struct
            let a = {|a|}
            let aa = {|aa|}
            let b = {|b|}
            let bb = {|bb_hash_70849e24af|}
          end
        let a = Virtual_dom.Vdom.Attr.class_ {|a|}
        let aa = Virtual_dom.Vdom.Attr.class_ {|aa|}
        let b = Virtual_dom.Vdom.Attr.class_ {|b|}
        let bb =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__010___70849e24af__group_0;
                Virtual_dom.Vdom.Attr.class_ {|bb_hash_70849e24af|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__010___70849e24af__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__010___70849e24af__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__010___70849e24af__2 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__010___70849e24af__3 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      (Inline_css.Private.update_stylesheet sheet_x__010___70849e24af__0
         {|
    /* app/foo/foo.ml */

    .b {
    }|};
       Inline_css.Private.update_stylesheet sheet_x__010___70849e24af__2
         {|
    /* app/foo/foo.ml */

    .a {
    }|});
      Inline_css.Private.update_stylesheet sheet_x__010___70849e24af__3
        {|
    /* app/foo/foo.ml */

    .aa {
    }|}
    let update_sheet_lazy_fn_x__010___70849e24af__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__010___70849e24af__1
           {|
    /* app/foo/foo.ml */

    .bb_hash_70849e24af {
    }|})
    |xxx}]
;;

let%expect_test "Both don't_hash and dont_hash are used" =
  test_struct
    [%expr
      stylesheet
        ~don't_hash:[ "b" ]
        ~dont_hash:[ "b" ]
        {|
    .b { }
  |}];
  [%expect
    {| ppx_css found unexpected arguments. Found two uses of alternate syntax in the same place which might be ambiguous/result in unexpected results. Only use 1 of "dont_hash" or "don't_hash". |}]
;;

let%expect_test "ppx_css hashes variables" =
  test_struct
    [%expr
      stylesheet
        {| :root {
    --my-variable: green;
   }

    .a {
      background-color: var(--my-variable, default);

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
        module Variables :
        sig
          val set : ?my_variable:string -> unit -> Virtual_dom.Vdom.Attr.t
          val set_all : my_variable:string -> Virtual_dom.Vdom.Attr.t
        end
        module For_referencing : sig val a : string val my_variable : string end
        val a : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module Variables =
          struct
            let ppx_css_variable_set__014_ ?my_variable () =
              let ppx_css_acc__012_ = [] in
              let ppx_css_acc__012_ =
                match my_variable with
                | None -> ppx_css_acc__012_
                | Some ppx_css_value__013_ ->
                    ({|--my-variable_hash_66b0c989aa|}, ppx_css_value__013_) ::
                    ppx_css_acc__012_ in
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__012_
            let set = ppx_css_variable_set__014_
            let set_all ~my_variable = ppx_css_variable_set__014_ () ~my_variable
          end
        module For_referencing =
          struct
            let a = {|a_hash_66b0c989aa|}
            let my_variable = {|--my-variable_hash_66b0c989aa|}
          end
        let a =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__011___66b0c989aa__group_0;
                Virtual_dom.Vdom.Attr.class_ {|a_hash_66b0c989aa|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__011___66b0c989aa__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__011___66b0c989aa__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__011___66b0c989aa__0
        {|
    /* app/foo/foo.ml */

    :root {
      --my-variable_hash_66b0c989aa: green;
    }|}
    let update_sheet_lazy_fn_x__011___66b0c989aa__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__011___66b0c989aa__1
           {|
    /* app/foo/foo.ml */

    .a_hash_66b0c989aa {
      background-color: var(--my-variable_hash_66b0c989aa, default);
    }|})
    |xxx}]
;;

let%expect_test "nested variables" =
  test_struct
    [%expr
      stylesheet
        {| :root {
    --a: green;
    --b: green;
    --c: green;
   }

    .navbar {
      background-color: var(--a, var(--b, (var(--c))));
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
        module Variables :
        sig
          val set :
            ?a:string ->
              ?b:string -> ?c:string -> unit -> Virtual_dom.Vdom.Attr.t
          val set_all :
            a:string -> b:string -> c:string -> Virtual_dom.Vdom.Attr.t
        end
        module For_referencing :
        sig val a : string val b : string val c : string val navbar : string end
        val navbar : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module Variables =
          struct
            let ppx_css_variable_set__018_ ?a ?b ?c () =
              let ppx_css_acc__016_ = [] in
              let ppx_css_acc__016_ =
                match a with
                | None -> ppx_css_acc__016_
                | Some ppx_css_value__017_ ->
                    ({|--a_hash_577da54a4b|}, ppx_css_value__017_) ::
                    ppx_css_acc__016_ in
              let ppx_css_acc__016_ =
                match b with
                | None -> ppx_css_acc__016_
                | Some ppx_css_value__017_ ->
                    ({|--b_hash_577da54a4b|}, ppx_css_value__017_) ::
                    ppx_css_acc__016_ in
              let ppx_css_acc__016_ =
                match c with
                | None -> ppx_css_acc__016_
                | Some ppx_css_value__017_ ->
                    ({|--c_hash_577da54a4b|}, ppx_css_value__017_) ::
                    ppx_css_acc__016_ in
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__016_
            let set = ppx_css_variable_set__018_
            let set_all ~a ~b ~c = ppx_css_variable_set__018_ () ~a ~b ~c
          end
        module For_referencing =
          struct
            let a = {|--a_hash_577da54a4b|}
            let b = {|--b_hash_577da54a4b|}
            let c = {|--c_hash_577da54a4b|}
            let navbar = {|navbar_hash_577da54a4b|}
          end
        let navbar =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__015___577da54a4b__group_0;
                Virtual_dom.Vdom.Attr.class_ {|navbar_hash_577da54a4b|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__015___577da54a4b__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__015___577da54a4b__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__015___577da54a4b__0
        {|
    /* app/foo/foo.ml */

    :root {
      --a_hash_577da54a4b: green;
      --b_hash_577da54a4b: green;
      --c_hash_577da54a4b: green;
    }|}
    let update_sheet_lazy_fn_x__015___577da54a4b__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__015___577da54a4b__1
           {|
    /* app/foo/foo.ml */

    .navbar_hash_577da54a4b {
      background-color: var(--a_hash_577da54a4b, var(--b_hash_577da54a4b, (var(--c_hash_577da54a4b))));
    }|})
    |xxx}]
;;

let%expect_test "Does not hash invalid places for selectors." =
  test_struct
    [%expr
      stylesheet
        {|
    has(.a) {
      background-color: :has(.a);
      background-color: has(.a);
    }
  |}];
  [%expect
    {xxx| Error while parsing selector. Expected start of type selector but got FUNCTION(has) |xxx}]
;;

let%expect_test "collision of names between ids and classes results in an alert." =
  test_struct
    [%expr
      stylesheet
        {|
#foo {}
.foo {}
  |}];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let __type_info_for_ppx_css :
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
    module type S  =
      sig
        module For_referencing : sig val foo : string end
        val foo : Virtual_dom.Vdom.Attr.t[@@alert
                                           unsafe
                                             "An id and a class both share the name \"foo\" which is ambiguous. Please use \"foo_id\" or \"foo_class\" instead."]
        val foo_id : Virtual_dom.Vdom.Attr.t
        val foo_class : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing = struct let foo = {|foo_hash_b313a0854a|} end
        let foo = Virtual_dom.Vdom.Attr.empty
        let foo_class =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__019___b313a0854a__group_1;
                Virtual_dom.Vdom.Attr.class_ {|foo_hash_b313a0854a|}))
        let foo_id =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__019___b313a0854a__group_0;
                Virtual_dom.Vdom.Attr.id {|foo_hash_b313a0854a|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__019___b313a0854a__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__019___b313a0854a__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__019___b313a0854a__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__019___b313a0854a__0
           {|
    /* app/foo/foo.ml */

    #foo_hash_b313a0854a {
    }|})
    let update_sheet_lazy_fn_x__019___b313a0854a__group_1 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__019___b313a0854a__1
           {|
    /* app/foo/foo.ml */

    .foo_hash_b313a0854a {
    }|})
    |xxx}]
;;

let%expect_test "collision of names between ids and classes colliding with a third party \
                 identifier elsewhere results in an error"
  =
  test_struct
    [%expr
      stylesheet
        {|
#foo {}
.foo {}
.foo_class {}
.foo_id {}
  |}];
  [%expect
    {xxx| Collision between identifiers! This occurs when a disambiguated identifier matches an existing identifier. To resolve this, rename the following identifiers: (foo_class foo_id). |xxx}]
;;

let%expect_test "behavior on sharing of id and class names" =
  test_struct
    [%expr
      stylesheet
        {|
    .a {}
    #a {}
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
        val a : Virtual_dom.Vdom.Attr.t[@@alert
                                         unsafe
                                           "An id and a class both share the name \"a\" which is ambiguous. Please use \"a_id\" or \"a_class\" instead."]
        val a_id : Virtual_dom.Vdom.Attr.t
        val a_class : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing = struct let a = {|a_hash_ff77223dd5|} end
        let a = Virtual_dom.Vdom.Attr.empty
        let a_class =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__021___ff77223dd5__group_1;
                Virtual_dom.Vdom.Attr.class_ {|a_hash_ff77223dd5|}))
        let a_id =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__021___ff77223dd5__group_0;
                Virtual_dom.Vdom.Attr.id {|a_hash_ff77223dd5|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__021___ff77223dd5__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__021___ff77223dd5__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__021___ff77223dd5__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__021___ff77223dd5__1
           {|
    /* app/foo/foo.ml */

    #a_hash_ff77223dd5 {
    }|})
    let update_sheet_lazy_fn_x__021___ff77223dd5__group_1 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__021___ff77223dd5__0
           {|
    /* app/foo/foo.ml */

    .a_hash_ff77223dd5 {
    }|})
    |xxx}];
  test_sig
    {|
    .a {}
    #a {}
                    |};
  [%expect
    {|
    sig
      module type S  =
        sig
          module For_referencing : sig val a : string end
          val a : Virtual_dom.Vdom.Attr.t[@@alert
                                           unsafe
                                             "An id and a class both share the name \"a\" which is ambiguous. Please use \"a_id\" or \"a_class\" instead."]
          val a_id : Virtual_dom.Vdom.Attr.t
          val a_class : Virtual_dom.Vdom.Attr.t
        end
      type t = (module S)
      val default : t
      module For_referencing : sig val a : string end
      val a : Virtual_dom.Vdom.Attr.t[@@alert
                                       unsafe
                                         "An id and a class both share the name \"a\" which is ambiguous. Please use \"a_id\" or \"a_class\" instead."]
      val a_id : Virtual_dom.Vdom.Attr.t
      val a_class : Virtual_dom.Vdom.Attr.t
    |}]
;;

let%expect_test "dont_hash" =
  test_struct
    [%expr
      stylesheet
        {|
    .a-class {
      --a-variable: red;
    }

    #an_id {}
                    |}
        ~dont_hash:[ "--a-variable"; "an_id"; "a-class" ]];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let __type_info_for_ppx_css :
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
    module type S  =
      sig
        module Variables :
        sig
          val set : ?a_variable:string -> unit -> Virtual_dom.Vdom.Attr.t
          val set_all : a_variable:string -> Virtual_dom.Vdom.Attr.t
        end
        module For_referencing :
        sig val a_class : string val a_variable : string val an_id : string end
        val a_class : Virtual_dom.Vdom.Attr.t
        val an_id : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module Variables =
          struct
            let ppx_css_variable_set__025_ ?a_variable () =
              let ppx_css_acc__023_ = [] in
              let ppx_css_acc__023_ =
                match a_variable with
                | None -> ppx_css_acc__023_
                | Some ppx_css_value__024_ ->
                    ({|--a-variable|}, ppx_css_value__024_) :: ppx_css_acc__023_ in
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__023_
            let set = ppx_css_variable_set__025_
            let set_all ~a_variable = ppx_css_variable_set__025_ () ~a_variable
          end
        module For_referencing =
          struct
            let a_class = {|a-class|}
            let a_variable = {|--a-variable|}
            let an_id = {|an_id|}
          end
        let a_class = Virtual_dom.Vdom.Attr.class_ {|a-class|}
        let an_id = Virtual_dom.Vdom.Attr.id {|an_id|}
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__022___a5675496ea__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__022___a5675496ea__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__022___a5675496ea__0
        {|
    /* app/foo/foo.ml */

    .a-class {
      --a-variable: red;
    }|};
      Inline_css.Private.update_stylesheet sheet_x__022___a5675496ea__1
        {|
    /* app/foo/foo.ml */

    #an_id {
    }|}
    |xxx}]
;;

let%expect_test "Clashes caused by the rename to snake case from kebab case trigger a \
                 warning"
  =
  test_struct [%expr stylesheet {| .a-a {}  .a_a {} |}];
  [%expect
    {xxx| Unsafe collisions of names. Two different css identifiers map to the same ocaml identifier which might lead to unintended results. Both 'a-a' and 'a_a' map to 'a_a' |xxx}]
;;

let%expect_test "Unused warnings also apply to [dont_hash]" =
  test_struct [%expr stylesheet {| .a {} |} ~dont_hash:[ "b" ]];
  [%expect {xxx| Unused keys: (b) |xxx}]
;;

let%expect_test "Duplicate values given to [dont_hash]" =
  test_struct [%expr stylesheet {| .a-a {}  |} ~dont_hash:[ "a-a"; "a-a" ]];
  [%expect {| Found duplicate values (a-a) inside of [dont_hash]. |}]
;;

let%expect_test "[dont_hash] syntax error" =
  test_struct [%expr stylesheet {| .a {}  |} ~dont_hash:[ not_a_string_constant ]];
  [%expect
    {|
    The dont_hash argument to 'stylesheet' must be called with a list literal containing string literals.

    example:
      stylesheet ~dont_hash:[ "foo_bar" ] (* Does not hash instances of "foo_bar". *)
      stylesheet ~dont_hash:[ "--bg-color" ] (* Does not hash instances of "--bg-color". *)
    |}]
;;

let%expect_test "[dont_hash] syntax error" =
  test_struct [%expr stylesheet {| .a {}  |} ~dont_hash:not_a_string_literal];
  [%expect
    {|
    The dont_hash argument to 'stylesheet' must be called with a list literal containing string literals.

    example:
      stylesheet ~dont_hash:[ "foo_bar" ] (* Does not hash instances of "foo_bar". *)
      stylesheet ~dont_hash:[ "--bg-color" ] (* Does not hash instances of "--bg-color". *)
    |}]
;;

let%expect_test "dont_hash_prefixes" =
  test_struct
    [%expr
      stylesheet
        {|
    .a {
      --bg-color: red;
      --fg-color: blue;
    }
                    |}
        ~dont_hash_prefixes:[ "--bg" ]];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let __type_info_for_ppx_css :
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
    module type S  =
      sig
        module Variables :
        sig
          val set :
            ?bg_color:string ->
              ?fg_color:string -> unit -> Virtual_dom.Vdom.Attr.t
          val set_all :
            bg_color:string -> fg_color:string -> Virtual_dom.Vdom.Attr.t
        end
        module For_referencing :
        sig val a : string val bg_color : string val fg_color : string end
        val a : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module Variables =
          struct
            let ppx_css_variable_set__029_ ?bg_color ?fg_color () =
              let ppx_css_acc__027_ = [] in
              let ppx_css_acc__027_ =
                match bg_color with
                | None -> ppx_css_acc__027_
                | Some ppx_css_value__028_ ->
                    ({|--bg-color|}, ppx_css_value__028_) :: ppx_css_acc__027_ in
              let ppx_css_acc__027_ =
                match fg_color with
                | None -> ppx_css_acc__027_
                | Some ppx_css_value__028_ ->
                    ({|--fg-color_hash_853e9b013a|}, ppx_css_value__028_) ::
                    ppx_css_acc__027_ in
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__027_
            let set = ppx_css_variable_set__029_
            let set_all ~bg_color ~fg_color =
              ppx_css_variable_set__029_ () ~bg_color ~fg_color
          end
        module For_referencing =
          struct
            let a = {|a_hash_853e9b013a|}
            let bg_color = {|--bg-color|}
            let fg_color = {|--fg-color_hash_853e9b013a|}
          end
        let a =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__026___853e9b013a__group_0;
                Virtual_dom.Vdom.Attr.class_ {|a_hash_853e9b013a|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__026___853e9b013a__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__026___853e9b013a__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__026___853e9b013a__0
           {|
    /* app/foo/foo.ml */

    .a_hash_853e9b013a {
      --bg-color: red;
      --fg-color_hash_853e9b013a: blue;
    }|})
    |xxx}]
;;

let%expect_test "dont_hash_prefixes accidental shadowing" =
  test_struct
    [%expr
      stylesheet
        {|
    .a {
      --bg-color: red;
      --fg-color: blue;
    }
                    |}
        ~dont_hash_prefixes:[ "--bg"; "--" ]];
  [%expect {xxx| Unused prefixes: (--bg) |xxx}]
;;

let%expect_test "dont_hash_prefixes no mention of prefixes" =
  test_struct
    [%expr
      stylesheet
        {|
    .a { }
                    |}
        ~dont_hash_prefixes:[ "--" ]];
  [%expect {xxx| Unused prefixes: (--) |xxx}]
;;

let%expect_test "dont_hash_prefixes two different prefixes" =
  test_struct
    [%expr
      stylesheet
        {|
    .a {
      --aa: red;
    }

    .b {
      --bb: blue;
    }


                    |}
        ~dont_hash_prefixes:[ "--aa"; "--bb" ]];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let __type_info_for_ppx_css :
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
    module type S  =
      sig
        module Variables :
        sig
          val set : ?aa:string -> ?bb:string -> unit -> Virtual_dom.Vdom.Attr.t
          val set_all : aa:string -> bb:string -> Virtual_dom.Vdom.Attr.t
        end
        module For_referencing :
        sig val a : string val aa : string val b : string val bb : string end
        val a : Virtual_dom.Vdom.Attr.t
        val b : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module Variables =
          struct
            let ppx_css_variable_set__033_ ?aa ?bb () =
              let ppx_css_acc__031_ = [] in
              let ppx_css_acc__031_ =
                match aa with
                | None -> ppx_css_acc__031_
                | Some ppx_css_value__032_ -> ({|--aa|}, ppx_css_value__032_) ::
                    ppx_css_acc__031_ in
              let ppx_css_acc__031_ =
                match bb with
                | None -> ppx_css_acc__031_
                | Some ppx_css_value__032_ -> ({|--bb|}, ppx_css_value__032_) ::
                    ppx_css_acc__031_ in
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__031_
            let set = ppx_css_variable_set__033_
            let set_all ~aa ~bb = ppx_css_variable_set__033_ () ~aa ~bb
          end
        module For_referencing =
          struct
            let a = {|a_hash_908634fc45|}
            let aa = {|--aa|}
            let b = {|b_hash_908634fc45|}
            let bb = {|--bb|}
          end
        let a =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__030___908634fc45__group_0;
                Virtual_dom.Vdom.Attr.class_ {|a_hash_908634fc45|}))
        let b =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__030___908634fc45__group_1;
                Virtual_dom.Vdom.Attr.class_ {|b_hash_908634fc45|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__030___908634fc45__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__030___908634fc45__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__030___908634fc45__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__030___908634fc45__0
           {|
    /* app/foo/foo.ml */

    .a_hash_908634fc45 {
      --aa: red;
    }|})
    let update_sheet_lazy_fn_x__030___908634fc45__group_1 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__030___908634fc45__1
           {|
    /* app/foo/foo.ml */

    .b_hash_908634fc45 {
      --bb: blue;
    }|})
    |xxx}]
;;

let%expect_test "[dont_hash_prefixes] syntax error" =
  test_struct
    [%expr stylesheet {| .a {}  |} ~dont_hash_prefixes:[ not_a_string_constant ]];
  [%expect
    {|
    The dont_hash_prefixes argument to 'stylesheet' must be called with a list literal containing string literals.

    example:
      stylesheet ~dont_hash_prefixes:[ "--bg" ] (* Does not hashes identifiers that start with "--bg" (e.g. "--bg-color"). *)
      stylesheet ~dont_hash_prefixes:[ "--" ] (* Does not hash css variables. *)
    |}]
;;

let%expect_test "[dont_hash_prefixes] syntax error" =
  test_struct [%expr stylesheet {| .a {}  |} ~dont_hash_prefixes:not_a_string_literal];
  [%expect
    {|
    The dont_hash_prefixes argument to 'stylesheet' must be called with a list literal containing string literals.

    example:
      stylesheet ~dont_hash_prefixes:[ "--bg" ] (* Does not hashes identifiers that start with "--bg" (e.g. "--bg-color"). *)
      stylesheet ~dont_hash_prefixes:[ "--" ] (* Does not hash css variables. *)
    |}]
;;

let%expect_test "Unsafe hashing warning is also blocked by [~dont_hash_prefixes]" =
  test_struct
    [%expr
      stylesheet
        {|
                :root {
                 color:var(--cm-bg-color);
                }
       |}
        ~dont_hash_prefixes:[ "--cm" ]];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let __type_info_for_ppx_css :
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
    module type S  =
      sig
        module Variables :
        sig
          val set : ?cm_bg_color:string -> unit -> Virtual_dom.Vdom.Attr.t
          val set_all : cm_bg_color:string -> Virtual_dom.Vdom.Attr.t
        end
        module For_referencing : sig val cm_bg_color : string end
      end
    type t = (module S)
    module Default : S =
      struct
        module Variables =
          struct
            let ppx_css_variable_set__037_ ?cm_bg_color () =
              let ppx_css_acc__035_ = [] in
              let ppx_css_acc__035_ =
                match cm_bg_color with
                | None -> ppx_css_acc__035_
                | Some ppx_css_value__036_ ->
                    ({|--cm-bg-color|}, ppx_css_value__036_) :: ppx_css_acc__035_ in
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__035_
            let set = ppx_css_variable_set__037_
            let set_all ~cm_bg_color = ppx_css_variable_set__037_ () ~cm_bg_color
          end
        module For_referencing = struct let cm_bg_color = {|--cm-bg-color|} end
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__034___d115ed8389__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__034___d115ed8389__0
        {|
    /* app/foo/foo.ml */

    :root {
      color: var(--cm-bg-color);
    }|}
    |xxx}]
;;

let%expect_test "Scary attributes shouldn't be hashed even if they look like selectors" =
  test_struct
    [%expr
      stylesheet
        {|
        div:asdf(".m .n .o") {}
        div:asdf(.d .e .f) {}
        div:qwerty(.t .q .r) {}
       |}];
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

    Hoisted module:

    let sheet_x__038___2e70e65706__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__038___2e70e65706__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__038___2e70e65706__2 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      (Inline_css.Private.update_stylesheet sheet_x__038___2e70e65706__0
         {|
    /* app/foo/foo.ml */

    div:asdf(".m .n .o") {
    }|};
       Inline_css.Private.update_stylesheet sheet_x__038___2e70e65706__1
         {|
    /* app/foo/foo.ml */

    div:asdf(.d .e .f) {
    }|});
      Inline_css.Private.update_stylesheet sheet_x__038___2e70e65706__2
        {|
    /* app/foo/foo.ml */

    div:qwerty(.t .q .r) {
    }|}
    |xxx}]
;;

let%expect_test "Scary attributes shouldn't be hashed" =
  test_struct
    [%expr
      stylesheet
        {|
        div[has=".x .y .z"] {}
        div[attr='.a .b .c'] {}
        div[href=#] {}
       |}];
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

    Hoisted module:

    let sheet_x__039___30526555a5__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__039___30526555a5__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__039___30526555a5__2 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      (Inline_css.Private.update_stylesheet sheet_x__039___30526555a5__0
         {|
    /* app/foo/foo.ml */

    div[has=".x .y .z"] {
    }|};
       Inline_css.Private.update_stylesheet sheet_x__039___30526555a5__1
         {|
    /* app/foo/foo.ml */

    div[attr='.a .b .c'] {
    }|});
      Inline_css.Private.update_stylesheet sheet_x__039___30526555a5__2
        {|
    /* app/foo/foo.ml */

    div[href=#] {
    }|}
    |xxx}]
;;

let%expect_test "classname and id's with the same name, but different auto-forcing \
                 behaviors"
  =
  test_struct
    [%expr
      stylesheet
        {|
    :not(#a) {}
    .a {}
    :not(.b) {}
    #b {}
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
        val a : Virtual_dom.Vdom.Attr.t[@@alert
                                         unsafe
                                           "An id and a class both share the name \"a\" which is ambiguous. Please use \"a_id\" or \"a_class\" instead."]
        val a_id : Virtual_dom.Vdom.Attr.t
        val a_class : Virtual_dom.Vdom.Attr.t
        val b : Virtual_dom.Vdom.Attr.t[@@alert
                                         unsafe
                                           "An id and a class both share the name \"b\" which is ambiguous. Please use \"b_id\" or \"b_class\" instead."]
        val b_id : Virtual_dom.Vdom.Attr.t
        val b_class : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing =
          struct let a = {|a_hash_34f4bb1372|}
                 let b = {|b_hash_34f4bb1372|} end
        let a = Virtual_dom.Vdom.Attr.empty
        let a_class =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__040___34f4bb1372__group_1;
                Virtual_dom.Vdom.Attr.class_ {|a_hash_34f4bb1372|}))
        let a_id = Virtual_dom.Vdom.Attr.id {|a_hash_34f4bb1372|}
        let b = Virtual_dom.Vdom.Attr.empty
        let b_class = Virtual_dom.Vdom.Attr.class_ {|b_hash_34f4bb1372|}
        let b_id =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__040___34f4bb1372__group_0;
                Virtual_dom.Vdom.Attr.id {|b_hash_34f4bb1372|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__040___34f4bb1372__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__040___34f4bb1372__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__040___34f4bb1372__2 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__040___34f4bb1372__3 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__040___34f4bb1372__0
        {|
    /* app/foo/foo.ml */

    :not(#a_hash_34f4bb1372) {
    }|};
      Inline_css.Private.update_stylesheet sheet_x__040___34f4bb1372__2
        {|
    /* app/foo/foo.ml */

    :not(.b_hash_34f4bb1372) {
    }|}
    let update_sheet_lazy_fn_x__040___34f4bb1372__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__040___34f4bb1372__3
           {|
    /* app/foo/foo.ml */

    #b_hash_34f4bb1372 {
    }|})
    let update_sheet_lazy_fn_x__040___34f4bb1372__group_1 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__040___34f4bb1372__1
           {|
    /* app/foo/foo.ml */

    .a_hash_34f4bb1372 {
    }|})
    |xxx}]
;;

let%expect_test "classnames and items - transitive" =
  test_struct
    [%expr
      stylesheet
        {|
      .a       .b {}
      :not(.a) .b {}
      :not(#a) #b {}
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
        val a : Virtual_dom.Vdom.Attr.t[@@alert
                                         unsafe
                                           "An id and a class both share the name \"a\" which is ambiguous. Please use \"a_id\" or \"a_class\" instead."]
        val a_id : Virtual_dom.Vdom.Attr.t
        val a_class : Virtual_dom.Vdom.Attr.t
        val b : Virtual_dom.Vdom.Attr.t[@@alert
                                         unsafe
                                           "An id and a class both share the name \"b\" which is ambiguous. Please use \"b_id\" or \"b_class\" instead."]
        val b_id : Virtual_dom.Vdom.Attr.t
        val b_class : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing =
          struct let a = {|a_hash_dba1924c19|}
                 let b = {|b_hash_dba1924c19|} end
        let a = Virtual_dom.Vdom.Attr.empty
        let a_class =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__041___dba1924c19__group_1;
                Virtual_dom.Vdom.Attr.class_ {|a_hash_dba1924c19|}))
        let a_id = Virtual_dom.Vdom.Attr.id {|a_hash_dba1924c19|}
        let b = Virtual_dom.Vdom.Attr.empty
        let b_class =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__041___dba1924c19__group_1;
                Virtual_dom.Vdom.Attr.class_ {|b_hash_dba1924c19|}))
        let b_id =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__041___dba1924c19__group_0;
                Virtual_dom.Vdom.Attr.id {|b_hash_dba1924c19|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__041___dba1924c19__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__041___dba1924c19__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__041___dba1924c19__2 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__041___dba1924c19__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__041___dba1924c19__2
           {|
    /* app/foo/foo.ml */

    :not(#a_hash_dba1924c19) #b_hash_dba1924c19 {
    }|})
    let update_sheet_lazy_fn_x__041___dba1924c19__group_1 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__041___dba1924c19__0
           {|
    /* app/foo/foo.ml */

    .a_hash_dba1924c19 .b_hash_dba1924c19 {
    }|};
         Inline_css.Private.update_stylesheet sheet_x__041___dba1924c19__1
           {|
    /* app/foo/foo.ml */

    :not(.a_hash_dba1924c19) .b_hash_dba1924c19 {
    }|})
    |xxx}]
;;

let%expect_test "String attrs with identifiers should not be hashed" =
  test_struct
    [%expr
      stylesheet
        {|
                div:asdf('.a .b .c') { }
                div:asdf(".a .b .c") { }
                div[foo=".a .b .c"] { }
                div[bar='.a .b .c'] { }
       |}];
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

    Hoisted module:

    let sheet_x__042___d29657b04e__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__042___d29657b04e__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__042___d29657b04e__2 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__042___d29657b04e__3 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      ((Inline_css.Private.update_stylesheet sheet_x__042___d29657b04e__0
          {|
    /* app/foo/foo.ml */

    div:asdf('.a .b .c') {
    }|};
        Inline_css.Private.update_stylesheet sheet_x__042___d29657b04e__1
          {|
    /* app/foo/foo.ml */

    div:asdf(".a .b .c") {
    }|});
       Inline_css.Private.update_stylesheet sheet_x__042___d29657b04e__2
         {|
    /* app/foo/foo.ml */

    div[foo=".a .b .c"] {
    }|});
      Inline_css.Private.update_stylesheet sheet_x__042___d29657b04e__3
        {|
    /* app/foo/foo.ml */

    div[bar='.a .b .c'] {
    }|}
    |xxx}]
;;

let%expect_test "Known  css functions with identifiers should hash" =
  test_struct
    [%expr
      stylesheet
        {|
                div:has(.a .b) {

                }
                div:not(.c + .d) {

                }
                div:where(.e ~ .f) {

                }
                div:is(.g > .h) {

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
        module For_referencing :
        sig
          val a : string
          val b : string
          val c : string
          val d : string
          val e : string
          val f : string
          val g : string
          val h : string
        end
        val a : Virtual_dom.Vdom.Attr.t
        val b : Virtual_dom.Vdom.Attr.t
        val c : Virtual_dom.Vdom.Attr.t
        val d : Virtual_dom.Vdom.Attr.t
        val e : Virtual_dom.Vdom.Attr.t
        val f : Virtual_dom.Vdom.Attr.t
        val g : Virtual_dom.Vdom.Attr.t
        val h : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing =
          struct
            let a = {|a_hash_686651dd8b|}
            let b = {|b_hash_686651dd8b|}
            let c = {|c_hash_686651dd8b|}
            let d = {|d_hash_686651dd8b|}
            let e = {|e_hash_686651dd8b|}
            let f = {|f_hash_686651dd8b|}
            let g = {|g_hash_686651dd8b|}
            let h = {|h_hash_686651dd8b|}
          end
        let a =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__043___686651dd8b__group_0;
                Virtual_dom.Vdom.Attr.class_ {|a_hash_686651dd8b|}))
        let b =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__043___686651dd8b__group_0;
                Virtual_dom.Vdom.Attr.class_ {|b_hash_686651dd8b|}))
        let c = Virtual_dom.Vdom.Attr.class_ {|c_hash_686651dd8b|}
        let d = Virtual_dom.Vdom.Attr.class_ {|d_hash_686651dd8b|}
        let e = Virtual_dom.Vdom.Attr.class_ {|e_hash_686651dd8b|}
        let f = Virtual_dom.Vdom.Attr.class_ {|f_hash_686651dd8b|}
        let g = Virtual_dom.Vdom.Attr.class_ {|g_hash_686651dd8b|}
        let h = Virtual_dom.Vdom.Attr.class_ {|h_hash_686651dd8b|}
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__043___686651dd8b__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__043___686651dd8b__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__043___686651dd8b__2 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__043___686651dd8b__3 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      (Inline_css.Private.update_stylesheet sheet_x__043___686651dd8b__1
         {|
    /* app/foo/foo.ml */

    div:not(.c_hash_686651dd8b + .d_hash_686651dd8b) {
    }|};
       Inline_css.Private.update_stylesheet sheet_x__043___686651dd8b__2
         {|
    /* app/foo/foo.ml */

    div:where(.e_hash_686651dd8b ~ .f_hash_686651dd8b) {
    }|});
      Inline_css.Private.update_stylesheet sheet_x__043___686651dd8b__3
        {|
    /* app/foo/foo.ml */

    div:is(.g_hash_686651dd8b > .h_hash_686651dd8b) {
    }|}
    let update_sheet_lazy_fn_x__043___686651dd8b__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__043___686651dd8b__0
           {|
    /* app/foo/foo.ml */

    div:has(.a_hash_686651dd8b .b_hash_686651dd8b) {
    }|})
    |xxx}]
;;

let%expect_test "Doesn't throw on pseudoclasses without selectors" =
  test_struct
    [%expr
      stylesheet
        {|
                div:asdf() { }
                div:bar { }
       |}];
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

    Hoisted module:

    let sheet_x__044___60b0c55bdb__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__044___60b0c55bdb__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__044___60b0c55bdb__0
        {|
    /* app/foo/foo.ml */

    div:asdf() {
    }|};
      Inline_css.Private.update_stylesheet sheet_x__044___60b0c55bdb__1
        {|
    /* app/foo/foo.ml */

    div:bar {
    }|}
    |xxx}]
;;
