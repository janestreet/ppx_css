open Core
open Ppxlib
open Ppx_css
open Css_jane
open Test_util

let loc = Location.none

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
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
    module type S  =
      sig
        module For_referencing : sig val foo : string end
        val foo : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing = struct let foo = {|foo_hash_81f5a2c98f|} end
        let foo =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__001___81f5a2c98f__group_0;
                Virtual_dom.Vdom.Attr.class_ {|foo_hash_81f5a2c98f|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__001___81f5a2c98f__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__001___81f5a2c98f__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__001___81f5a2c98f__0
           {|
    /* _none_ */

    *.foo_hash_81f5a2c98f {
     background-color:red
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
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
    module type S  = sig module For_referencing : sig  end end
    type t = (module S)
    module Default : S = struct module For_referencing = struct  end end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__002___f40c1c678a__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__002___f40c1c678a__0
        {|
    /* _none_ */

    @charset" UTF-8";|}
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
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
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
            let bar = {|bar_hash_8397f3ea42|}
            let my_foo = {|my_foo_hash_8397f3ea42|}
          end
        let bar = Virtual_dom.Vdom.Attr.id {|bar_hash_8397f3ea42|}
        let my_foo = Virtual_dom.Vdom.Attr.class_ {|my_foo_hash_8397f3ea42|}
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__003___8397f3ea42__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__003___8397f3ea42__0
        {|
    /* _none_ */

    @media screen and (min-width:900px){
     div#bar_hash_8397f3ea42 {
      color:red
     }

     @media screen and (min-width:900px){
      article.my_foo_hash_8397f3ea42 {
       padding:1rem 3rem
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
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
    module type S  =
      sig
        module For_referencing : sig val my_foo : string end
        val my_foo : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing =
          struct let my_foo = {|my_foo_hash_ab8256fa1e|} end
        let my_foo = Virtual_dom.Vdom.Attr.class_ {|my_foo_hash_ab8256fa1e|}
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__004___ab8256fa1e__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__004___ab8256fa1e__0
        {|
    /* _none_ */

    @media screen and (min-width:900px){
     article.my_foo_hash_ab8256fa1e {
      padding:1rem 3rem
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
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
    module type S  =
      sig
        module For_referencing : sig val foo : string end
        val foo : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing = struct let foo = {|foo_hash_d6b3bc34a5|} end
        let foo =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__005___d6b3bc34a5__group_0;
                Virtual_dom.Vdom.Attr.id {|foo_hash_d6b3bc34a5|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__005___d6b3bc34a5__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__005___d6b3bc34a5__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__005___d6b3bc34a5__0
           {|
    /* _none_ */

    *#foo_hash_d6b3bc34a5 {
     background-color:red
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
  [%expect {xxx| Parse error while reading token 'EOF' |xxx}]
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
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
    module type S  =
      sig
        module For_referencing : sig val spinner : string end
        val spinner : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        module For_referencing =
          struct let spinner = {|spinner_hash_4f48a20d31|} end
        let spinner =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__006___4f48a20d31__group_0;
                Virtual_dom.Vdom.Attr.class_ {|spinner_hash_4f48a20d31|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__006___4f48a20d31__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__006___4f48a20d31__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__006___4f48a20d31__1
        {|
    /* _none_ */

    @keyframes spin{
     from {
      transform:rotate(0deg)
     }

     to {
      transform:rotate(360deg)
     }

    }|}
    let update_sheet_lazy_fn_x__006___4f48a20d31__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__006___4f48a20d31__0
           {|
    /* _none_ */

    *.spinner_hash_4f48a20d31 {
     animation-name:spin;
     animation-duration:5000ms;
     animation-iteration-count:infinite;
     animation-timing-function:linear
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
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
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
            let hash_me = {|hash-me_hash_efbf1b0ed5|}
            let hash_me_id = {|hash-me-id_hash_efbf1b0ed5|}
          end
        let dont_hash_me = Virtual_dom.Vdom.Attr.class_ {|dont-hash-me|}
        let dont_hash_me_id = Virtual_dom.Vdom.Attr.id {|dont-hash-me-id|}
        let hash_me =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__007___efbf1b0ed5__group_1;
                Virtual_dom.Vdom.Attr.class_ {|hash-me_hash_efbf1b0ed5|}))
        let hash_me_id =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__007___efbf1b0ed5__group_0;
                Virtual_dom.Vdom.Attr.id {|hash-me-id_hash_efbf1b0ed5|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__007___efbf1b0ed5__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__007___efbf1b0ed5__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__007___efbf1b0ed5__2 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__007___efbf1b0ed5__3 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__007___efbf1b0ed5__1
        {|
    /* _none_ */

    *.dont-hash-me {
     color:green
    }|};
      Inline_css.Private.update_stylesheet sheet_x__007___efbf1b0ed5__3
        {|
    /* _none_ */

    *#dont-hash-me-id {
     color:white
    }|}
    let update_sheet_lazy_fn_x__007___efbf1b0ed5__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__007___efbf1b0ed5__2
           {|
    /* _none_ */

    *#hash-me-id_hash_efbf1b0ed5 {
     color:red
    }|})
    let update_sheet_lazy_fn_x__007___efbf1b0ed5__group_1 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__007___efbf1b0ed5__0
           {|
    /* _none_ */

    *.hash-me_hash_efbf1b0ed5 {
     color:black
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
      Stylesheet.of_string css_string |> Ppx_css.Stable_stylesheet.of_stylesheet
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
    let struct_ =
      For_css_inliner.gen_struct
        ~dont_hash:String.Set.empty
        ~css_string:s
        ~dont_hash_prefixes:[]
        ~stylesheet_location:Location.none
        ~lazy_loading_optimization:Ppx_css_syntax.Preprocess_arguments.Lazy_graph
    in
    let sig_ = For_css_inliner.gen_sig s in
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
                let sheet_x__008___6f26622dd7__0 =
                  let sheet = Inline_css.Private.create_stylesheet () in
                  Inline_css.Private.append_stylesheet sheet; sheet
                let sheet_x__008___6f26622dd7__1 =
                  let sheet = Inline_css.Private.create_stylesheet () in
                  Inline_css.Private.append_stylesheet sheet; sheet
                let update_sheet_lazy_fn_x__008___6f26622dd7__group_0 =
                  lazy
                    (Inline_css.Private.update_stylesheet
                       sheet_x__008___6f26622dd7__0
                       {|
      /* _none_ */

      *.a_hash_6f26622dd7 {
       color:green
      }|})
                let update_sheet_lazy_fn_x__008___6f26622dd7__group_1 =
                  lazy
                    (Inline_css.Private.update_stylesheet
                       sheet_x__008___6f26622dd7__1
                       {|
      /* _none_ */

      *.b_hash_6f26622dd7 {
       color:white
      }|})
              end
          end
        [@@@ocaml.warning "-32"]
        let __type_info_for_ppx_css :
          ?dont_hash:string list ->
            ?dont_hash_prefixes:string list -> string -> unit
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
              struct let a = {|a_hash_6f26622dd7|}
                     let b = {|b_hash_6f26622dd7|} end
            let a =
              Virtual_dom.Vdom.Attr.lazy_
                (lazy
                   (Inline_css.Ppx_css_runtime.force
                      Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__008___6f26622dd7__group_0;
                    Virtual_dom.Vdom.Attr.class_ {|a_hash_6f26622dd7|}))
            let b =
              Virtual_dom.Vdom.Attr.lazy_
                (lazy
                   (Inline_css.Ppx_css_runtime.force
                      Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__008___6f26622dd7__group_1;
                    Virtual_dom.Vdom.Attr.class_ {|b_hash_6f26622dd7|}))
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
                let sheet_x__008___6f26622dd7__0 =
                  let sheet = Inline_css.Private.create_stylesheet () in
                  Inline_css.Private.append_stylesheet sheet; sheet
                let sheet_x__008___6f26622dd7__1 =
                  let sheet = Inline_css.Private.create_stylesheet () in
                  Inline_css.Private.append_stylesheet sheet; sheet
                let update_sheet_lazy_fn_x__008___6f26622dd7__group_0 =
                  lazy
                    (Inline_css.Private.update_stylesheet
                       sheet_x__008___6f26622dd7__0
                       {|
      /* _none_ */

      *.a_hash_6f26622dd7 {
       color:green
      }|})
                let update_sheet_lazy_fn_x__008___6f26622dd7__group_1 =
                  lazy
                    (Inline_css.Private.update_stylesheet
                       sheet_x__008___6f26622dd7__1
                       {|
      /* _none_ */

      *.b_hash_6f26622dd7 {
       color:white
      }|})
                let sheet_x__009___7087cb1e7f__0 =
                  let sheet = Inline_css.Private.create_stylesheet () in
                  Inline_css.Private.append_stylesheet sheet; sheet
                let sheet_x__009___7087cb1e7f__1 =
                  let sheet = Inline_css.Private.create_stylesheet () in
                  Inline_css.Private.append_stylesheet sheet; sheet
                let update_sheet_lazy_fn_x__009___7087cb1e7f__group_0 =
                  lazy
                    (Inline_css.Private.update_stylesheet
                       sheet_x__009___7087cb1e7f__0
                       {|
      /* _none_ */

      *.a_hash_7087cb1e7f {
       color:green
      }|};
                     Inline_css.Private.update_stylesheet
                       sheet_x__009___7087cb1e7f__1
                       {|
      /* _none_ */

      *.a_hash_7087cb1e7f {
       color:white
      }|})
              end
          end
        [@@@ocaml.warning "-32"]
        let __type_info_for_ppx_css :
          ?dont_hash:string list ->
            ?dont_hash_prefixes:string list -> string -> unit
          = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
        module type S  =
          sig
            module For_referencing : sig val a : string end
            val a : Virtual_dom.Vdom.Attr.t
          end
        type t = (module S)
        module Default : S =
          struct
            module For_referencing = struct let a = {|a_hash_7087cb1e7f|} end
            let a =
              Virtual_dom.Vdom.Attr.lazy_
                (lazy
                   (Inline_css.Ppx_css_runtime.force
                      Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__009___7087cb1e7f__group_0;
                    Virtual_dom.Vdom.Attr.class_ {|a_hash_7087cb1e7f|}))
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
  --fg-color: black
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
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
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
            let bb = {|bb_hash_695293f60c|}
          end
        let a = Virtual_dom.Vdom.Attr.class_ {|a|}
        let aa = Virtual_dom.Vdom.Attr.class_ {|aa|}
        let b = Virtual_dom.Vdom.Attr.class_ {|b|}
        let bb =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__010___695293f60c__group_0;
                Virtual_dom.Vdom.Attr.class_ {|bb_hash_695293f60c|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__010___695293f60c__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__010___695293f60c__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__010___695293f60c__2 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__010___695293f60c__3 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      (Inline_css.Private.update_stylesheet sheet_x__010___695293f60c__0
         {|
    /* _none_ */

    *.b {

    }|};
       Inline_css.Private.update_stylesheet sheet_x__010___695293f60c__2
         {|
    /* _none_ */

    *.a {

    }|});
      Inline_css.Private.update_stylesheet sheet_x__010___695293f60c__3
        {|
    /* _none_ */

    *.aa {

    }|}
    let update_sheet_lazy_fn_x__010___695293f60c__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__010___695293f60c__1
           {|
    /* _none_ */

    *.bb_hash_695293f60c {

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
      background-color: var(--my-variable, default)

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
                    ({|--my-variable_hash_5e98bc0535|}, ppx_css_value__013_) ::
                    ppx_css_acc__012_ in
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__012_
            let set = ppx_css_variable_set__014_
            let set_all ~my_variable = ppx_css_variable_set__014_ () ~my_variable
          end
        module For_referencing =
          struct
            let a = {|a_hash_5e98bc0535|}
            let my_variable = {|--my-variable_hash_5e98bc0535|}
          end
        let a =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__011___5e98bc0535__group_0;
                Virtual_dom.Vdom.Attr.class_ {|a_hash_5e98bc0535|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__011___5e98bc0535__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__011___5e98bc0535__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__011___5e98bc0535__0
        {|
    /* _none_ */

    *:root {
     --my-variable_hash_5e98bc0535:green
    }|}
    let update_sheet_lazy_fn_x__011___5e98bc0535__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__011___5e98bc0535__1
           {|
    /* _none_ */

    *.a_hash_5e98bc0535 {
     background-color:var(--my-variable_hash_5e98bc0535,default)
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
      background-color: var(--a, var(--b, (var(--c))))
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
                    ({|--a_hash_cce96b76a8|}, ppx_css_value__017_) ::
                    ppx_css_acc__016_ in
              let ppx_css_acc__016_ =
                match b with
                | None -> ppx_css_acc__016_
                | Some ppx_css_value__017_ ->
                    ({|--b_hash_cce96b76a8|}, ppx_css_value__017_) ::
                    ppx_css_acc__016_ in
              let ppx_css_acc__016_ =
                match c with
                | None -> ppx_css_acc__016_
                | Some ppx_css_value__017_ ->
                    ({|--c_hash_cce96b76a8|}, ppx_css_value__017_) ::
                    ppx_css_acc__016_ in
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__016_
            let set = ppx_css_variable_set__018_
            let set_all ~a ~b ~c = ppx_css_variable_set__018_ () ~a ~b ~c
          end
        module For_referencing =
          struct
            let a = {|--a_hash_cce96b76a8|}
            let b = {|--b_hash_cce96b76a8|}
            let c = {|--c_hash_cce96b76a8|}
            let navbar = {|navbar_hash_cce96b76a8|}
          end
        let navbar =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__015___cce96b76a8__group_0;
                Virtual_dom.Vdom.Attr.class_ {|navbar_hash_cce96b76a8|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__015___cce96b76a8__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__015___cce96b76a8__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__015___cce96b76a8__0
        {|
    /* _none_ */

    *:root {
     --a_hash_cce96b76a8:green;
     --b_hash_cce96b76a8:green;
     --c_hash_cce96b76a8:green
    }|}
    let update_sheet_lazy_fn_x__015___cce96b76a8__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__015___cce96b76a8__1
           {|
    /* _none_ */

    *.navbar_hash_cce96b76a8 {
     background-color:var(--a_hash_cce96b76a8,var(--b_hash_cce96b76a8,(var(--c_hash_cce96b76a8))))
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
    {xxx|
    [@@@ocaml.warning "-32"]
    let __type_info_for_ppx_css :
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
    module type S  = sig module For_referencing : sig  end end
    type t = (module S)
    module Default : S = struct module For_referencing = struct  end end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__019___e11edcd4a7__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__019___e11edcd4a7__0
        {|
    /* _none_ */

    has(.a) {
     background-color::has(.a);
     background-color:has(.a)
    }|}
    |xxx}]
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
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
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
        module For_referencing = struct let foo = {|foo_hash_ea252eb70b|} end
        let foo = Virtual_dom.Vdom.Attr.empty
        let foo_class =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__020___ea252eb70b__group_1;
                Virtual_dom.Vdom.Attr.class_ {|foo_hash_ea252eb70b|}))
        let foo_id =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__020___ea252eb70b__group_0;
                Virtual_dom.Vdom.Attr.id {|foo_hash_ea252eb70b|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__020___ea252eb70b__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__020___ea252eb70b__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__020___ea252eb70b__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__020___ea252eb70b__0
           {|
    /* _none_ */

    *#foo_hash_ea252eb70b {

    }|})
    let update_sheet_lazy_fn_x__020___ea252eb70b__group_1 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__020___ea252eb70b__1
           {|
    /* _none_ */

    *.foo_hash_ea252eb70b {

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
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
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
        module For_referencing = struct let a = {|a_hash_c775f164d0|} end
        let a = Virtual_dom.Vdom.Attr.empty
        let a_class =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__022___c775f164d0__group_1;
                Virtual_dom.Vdom.Attr.class_ {|a_hash_c775f164d0|}))
        let a_id =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__022___c775f164d0__group_0;
                Virtual_dom.Vdom.Attr.id {|a_hash_c775f164d0|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__022___c775f164d0__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__022___c775f164d0__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__022___c775f164d0__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__022___c775f164d0__1
           {|
    /* _none_ */

    *#a_hash_c775f164d0 {

    }|})
    let update_sheet_lazy_fn_x__022___c775f164d0__group_1 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__022___c775f164d0__0
           {|
    /* _none_ */

    *.a_hash_c775f164d0 {

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
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
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
            let ppx_css_variable_set__026_ ?a_variable () =
              let ppx_css_acc__024_ = [] in
              let ppx_css_acc__024_ =
                match a_variable with
                | None -> ppx_css_acc__024_
                | Some ppx_css_value__025_ ->
                    ({|--a-variable|}, ppx_css_value__025_) :: ppx_css_acc__024_ in
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__024_
            let set = ppx_css_variable_set__026_
            let set_all ~a_variable = ppx_css_variable_set__026_ () ~a_variable
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

    let sheet_x__023___8aa77c6364__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__023___8aa77c6364__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__023___8aa77c6364__0
        {|
    /* _none_ */

    *.a-class {
     --a-variable:red
    }|};
      Inline_css.Private.update_stylesheet sheet_x__023___8aa77c6364__1
        {|
    /* _none_ */

    *#an_id {

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
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
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
            let ppx_css_variable_set__030_ ?bg_color ?fg_color () =
              let ppx_css_acc__028_ = [] in
              let ppx_css_acc__028_ =
                match bg_color with
                | None -> ppx_css_acc__028_
                | Some ppx_css_value__029_ ->
                    ({|--bg-color|}, ppx_css_value__029_) :: ppx_css_acc__028_ in
              let ppx_css_acc__028_ =
                match fg_color with
                | None -> ppx_css_acc__028_
                | Some ppx_css_value__029_ ->
                    ({|--fg-color_hash_9ac909b280|}, ppx_css_value__029_) ::
                    ppx_css_acc__028_ in
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__028_
            let set = ppx_css_variable_set__030_
            let set_all ~bg_color ~fg_color =
              ppx_css_variable_set__030_ () ~bg_color ~fg_color
          end
        module For_referencing =
          struct
            let a = {|a_hash_9ac909b280|}
            let bg_color = {|--bg-color|}
            let fg_color = {|--fg-color_hash_9ac909b280|}
          end
        let a =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__027___9ac909b280__group_0;
                Virtual_dom.Vdom.Attr.class_ {|a_hash_9ac909b280|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__027___9ac909b280__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__027___9ac909b280__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__027___9ac909b280__0
           {|
    /* _none_ */

    *.a_hash_9ac909b280 {
     --bg-color:red;
     --fg-color_hash_9ac909b280:blue
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
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
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
            let ppx_css_variable_set__034_ ?aa ?bb () =
              let ppx_css_acc__032_ = [] in
              let ppx_css_acc__032_ =
                match aa with
                | None -> ppx_css_acc__032_
                | Some ppx_css_value__033_ -> ({|--aa|}, ppx_css_value__033_) ::
                    ppx_css_acc__032_ in
              let ppx_css_acc__032_ =
                match bb with
                | None -> ppx_css_acc__032_
                | Some ppx_css_value__033_ -> ({|--bb|}, ppx_css_value__033_) ::
                    ppx_css_acc__032_ in
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__032_
            let set = ppx_css_variable_set__034_
            let set_all ~aa ~bb = ppx_css_variable_set__034_ () ~aa ~bb
          end
        module For_referencing =
          struct
            let a = {|a_hash_d0050e0025|}
            let aa = {|--aa|}
            let b = {|b_hash_d0050e0025|}
            let bb = {|--bb|}
          end
        let a =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__031___d0050e0025__group_0;
                Virtual_dom.Vdom.Attr.class_ {|a_hash_d0050e0025|}))
        let b =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__031___d0050e0025__group_1;
                Virtual_dom.Vdom.Attr.class_ {|b_hash_d0050e0025|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__031___d0050e0025__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__031___d0050e0025__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__031___d0050e0025__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__031___d0050e0025__0
           {|
    /* _none_ */

    *.a_hash_d0050e0025 {
     --aa:red
    }|})
    let update_sheet_lazy_fn_x__031___d0050e0025__group_1 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__031___d0050e0025__1
           {|
    /* _none_ */

    *.b_hash_d0050e0025 {
     --bb:blue
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
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
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
            let ppx_css_variable_set__038_ ?cm_bg_color () =
              let ppx_css_acc__036_ = [] in
              let ppx_css_acc__036_ =
                match cm_bg_color with
                | None -> ppx_css_acc__036_
                | Some ppx_css_value__037_ ->
                    ({|--cm-bg-color|}, ppx_css_value__037_) :: ppx_css_acc__036_ in
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__036_
            let set = ppx_css_variable_set__038_
            let set_all ~cm_bg_color = ppx_css_variable_set__038_ () ~cm_bg_color
          end
        module For_referencing = struct let cm_bg_color = {|--cm-bg-color|} end
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__035___4c842aeb2d__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__035___4c842aeb2d__0
        {|
    /* _none_ */

    *:root {
     color:var(--cm-bg-color)
    }|}
    |xxx}]
;;

let%expect_test "Scary attributes shouldn't be hashed and should throw" =
  test_struct
    [%expr
      stylesheet
        {|
        div:asdf(".m .n .o") {}
        div:asdf(.d .e .f) {}
        div:qwerty(.t .q .r) {}
       |}];
  [%expect
    {xxx| Unsafe use of classes or ids ["d"; "e"; "f"] within unknown function 'asdf'. Please contact the owners of [ppx_css] so that this CSS function can be audited and added to the allow list in order to resolve this bug. |xxx}]
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
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
    module type S  = sig module For_referencing : sig  end end
    type t = (module S)
    module Default : S = struct module For_referencing = struct  end end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__039___d08c354fd0__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__039___d08c354fd0__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__039___d08c354fd0__2 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      (Inline_css.Private.update_stylesheet sheet_x__039___d08c354fd0__0
         {|
    /* _none_ */

    div[has=".x .y .z"] {

    }|};
       Inline_css.Private.update_stylesheet sheet_x__039___d08c354fd0__1
         {|
    /* _none_ */

    div[attr=".a .b .c"] {

    }|});
      Inline_css.Private.update_stylesheet sheet_x__039___d08c354fd0__2
        {|
    /* _none_ */

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
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
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
          struct let a = {|a_hash_14cc7e76e2|}
                 let b = {|b_hash_14cc7e76e2|} end
        let a = Virtual_dom.Vdom.Attr.empty
        let a_class =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__040___14cc7e76e2__group_1;
                Virtual_dom.Vdom.Attr.class_ {|a_hash_14cc7e76e2|}))
        let a_id = Virtual_dom.Vdom.Attr.id {|a_hash_14cc7e76e2|}
        let b = Virtual_dom.Vdom.Attr.empty
        let b_class = Virtual_dom.Vdom.Attr.class_ {|b_hash_14cc7e76e2|}
        let b_id =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__040___14cc7e76e2__group_0;
                Virtual_dom.Vdom.Attr.id {|b_hash_14cc7e76e2|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__040___14cc7e76e2__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__040___14cc7e76e2__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__040___14cc7e76e2__2 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__040___14cc7e76e2__3 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__040___14cc7e76e2__0
        {|
    /* _none_ */

    *:not(#a_hash_14cc7e76e2) {

    }|};
      Inline_css.Private.update_stylesheet sheet_x__040___14cc7e76e2__2
        {|
    /* _none_ */

    *:not(.b_hash_14cc7e76e2) {

    }|}
    let update_sheet_lazy_fn_x__040___14cc7e76e2__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__040___14cc7e76e2__3
           {|
    /* _none_ */

    *#b_hash_14cc7e76e2 {

    }|})
    let update_sheet_lazy_fn_x__040___14cc7e76e2__group_1 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__040___14cc7e76e2__1
           {|
    /* _none_ */

    *.a_hash_14cc7e76e2 {

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
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
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
          struct let a = {|a_hash_c14da1c08e|}
                 let b = {|b_hash_c14da1c08e|} end
        let a = Virtual_dom.Vdom.Attr.empty
        let a_class =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__041___c14da1c08e__group_1;
                Virtual_dom.Vdom.Attr.class_ {|a_hash_c14da1c08e|}))
        let a_id = Virtual_dom.Vdom.Attr.id {|a_hash_c14da1c08e|}
        let b = Virtual_dom.Vdom.Attr.empty
        let b_class =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__041___c14da1c08e__group_1;
                Virtual_dom.Vdom.Attr.class_ {|b_hash_c14da1c08e|}))
        let b_id =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__041___c14da1c08e__group_0;
                Virtual_dom.Vdom.Attr.id {|b_hash_c14da1c08e|}))
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__041___c14da1c08e__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__041___c14da1c08e__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__041___c14da1c08e__2 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__041___c14da1c08e__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__041___c14da1c08e__2
           {|
    /* _none_ */

    *:not(#a_hash_c14da1c08e) *#b_hash_c14da1c08e {

    }|})
    let update_sheet_lazy_fn_x__041___c14da1c08e__group_1 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__041___c14da1c08e__0
           {|
    /* _none_ */

    *.a_hash_c14da1c08e *.b_hash_c14da1c08e {

    }|};
         Inline_css.Private.update_stylesheet sheet_x__041___c14da1c08e__1
           {|
    /* _none_ */

    *:not(.a_hash_c14da1c08e) *.b_hash_c14da1c08e {

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
                div:has('.a .b .c') { }
                div:has(".a .b .c") { }
                div[foo=".a .b .c"] { }
                div[bar='.a .b .c'] { }
       |}];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let __type_info_for_ppx_css :
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
    module type S  = sig module For_referencing : sig  end end
    type t = (module S)
    module Default : S = struct module For_referencing = struct  end end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__042___b64b2242d6__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__042___b64b2242d6__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__042___b64b2242d6__2 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__042___b64b2242d6__3 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__042___b64b2242d6__4 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__042___b64b2242d6__5 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      ((((Inline_css.Private.update_stylesheet sheet_x__042___b64b2242d6__0
            {|
    /* _none_ */

    div:asdf(".a .b .c") {

    }|};
          Inline_css.Private.update_stylesheet sheet_x__042___b64b2242d6__1
            {|
    /* _none_ */

    div:asdf(".a .b .c") {

    }|});
         Inline_css.Private.update_stylesheet sheet_x__042___b64b2242d6__2
           {|
    /* _none_ */

    div:has(".a .b .c") {

    }|});
        Inline_css.Private.update_stylesheet sheet_x__042___b64b2242d6__3
          {|
    /* _none_ */

    div:has(".a .b .c") {

    }|});
       Inline_css.Private.update_stylesheet sheet_x__042___b64b2242d6__4
         {|
    /* _none_ */

    div[foo=".a .b .c"] {

    }|});
      Inline_css.Private.update_stylesheet sheet_x__042___b64b2242d6__5
        {|
    /* _none_ */

    div[bar=".a .b .c"] {

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
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
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
            let a = {|a_hash_b185384f4b|}
            let b = {|b_hash_b185384f4b|}
            let c = {|c_hash_b185384f4b|}
            let d = {|d_hash_b185384f4b|}
            let e = {|e_hash_b185384f4b|}
            let f = {|f_hash_b185384f4b|}
            let g = {|g_hash_b185384f4b|}
            let h = {|h_hash_b185384f4b|}
          end
        let a =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__043___b185384f4b__group_0;
                Virtual_dom.Vdom.Attr.class_ {|a_hash_b185384f4b|}))
        let b =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__043___b185384f4b__group_0;
                Virtual_dom.Vdom.Attr.class_ {|b_hash_b185384f4b|}))
        let c = Virtual_dom.Vdom.Attr.class_ {|c_hash_b185384f4b|}
        let d = Virtual_dom.Vdom.Attr.class_ {|d_hash_b185384f4b|}
        let e = Virtual_dom.Vdom.Attr.class_ {|e_hash_b185384f4b|}
        let f = Virtual_dom.Vdom.Attr.class_ {|f_hash_b185384f4b|}
        let g = Virtual_dom.Vdom.Attr.class_ {|g_hash_b185384f4b|}
        let h = Virtual_dom.Vdom.Attr.class_ {|h_hash_b185384f4b|}
      end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__043___b185384f4b__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__043___b185384f4b__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__043___b185384f4b__2 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__043___b185384f4b__3 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      (Inline_css.Private.update_stylesheet sheet_x__043___b185384f4b__1
         {|
    /* _none_ */

    div:not(.c_hash_b185384f4b+*.d_hash_b185384f4b) {

    }|};
       Inline_css.Private.update_stylesheet sheet_x__043___b185384f4b__2
         {|
    /* _none_ */

    div:where(.e_hash_b185384f4b~*.f_hash_b185384f4b) {

    }|});
      Inline_css.Private.update_stylesheet sheet_x__043___b185384f4b__3
        {|
    /* _none_ */

    div:is(.g_hash_b185384f4b>*.h_hash_b185384f4b) {

    }|}
    let update_sheet_lazy_fn_x__043___b185384f4b__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__043___b185384f4b__0
           {|
    /* _none_ */

    div:has(.a_hash_b185384f4b *.b_hash_b185384f4b) {

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
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
    module type S  = sig module For_referencing : sig  end end
    type t = (module S)
    module Default : S = struct module For_referencing = struct  end end
    include Default
    let default : t = (module Default)

    Hoisted module:

    let sheet_x__044___04bf3bff1c__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__044___04bf3bff1c__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let () =
      Inline_css.Private.update_stylesheet sheet_x__044___04bf3bff1c__0
        {|
    /* _none_ */

    div:asdf() {

    }|};
      Inline_css.Private.update_stylesheet sheet_x__044___04bf3bff1c__1
        {|
    /* _none_ */

    div:bar {

    }|}
    |xxx}]
;;

let%expect_test "Unknown css functions with identifiers should error out" =
  test_struct
    [%expr
      stylesheet
        {|
                div:asdf(.a .b .c) {
                }
       |}];
  [%expect
    {xxx| Unsafe use of classes or ids ["a"; "b"; "c"] within unknown function 'asdf'. Please contact the owners of [ppx_css] so that this CSS function can be audited and added to the allow list in order to resolve this bug. |xxx}]
;;
