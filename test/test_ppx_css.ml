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
        module For_referencing = struct let foo = {|foo_hash_aada351bb3|} end
        let foo = Virtual_dom.Vdom.Attr.class_ {|foo_hash_aada351bb3|}
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.foo_hash_aada351bb3 {
     background-color:red
    }|}
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
    let () =
      Inline_css.Private.append_but_do_not_update
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
            let bar = {|bar_hash_62b41ab32f|}
            let my_foo = {|my_foo_hash_62b41ab32f|}
          end
        let bar = Virtual_dom.Vdom.Attr.id {|bar_hash_62b41ab32f|}
        let my_foo = Virtual_dom.Vdom.Attr.class_ {|my_foo_hash_62b41ab32f|}
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    @media screen and (min-width:900px){
     div#bar_hash_62b41ab32f {
      color:red
     }

     @media screen and (min-width:900px){
      article.my_foo_hash_62b41ab32f {
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
          struct let my_foo = {|my_foo_hash_70568f53c7|} end
        let my_foo = Virtual_dom.Vdom.Attr.class_ {|my_foo_hash_70568f53c7|}
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    @media screen and (min-width:900px){
     article.my_foo_hash_70568f53c7 {
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
        module For_referencing = struct let foo = {|foo_hash_735b1fee1b|} end
        let foo = Virtual_dom.Vdom.Attr.id {|foo_hash_735b1fee1b|}
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *#foo_hash_735b1fee1b {
     background-color:red
    }|}
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
          struct let spinner = {|spinner_hash_cdb7b13d32|} end
        let spinner = Virtual_dom.Vdom.Attr.class_ {|spinner_hash_cdb7b13d32|}
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.spinner_hash_cdb7b13d32 {
     animation-name:spin;
     animation-duration:5000ms;
     animation-iteration-count:infinite;
     animation-timing-function:linear
    }

    @keyframes spin{
     from {
      transform:rotate(0deg)
     }

     to {
      transform:rotate(360deg)
     }

    }|}
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
            let hash_me_id = {|hash-me-id_hash_1ca851e74f|}
            let hash_me = {|hash-me_hash_1ca851e74f|}
            let dont_hash_me_id = {|dont-hash-me-id|}
            let dont_hash_me = {|dont-hash-me|}
          end
        let hash_me_id = Virtual_dom.Vdom.Attr.id {|hash-me-id_hash_1ca851e74f|}
        let hash_me = Virtual_dom.Vdom.Attr.class_ {|hash-me_hash_1ca851e74f|}
        let dont_hash_me_id = Virtual_dom.Vdom.Attr.id {|dont-hash-me-id|}
        let dont_hash_me = Virtual_dom.Vdom.Attr.class_ {|dont-hash-me|}
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.hash-me_hash_1ca851e74f {
     color:black
    }

    *.dont-hash-me {
     color:green
    }

    *#hash-me-id_hash_1ca851e74f {
     color:red
    }

    *#dont-hash-me-id {
     color:white
    }|}
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
    {xxx| Unsafe collision of names. Cannot rename 'hello-world' to 'hello_world' because 'hello_world' already exists |xxx}]
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
    {xxx| Unsafe collisions of names. Two different unsafe names map to the same fixed name which might lead to unintended results. Both 'hello-world_1' and 'hello_world-1' map to 'hello_world_1' |xxx}];
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
    {xxx| Unsafe collisions of names. Two different unsafe names map to the same fixed name which might lead to unintended results. Both '--hello-world-1' and 'hello-world_1' map to 'hello_world_1' |xxx}];
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
    {xxx| Unsafe collisions of names. Two different unsafe names map to the same fixed name which might lead to unintended results. Both '--hello-world-1' and 'hello-world_1' map to 'hello_world_1' |xxx}]
;;

let%expect_test "will not rewrite if even one value is different" =
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
    let style_sheet = Stylesheet.of_string css_string in
    let traversed_ids = ref Reversed_list.[] in
    Ppx_css.For_testing.map_style_sheet
      ~dont_hash:String.Set.empty
      ~dont_hash_prefixes:[]
      style_sheet
      ~f:(fun (`Variable id | `Class id | `Id id) _ ->
        (traversed_ids := Reversed_list.(id :: !traversed_ids));
        id)
    |> (ignore : Stylesheet.t -> unit);
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
        let () =
          Inline_css.Private.append_but_do_not_update
            {|
      /* _none_ */

      *.a_hash_5da393bd89 {
       color:green
      }

      *.b_hash_5da393bd89 {
       color:white
      }|}
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
              struct let b = {|b_hash_5da393bd89|}
                     let a = {|a_hash_5da393bd89|} end
            let b = Virtual_dom.Vdom.Attr.class_ {|b_hash_5da393bd89|}
            let a = Virtual_dom.Vdom.Attr.class_ {|a_hash_5da393bd89|}
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
        let () =
          Inline_css.Private.append_but_do_not_update
            {|
      /* _none_ */

      *.a_hash_c7bdcfd04c {
       color:green
      }

      *.a_hash_c7bdcfd04c {
       color:white
      }|}
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
            module For_referencing = struct let a = {|a_hash_c7bdcfd04c|} end
            let a = Virtual_dom.Vdom.Attr.class_ {|a_hash_c7bdcfd04c|}
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
            let bb = {|bb_hash_d96aca4865|}
            let b = {|b|}
            let a = {|a|}
            let aa = {|aa|}
          end
        let bb = Virtual_dom.Vdom.Attr.class_ {|bb_hash_d96aca4865|}
        let b = Virtual_dom.Vdom.Attr.class_ {|b|}
        let a = Virtual_dom.Vdom.Attr.class_ {|a|}
        let aa = Virtual_dom.Vdom.Attr.class_ {|aa|}
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.b {

    }

    *.bb_hash_d96aca4865 {

    }

    *.a {

    }

    *.aa {

    }|}
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
            let ppx_css_variable_set__003_ ?my_variable () =
              let ppx_css_acc__001_ = [] in
              let ppx_css_acc__001_ =
                match my_variable with
                | None -> ppx_css_acc__001_
                | Some ppx_css_value__002_ ->
                    ({|--my-variable_hash_bfcd4586aa|}, ppx_css_value__002_) ::
                    ppx_css_acc__001_ in
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__001_
            let set = ppx_css_variable_set__003_
            let set_all ~my_variable = ppx_css_variable_set__003_ () ~my_variable
          end
        module For_referencing =
          struct
            let a = {|a_hash_bfcd4586aa|}
            let my_variable = {|--my-variable_hash_bfcd4586aa|}
          end
        let a = Virtual_dom.Vdom.Attr.class_ {|a_hash_bfcd4586aa|}
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *:root {
     --my-variable_hash_bfcd4586aa:green
    }

    *.a_hash_bfcd4586aa {
     background-color:var(--my-variable_hash_bfcd4586aa,default)
    }|}
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
            let ppx_css_variable_set__006_ ?a ?b ?c () =
              let ppx_css_acc__004_ = [] in
              let ppx_css_acc__004_ =
                match a with
                | None -> ppx_css_acc__004_
                | Some ppx_css_value__005_ ->
                    ({|--a_hash_90b2c949fd|}, ppx_css_value__005_) ::
                    ppx_css_acc__004_ in
              let ppx_css_acc__004_ =
                match b with
                | None -> ppx_css_acc__004_
                | Some ppx_css_value__005_ ->
                    ({|--b_hash_90b2c949fd|}, ppx_css_value__005_) ::
                    ppx_css_acc__004_ in
              let ppx_css_acc__004_ =
                match c with
                | None -> ppx_css_acc__004_
                | Some ppx_css_value__005_ ->
                    ({|--c_hash_90b2c949fd|}, ppx_css_value__005_) ::
                    ppx_css_acc__004_ in
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__004_
            let set = ppx_css_variable_set__006_
            let set_all ~a ~b ~c = ppx_css_variable_set__006_ () ~a ~b ~c
          end
        module For_referencing =
          struct
            let navbar = {|navbar_hash_90b2c949fd|}
            let c = {|--c_hash_90b2c949fd|}
            let b = {|--b_hash_90b2c949fd|}
            let a = {|--a_hash_90b2c949fd|}
          end
        let navbar = Virtual_dom.Vdom.Attr.class_ {|navbar_hash_90b2c949fd|}
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *:root {
     --a_hash_90b2c949fd:green;
     --b_hash_90b2c949fd:green;
     --c_hash_90b2c949fd:green
    }

    *.navbar_hash_90b2c949fd {
     background-color:var(--a_hash_90b2c949fd,var(--b_hash_90b2c949fd,(var(--c_hash_90b2c949fd))))
    }|}
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
    let () =
      Inline_css.Private.append_but_do_not_update
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
        module For_referencing = struct let foo = {|foo_hash_57d44d95ea|} end
        let foo = Virtual_dom.Vdom.Attr.empty
        let foo_class = Virtual_dom.Vdom.Attr.class_ {|foo_hash_57d44d95ea|}
        let foo_id = Virtual_dom.Vdom.Attr.id {|foo_hash_57d44d95ea|}
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *#foo_hash_57d44d95ea {

    }

    *.foo_hash_57d44d95ea {

    }|}
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
        module For_referencing = struct let a = {|a_hash_7294022fdf|} end
        let a = Virtual_dom.Vdom.Attr.empty
        let a_class = Virtual_dom.Vdom.Attr.class_ {|a_hash_7294022fdf|}
        let a_id = Virtual_dom.Vdom.Attr.id {|a_hash_7294022fdf|}
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.a_hash_7294022fdf {

    }

    *#a_hash_7294022fdf {

    }|}
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
            let ppx_css_variable_set__009_ ?a_variable () =
              let ppx_css_acc__007_ = [] in
              let ppx_css_acc__007_ =
                match a_variable with
                | None -> ppx_css_acc__007_
                | Some ppx_css_value__008_ ->
                    ({|--a-variable|}, ppx_css_value__008_) :: ppx_css_acc__007_ in
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__007_
            let set = ppx_css_variable_set__009_
            let set_all ~a_variable = ppx_css_variable_set__009_ () ~a_variable
          end
        module For_referencing =
          struct
            let an_id = {|an_id|}
            let a_class = {|a-class|}
            let a_variable = {|--a-variable|}
          end
        let an_id = Virtual_dom.Vdom.Attr.id {|an_id|}
        let a_class = Virtual_dom.Vdom.Attr.class_ {|a-class|}
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.a-class {
     --a-variable:red
    }

    *#an_id {

    }|}
    |xxx}]
;;

let%expect_test "Clashes caused by the rename to snake case from kebab case trigger a \
                 warning"
  =
  test_struct [%expr stylesheet {| .a-a {}  .a_a {} |}];
  [%expect
    {xxx| Unsafe collision of names. Cannot rename 'a-a' to 'a_a' because 'a_a' already exists |xxx}]
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
            let ppx_css_variable_set__012_ ?bg_color ?fg_color () =
              let ppx_css_acc__010_ = [] in
              let ppx_css_acc__010_ =
                match bg_color with
                | None -> ppx_css_acc__010_
                | Some ppx_css_value__011_ ->
                    ({|--bg-color|}, ppx_css_value__011_) :: ppx_css_acc__010_ in
              let ppx_css_acc__010_ =
                match fg_color with
                | None -> ppx_css_acc__010_
                | Some ppx_css_value__011_ ->
                    ({|--fg-color_hash_ae072185ae|}, ppx_css_value__011_) ::
                    ppx_css_acc__010_ in
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__010_
            let set = ppx_css_variable_set__012_
            let set_all ~bg_color ~fg_color =
              ppx_css_variable_set__012_ () ~bg_color ~fg_color
          end
        module For_referencing =
          struct
            let a = {|a_hash_ae072185ae|}
            let fg_color = {|--fg-color_hash_ae072185ae|}
            let bg_color = {|--bg-color|}
          end
        let a = Virtual_dom.Vdom.Attr.class_ {|a_hash_ae072185ae|}
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.a_hash_ae072185ae {
     --bg-color:red;
     --fg-color_hash_ae072185ae:blue
    }|}
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
            let ppx_css_variable_set__015_ ?aa ?bb () =
              let ppx_css_acc__013_ = [] in
              let ppx_css_acc__013_ =
                match aa with
                | None -> ppx_css_acc__013_
                | Some ppx_css_value__014_ -> ({|--aa|}, ppx_css_value__014_) ::
                    ppx_css_acc__013_ in
              let ppx_css_acc__013_ =
                match bb with
                | None -> ppx_css_acc__013_
                | Some ppx_css_value__014_ -> ({|--bb|}, ppx_css_value__014_) ::
                    ppx_css_acc__013_ in
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__013_
            let set = ppx_css_variable_set__015_
            let set_all ~aa ~bb = ppx_css_variable_set__015_ () ~aa ~bb
          end
        module For_referencing =
          struct
            let b = {|b_hash_6f1ac654e8|}
            let a = {|a_hash_6f1ac654e8|}
            let bb = {|--bb|}
            let aa = {|--aa|}
          end
        let b = Virtual_dom.Vdom.Attr.class_ {|b_hash_6f1ac654e8|}
        let a = Virtual_dom.Vdom.Attr.class_ {|a_hash_6f1ac654e8|}
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.a_hash_6f1ac654e8 {
     --aa:red
    }

    *.b_hash_6f1ac654e8 {
     --bb:blue
    }|}
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
            let ppx_css_variable_set__018_ ?cm_bg_color () =
              let ppx_css_acc__016_ = [] in
              let ppx_css_acc__016_ =
                match cm_bg_color with
                | None -> ppx_css_acc__016_
                | Some ppx_css_value__017_ ->
                    ({|--cm-bg-color|}, ppx_css_value__017_) :: ppx_css_acc__016_ in
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__016_
            let set = ppx_css_variable_set__018_
            let set_all ~cm_bg_color = ppx_css_variable_set__018_ () ~cm_bg_color
          end
        module For_referencing = struct let cm_bg_color = {|--cm-bg-color|} end
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *:root {
     color:var(--cm-bg-color)
    }|}
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
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    div:asdf(".a .b .c") {

    }

    div:asdf(".a .b .c") {

    }

    div:has(".a .b .c") {

    }

    div:has(".a .b .c") {

    }

    div[foo=".a .b .c"] {

    }

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
            let g = {|g_hash_8f2cc541c7|}
            let c = {|c_hash_8f2cc541c7|}
            let b = {|b_hash_8f2cc541c7|}
            let e = {|e_hash_8f2cc541c7|}
            let f = {|f_hash_8f2cc541c7|}
            let d = {|d_hash_8f2cc541c7|}
            let h = {|h_hash_8f2cc541c7|}
            let a = {|a_hash_8f2cc541c7|}
          end
        let g = Virtual_dom.Vdom.Attr.class_ {|g_hash_8f2cc541c7|}
        let c = Virtual_dom.Vdom.Attr.class_ {|c_hash_8f2cc541c7|}
        let b = Virtual_dom.Vdom.Attr.class_ {|b_hash_8f2cc541c7|}
        let e = Virtual_dom.Vdom.Attr.class_ {|e_hash_8f2cc541c7|}
        let f = Virtual_dom.Vdom.Attr.class_ {|f_hash_8f2cc541c7|}
        let d = Virtual_dom.Vdom.Attr.class_ {|d_hash_8f2cc541c7|}
        let h = Virtual_dom.Vdom.Attr.class_ {|h_hash_8f2cc541c7|}
        let a = Virtual_dom.Vdom.Attr.class_ {|a_hash_8f2cc541c7|}
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    div:has(.a_hash_8f2cc541c7 *.b_hash_8f2cc541c7) {

    }

    div:not(.c_hash_8f2cc541c7+*.d_hash_8f2cc541c7) {

    }

    div:where(.e_hash_8f2cc541c7~*.f_hash_8f2cc541c7) {

    }

    div:is(.g_hash_8f2cc541c7>*.h_hash_8f2cc541c7) {

    }|}
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
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    div:asdf() {

    }

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
    {xxx| Unsafe use of classes or ids [".a"; ".b"; ".c"] within unknown function 'asdf'. Please contact the owners of [ppx_css] so that this CSS function can be audited and added to the allow list in order to resolve this bug. |xxx}]
;;
