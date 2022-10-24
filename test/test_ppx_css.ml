open Core
open Ppxlib
open Ppx_css
open Css_jane

let loc = Location.none

let catch_location_error ~f =
  try f () with
  | ex ->
    (match Location.Error.of_exn ex with
     | Some error -> print_endline (Location.Error.message error)
     | None -> raise ex)
;;

let test_struct ?(allow_potential_accidental_hashing = false) expr =
  catch_location_error ~f:(fun () ->
    let transformed =
      For_testing.generate_struct ~allow_potential_accidental_hashing expr
    in
    let structure =
      match transformed.pmod_desc with
      | Pmod_structure s -> s
      | _ -> assert false
    in
    print_endline (Pprintast.string_of_structure structure))
;;

let test_sig s =
  catch_location_error ~f:(fun () ->
    let transformed = Ppx_css.For_css_inliner.gen_sig s in
    print_endline transformed)
;;

let%expect_test "basic class" =
  test_struct
    [%expr stylesheet {|
     .foo {
       background-color: red;
     }
      |}];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let () =
      Inline_css.Private.append
        {|
    /* _none_ */

    *.foo_hash_33d98f18fa {
     background-color:red
    }|}
    let (__type_info_for_ppx_css :
      ?rewrite:(string * string) list ->
        ?dont_hash:string list ->
          ?dont_hash_prefixes:string list -> string -> unit)
      =
      fun ?rewrite:_ ->
        fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
    module type S  = sig val foo : string end
    type t = (module S)
    module Default = struct let foo = {|foo_hash_33d98f18fa|} end
    include Default
    let default : t = (module Default) |xxx}]
;;

let%expect_test "charset" =
  test_struct [%expr stylesheet {| @charset "UTF-8"; |}];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let () = Inline_css.Private.append {|
    /* _none_ */

    @charset" UTF-8";|}
    let (__type_info_for_ppx_css :
      ?rewrite:(string * string) list ->
        ?dont_hash:string list ->
          ?dont_hash_prefixes:string list -> string -> unit)
      =
      fun ?rewrite:_ ->
        fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
    module type S  = sig  end
    type t = (module S)
    module Default = struct  end
    include Default
    let default : t = (module Default) |xxx}]
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
    let () =
      Inline_css.Private.append
        {|
    /* _none_ */

    @media screen and (min-width:900px){
     div#bar_hash_9b00fea12b {
      color:red
     }

     @media screen and (min-width:900px){
      article.my_foo_hash_9b00fea12b {
       padding:1rem 3rem
      }

     }


    }|}
    let (__type_info_for_ppx_css :
      ?rewrite:(string * string) list ->
        ?dont_hash:string list ->
          ?dont_hash_prefixes:string list -> string -> unit)
      =
      fun ?rewrite:_ ->
        fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
    module type S  = sig val bar : string val my_foo : string end
    type t = (module S)
    module Default =
      struct
        let bar = {|bar_hash_9b00fea12b|}
        let my_foo = {|my_foo_hash_9b00fea12b|}
      end
    include Default
    let default : t = (module Default) |xxx}]
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
    let () =
      Inline_css.Private.append
        {|
    /* _none_ */

    @media screen and (min-width:900px){
     article.my_foo_hash_89c052f1ae {
      padding:1rem 3rem
     }

    }|}
    let (__type_info_for_ppx_css :
      ?rewrite:(string * string) list ->
        ?dont_hash:string list ->
          ?dont_hash_prefixes:string list -> string -> unit)
      =
      fun ?rewrite:_ ->
        fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
    module type S  = sig val my_foo : string end
    type t = (module S)
    module Default = struct let my_foo = {|my_foo_hash_89c052f1ae|} end
    include Default
    let default : t = (module Default) |xxx}]
;;

let%expect_test "no mention of stylesheet function" =
  test_struct [%expr {| .a { }|}];
  [%expect
    {xxx|
    %css must contain a call to [val stylesheet : ?rewrite:(string * string) list -> ?dont_hash:string list -> dont_hash_prefixes:string list -> string -> unit] |xxx}]
;;

let%expect_test "not a string" =
  test_struct [%expr stylesheet 5];
  [%expect
    {xxx|
    ppx_css found unexpected arguments. %css must contain a call to [val stylesheet : ?rewrite:(string * string) list -> ?dont_hash:string list -> dont_hash_prefixes:string list -> string -> unit] |xxx}]
;;

let%expect_test "basic id" =
  test_struct
    [%expr stylesheet {|
     #foo {
       background-color: red;
     }
      |}];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let () =
      Inline_css.Private.append
        {|
    /* _none_ */

    *#foo_hash_734bfa5411 {
     background-color:red
    }|}
    let (__type_info_for_ppx_css :
      ?rewrite:(string * string) list ->
        ?dont_hash:string list ->
          ?dont_hash_prefixes:string list -> string -> unit)
      =
      fun ?rewrite:_ ->
        fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
    module type S  = sig val foo : string end
    type t = (module S)
    module Default = struct let foo = {|foo_hash_734bfa5411|} end
    include Default
    let default : t = (module Default) |xxx}]
;;

let%expect_test "missing closing brace" =
  test_struct [%expr stylesheet {|
     #foo {
       background-color: red;
      |}];
  [%expect {xxx| Parse error while reading token 'EOF' |xxx}]
;;

let%expect_test "listed identifiers" =
  test_sig {|
    .a {}
    #b {}
    .c {}
|};
  [%expect
    {|
      sig
        module type S  = sig val a : string val b : string val c : string end
        type t = (module S)
        val default : t
        val a : string
        val b : string
        val c : string |}]
;;

let%expect_test "duplicates sig" =
  test_sig {|
    .a {}
    #a {}
    .b {}
  |};
  [%expect
    {|
      sig
        module type S  = sig val a : string val b : string end
        type t = (module S)
        val default : t
        val a : string
        val b : string |}]
;;

let%expect_test "politicians example" =
  test_sig {|.politicians {}|};
  [%expect
    {|
      sig
        module type S  = sig val politicians : string end
        type t = (module S)
        val default : t
        val politicians : string |}]
;;

let%expect_test "variables on signature generation" =
  test_sig {|
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
          end
          val card : string
        end
      type t = (module S)
      val default : t
      module Variables :
      sig
        val set :
          ?bg_color:string -> ?fg_color:string -> unit -> Virtual_dom.Vdom.Attr.t
      end
      val card : string |}]
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
    let () =
      Inline_css.Private.append
        {|
    /* _none_ */

    *.spinner_hash_89c1bc2218 {
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
    let (__type_info_for_ppx_css :
      ?rewrite:(string * string) list ->
        ?dont_hash:string list ->
          ?dont_hash_prefixes:string list -> string -> unit)
      =
      fun ?rewrite:_ ->
        fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
    module type S  = sig val spinner : string end
    type t = (module S)
    module Default = struct let spinner = {|spinner_hash_89c1bc2218|} end
    include Default
    let default : t = (module Default) |xxx}]
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
        ~rewrite:[ "dont-hash-me", "dont-hash-me"; "dont-hash-me-id", "dont-hash-me-id" ]];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let () =
      Inline_css.Private.append
        {|
    /* _none_ */

    *.hash-me_hash_1e8ab0f0e7 {
     color:black
    }

    *.dont-hash-me {
     color:green
    }

    *#hash-me-id_hash_1e8ab0f0e7 {
     color:red
    }

    *#dont-hash-me-id {
     color:white
    }|}
    let (__type_info_for_ppx_css :
      ?rewrite:(string * string) list ->
        ?dont_hash:string list ->
          ?dont_hash_prefixes:string list -> string -> unit)
      =
      fun ?rewrite:_ ->
        fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
    module type S  =
      sig
        val dont_hash_me : string
        val dont_hash_me_id : string
        val hash_me : string
        val hash_me_id : string
      end
    type t = (module S)
    module Default =
      struct
        let hash_me_id = {|hash-me-id_hash_1e8ab0f0e7|}
        let hash_me = {|hash-me_hash_1e8ab0f0e7|}
        let dont_hash_me_id = {|dont-hash-me-id|}
        let dont_hash_me = {|dont-hash-me|}
      end
    include Default
    let default : t = (module Default) |xxx}]
;;

let%expect_test "rewrite malformed syntax" =
  test_struct [%expr stylesheet {||} ~rewrite:[ "dont-hash-me"; 1 ]];
  [%expect
    {xxx|
    The rewrite argument to 'stylesheet' must be called with a list literal containing tuple literals,
    where the first element of the tuple must be a string literal (the second element in the tuple can be
    any expression which evaluates to a string.)

    examples:
      stylesheet ~rewrite:[ "foo_bar", "foo-bar" ] (* Rewrites instances of "foo_bar" in the css string to "foo-bar" *)
      stylesheet ~rewrite:[ "foo-bar", "foo-bar" ] (* Prevents the "foo-bar" identifier from being hashed for uniqueness *)
      stylesheet ~rewrite:[ "my_table", My_table_component.table ] (* References an identifier defined in another module *) |xxx}];
  test_struct [%expr stylesheet {||} ~rewrite];
  [%expect
    {xxx|
      The rewrite argument to 'stylesheet' must be called with a list literal containing tuple literals,
      where the first element of the tuple must be a string literal (the second element in the tuple can be
      any expression which evaluates to a string.)

      examples:
        stylesheet ~rewrite:[ "foo_bar", "foo-bar" ] (* Rewrites instances of "foo_bar" in the css string to "foo-bar" *)
        stylesheet ~rewrite:[ "foo-bar", "foo-bar" ] (* Prevents the "foo-bar" identifier from being hashed for uniqueness *)
        stylesheet ~rewrite:[ "my_table", My_table_component.table ] (* References an identifier defined in another module *) |xxx}];
  test_struct [%expr stylesheet {||} ~rewrite:foo];
  [%expect
    {|
      The rewrite argument to 'stylesheet' must be called with a list literal containing tuple literals,
      where the first element of the tuple must be a string literal (the second element in the tuple can be
      any expression which evaluates to a string.)

      examples:
        stylesheet ~rewrite:[ "foo_bar", "foo-bar" ] (* Rewrites instances of "foo_bar" in the css string to "foo-bar" *)
        stylesheet ~rewrite:[ "foo-bar", "foo-bar" ] (* Prevents the "foo-bar" identifier from being hashed for uniqueness *)
        stylesheet ~rewrite:[ "my_table", My_table_component.table ] (* References an identifier defined in another module *) |}];
  test_struct [%expr stylesheet {||} ~rewrite:[ function_call "hi" ]];
  [%expect
    {|
      The rewrite argument to 'stylesheet' must be called with a list literal containing tuple literals,
      where the first element of the tuple must be a string literal (the second element in the tuple can be
      any expression which evaluates to a string.)

      examples:
        stylesheet ~rewrite:[ "foo_bar", "foo-bar" ] (* Rewrites instances of "foo_bar" in the css string to "foo-bar" *)
        stylesheet ~rewrite:[ "foo-bar", "foo-bar" ] (* Prevents the "foo-bar" identifier from being hashed for uniqueness *)
        stylesheet ~rewrite:[ "my_table", My_table_component.table ] (* References an identifier defined in another module *) |}];
  test_struct [%expr stylesheet {||} ?rewrite:None];
  [%expect
    {| ppx_css found unexpected arguments. %css must contain a call to [val stylesheet : ?rewrite:(string * string) list -> ?dont_hash:string list -> dont_hash_prefixes:string list -> string -> unit] |}]
;;

let%expect_test "empty rewrite" =
  test_struct [%expr stylesheet {||} ~rewrite:[]];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let () = Inline_css.Private.append {|
    /* _none_ */

    |}
    let (__type_info_for_ppx_css :
      ?rewrite:(string * string) list ->
        ?dont_hash:string list ->
          ?dont_hash_prefixes:string list -> string -> unit)
      =
      fun ?rewrite:_ ->
        fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
    module type S  = sig  end
    type t = (module S)
    module Default = struct  end
    include Default
    let default : t = (module Default) |xxx}]
;;

let%expect_test "extra args" =
  test_struct [%expr stylesheet {||} ~rewrite:[] extra];
  [%expect
    {xxx|
    ppx_css found unexpected arguments. %css must contain a call to [val stylesheet : ?rewrite:(string * string) list -> ?dont_hash:string list -> dont_hash_prefixes:string list -> string -> unit] |xxx}]
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
    ~allow_potential_accidental_hashing:true
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
    {xxx|
      Unsafe collisions of names. Two different unsafe names map to the same fixed name which might lead to unintended results. Both '--hello-world-1' and 'hello-world_1' map to 'hello_world_1' |xxx}]
;;

let%test_module "Testing the difference between [%css] and [%css.hash_variables]" =
  (module struct
    (* NOTE: The reason for why we need [%css.hash_variables] requires a bit of context. First, if
       we want to hash new kinds of identifiers, this could result in breaking changes if clients
       expect their identifiers to not be hashed. While a tree-smash would normally be helpful in
       these situations, the tree-smash itself is to disable the default behavior, which means
       that clients that haven't rebased on top of the feature will silently have the potential breaking
       changes introduced which might cause confusion. Because of this, a warning is sent warning about the
       change to default behavior which should be explictly expressed/desired with [%css.hash_variables],
       mostly acting as an acknowledgement from users that this is fine to use. At some point in the future,
       this temporary patch will be removed.
    *)

    let%expect_test "Fixing a newly hashed variable." =
      test_struct
        [%expr
          stylesheet {|
         .a {
         --bg-color: white;
         }
         |}];
      [%expect
        {|
         The following identifiers will be hashed when they previously were not: (--bg-color)
         If your application relies on the identifiers, being unhashed, this could
         potentially break the styles of your app. To enable hashing, please use
         [%css.hash_variables] instead of [%css].

         To disable hashing an keep the default behavior you can make use of the [~rewrite]
         flag. To ppx_css. You can do so by adding:

         ~dont_hash:["--bg-color"] |}];
      test_struct
        [%expr
          stylesheet
            {|
       .a {
       --bg-color: white;
       }
       |}
            ~rewrite:[ "--bg-color", "--bg-color" ]];
      [%expect
        {xxx|
     [@@@ocaml.warning "-32"]
     let () =
       Inline_css.Private.append
         {|
     /* _none_ */

     *.a_hash_0e50ba5d7c {
      --bg-color:white
     }|}
     let (__type_info_for_ppx_css :
       ?rewrite:(string * string) list ->
         ?dont_hash:string list ->
           ?dont_hash_prefixes:string list -> string -> unit)
       =
       fun ?rewrite:_ ->
         fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
     module type S  =
       sig
         module Variables :
         sig val set : ?bg_color:string -> unit -> Virtual_dom.Vdom.Attr.t end
         val a : string
       end
     type t = (module S)
     module Default =
       struct
         module Variables =
           struct
             let set ?bg_color  () =
               let ppx_css_acc__001_ = [] in
               let ppx_css_acc__001_ =
                 match bg_color with
                 | None -> ppx_css_acc__001_
                 | Some ppx_css_value__002_ ->
                     ({|--bg-color|}, ppx_css_value__002_) :: ppx_css_acc__001_ in
               Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__001_
           end
         let a = {|a_hash_0e50ba5d7c|}
       end
     include Default
     let default : t = (module Default) |xxx}];
      test_struct
        ~allow_potential_accidental_hashing:true
        [%expr stylesheet {|
       .a {
       --bg-color: white;
       }
       |}];
      [%expect
        {xxx|
     [@@@ocaml.warning "-32"]
     let () =
       Inline_css.Private.append
         {|
     /* _none_ */

     *.a_hash_0e50ba5d7c {
      --bg-color_hash_0e50ba5d7c:white
     }|}
     let (__type_info_for_ppx_css :
       ?rewrite:(string * string) list ->
         ?dont_hash:string list ->
           ?dont_hash_prefixes:string list -> string -> unit)
       =
       fun ?rewrite:_ ->
         fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
     module type S  =
       sig
         module Variables :
         sig val set : ?bg_color:string -> unit -> Virtual_dom.Vdom.Attr.t end
         val a : string
       end
     type t = (module S)
     module Default =
       struct
         module Variables =
           struct
             let set ?bg_color  () =
               let ppx_css_acc__003_ = [] in
               let ppx_css_acc__003_ =
                 match bg_color with
                 | None -> ppx_css_acc__003_
                 | Some ppx_css_value__004_ ->
                     ({|--bg-color_hash_0e50ba5d7c|}, ppx_css_value__004_) ::
                     ppx_css_acc__003_ in
               Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__003_
           end
         let a = {|a_hash_0e50ba5d7c|}
       end
     include Default
     let default : t = (module Default) |xxx}]
    ;;

    let%expect_test "Fixing a newly hashed class inside of a pseudoselector" =
      test_struct [%expr stylesheet {|
           :where(.a) { }
           |}];
      [%expect
        {xxx|
             The following identifiers will be hashed when they previously were not: (a)
             If your application relies on the identifiers, being unhashed, this could
             potentially break the styles of your app. To enable hashing, please use
             [%css.hash_variables] instead of [%css].

             To disable hashing an keep the default behavior you can make use of the [~rewrite]
             flag. To ppx_css. You can do so by adding:

             ~dont_hash:["a"] |xxx}];
      test_struct
        [%expr stylesheet {|
       :where(.a) { }
       |} ~rewrite:[ "a", "a" ]];
      [%expect
        {xxx|
     [@@@ocaml.warning "-32"]
     let () = Inline_css.Private.append {|
     /* _none_ */

     *:where(.a) {

     }|}
     let (__type_info_for_ppx_css :
       ?rewrite:(string * string) list ->
         ?dont_hash:string list ->
           ?dont_hash_prefixes:string list -> string -> unit)
       =
       fun ?rewrite:_ ->
         fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
     module type S  = sig val a : string end
     type t = (module S)
     module Default = struct let a = {|a|} end
     include Default
     let default : t = (module Default) |xxx}];
      test_struct
        ~allow_potential_accidental_hashing:true
        [%expr stylesheet {|
       :where(.a) { }
       |}];
      [%expect
        {xxx|
     [@@@ocaml.warning "-32"]
     let () =
       Inline_css.Private.append
         {|
     /* _none_ */

     *:where(.a_hash_17f4534609) {

     }|}
     let (__type_info_for_ppx_css :
       ?rewrite:(string * string) list ->
         ?dont_hash:string list ->
           ?dont_hash_prefixes:string list -> string -> unit)
       =
       fun ?rewrite:_ ->
         fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
     module type S  = sig val a : string end
     type t = (module S)
     module Default = struct let a = {|a_hash_17f4534609|} end
     include Default
     let default : t = (module Default) |xxx}]
    ;;
  end)
;;

let%expect_test "collisions between [~rewrite] keys." =
  test_struct [%expr stylesheet {|
    .a { }
  |} ~rewrite:[ "a", "a"; "a", "b" ]];
  [%expect {xxx| Found duplicate key "a" inside of [rewrite]. |xxx}]
;;

let%expect_test "simple use of [~rewrite]." =
  test_struct [%expr stylesheet {|
    .a { }
  |} ~rewrite:[ "a", M.a ]];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let () =
      Inline_css.Private.append
        (Base.Printf.sprintf {|
    /* _none_ */

    *.%s {

    }|} M.a)
    let (__type_info_for_ppx_css :
      ?rewrite:(string * string) list ->
        ?dont_hash:string list ->
          ?dont_hash_prefixes:string list -> string -> unit)
      =
      fun ?rewrite:_ ->
        fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
    module type S  = sig val a : string end
    type t = (module S)
    module Default = struct let a = M.a end
    include Default
    let default : t = (module Default) |xxx}]
;;

let%expect_test "simple use of [~rewrite] through a string constant." =
  test_struct [%expr stylesheet {| .a { color: green;
    }
  |} ~rewrite:[ "a", "a" ]];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let () = Inline_css.Private.append {|
    /* _none_ */

    *.a {
     color:green
    }|}
    let (__type_info_for_ppx_css :
      ?rewrite:(string * string) list ->
        ?dont_hash:string list ->
          ?dont_hash_prefixes:string list -> string -> unit)
      =
      fun ?rewrite:_ ->
        fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
    module type S  = sig val a : string end
    type t = (module S)
    module Default = struct let a = {|a|} end
    include Default
    let default : t = (module Default) |xxx}]
;;

let%expect_test "weirder ordering of [~rewrite]." =
  test_struct
    [%expr
      stylesheet
        ~rewrite:[ "a", M.a; "b", M.b; "c", c; "d", "d" ]
        {|
    .a {}
    .other {}
    .c::before {}
    .d {}
    .c {}
    .b {}
    .a {}
  |}];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let () =
      Inline_css.Private.append
        (Base.Printf.sprintf
           {|
    /* _none_ */

    *.%s {

    }

    *.other_hash_46adddeee6 {

    }

    *.%s::before {

    }

    *.d {

    }

    *.%s {

    }

    *.%s {

    }

    *.%s {

    }|}
           M.a c c M.b M.a)
    let (__type_info_for_ppx_css :
      ?rewrite:(string * string) list ->
        ?dont_hash:string list ->
          ?dont_hash_prefixes:string list -> string -> unit)
      =
      fun ?rewrite:_ ->
        fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
    module type S  =
      sig
        val a : string
        val b : string
        val c : string
        val d : string
        val other : string
      end
    type t = (module S)
    module Default =
      struct
        let c = c
        let other = {|other_hash_46adddeee6|}
        let b = M.b
        let d = {|d|}
        let a = M.a
      end
    include Default
    let default : t = (module Default) |xxx}]
;;

let%expect_test "both use of string constants and arbitrary expressions in use." =
  test_struct
    [%expr
      stylesheet
        {|
    .a { }
    .b { }
    .c { }
  |}
        ~rewrite:[ "a", f M.a; "b", "b_but_unhashed" ]];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let () =
      Inline_css.Private.append
        (Base.Printf.sprintf
           {|
    /* _none_ */

    *.%s {

    }

    *.b_but_unhashed {

    }

    *.c_hash_0c073e40ac {

    }|}
           (f M.a))
    let (__type_info_for_ppx_css :
      ?rewrite:(string * string) list ->
        ?dont_hash:string list ->
          ?dont_hash_prefixes:string list -> string -> unit)
      =
      fun ?rewrite:_ ->
        fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
    module type S  = sig val a : string val b : string val c : string end
    type t = (module S)
    module Default =
      struct
        let c = {|c_hash_0c073e40ac|}
        let b = {|b_but_unhashed|}
        let a = f M.a
      end
    include Default
    let default : t = (module Default) |xxx}]
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
      ~allow_potential_accidental_hashing:true
      ~rewrite:String.Map.empty
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

let%expect_test "unused [~rewrite] target warning" =
  test_struct [%expr stylesheet {|
    .a { }
  |} ~rewrite:[ "b", "b" ]];
  [%expect {xxx|
    Unused keys: (b) |xxx}]
;;

let%test_module "css_inliner_tests" =
  (module struct
    let test_sig_and_struct s =
      let struct_ = For_css_inliner.gen_struct ~options:(Options.empty ~css_string:s) in
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
          [@@@ocaml.warning "-32"]
          let () =
            Inline_css.Private.append
              {|
        /* _none_ */

        *.a_hash_0c732ee1e6 {
         color:green
        }

        *.b_hash_0c732ee1e6 {
         color:white
        }|}
          let (__type_info_for_ppx_css :
            ?rewrite:(string * string) list ->
              ?dont_hash:string list ->
                ?dont_hash_prefixes:string list -> string -> unit)
            =
            fun ?rewrite:_ ->
              fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
          module type S  = sig val a : string val b : string end
          type t = (module S)
          module Default =
            struct let b = {|b_hash_0c732ee1e6|}
                   let a = {|a_hash_0c732ee1e6|} end
          include Default
          let default : t = (module Default)
        ==sig==
        sig
          module type S  = sig val a : string val b : string end
          type t = (module S)
          val default : t
          val a : string
          val b : string |xxx}]
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
          [@@@ocaml.warning "-32"]
          let () =
            Inline_css.Private.append
              {|
        /* _none_ */

        *.a_hash_7dfc841c58 {
         color:green
        }

        *.a_hash_7dfc841c58 {
         color:white
        }|}
          let (__type_info_for_ppx_css :
            ?rewrite:(string * string) list ->
              ?dont_hash:string list ->
                ?dont_hash_prefixes:string list -> string -> unit)
            =
            fun ?rewrite:_ ->
              fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
          module type S  = sig val a : string end
          type t = (module S)
          module Default = struct let a = {|a_hash_7dfc841c58|} end
          include Default
          let default : t = (module Default)
        ==sig==
        sig
          module type S  = sig val a : string end
          type t = (module S)
          val default : t
          val a : string |xxx}]
    ;;
  end)
;;

let%expect_test "rewrite on identifier which would eventually get unkebab'ed" =
  test_struct [%expr stylesheet {| .a-b { } |} ~rewrite:[ "a-b", "a_b" ]];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let () = Inline_css.Private.append {|
    /* _none_ */

    *.a_b {

    }|}
    let (__type_info_for_ppx_css :
      ?rewrite:(string * string) list ->
        ?dont_hash:string list ->
          ?dont_hash_prefixes:string list -> string -> unit)
      =
      fun ?rewrite:_ ->
        fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
    module type S  = sig val a_b : string end
    type t = (module S)
    module Default = struct let a_b = {|a_b|} end
    include Default
    let default : t = (module Default) |xxx}]
;;

let%expect_test "unused [~rewrite] target warning" =
  test_struct [%expr stylesheet {|
    .a { }
  |} ~rewrite:[ "b", "b" ]];
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
      let () =
        Inline_css.Private.append
          {|
      /* _none_ */

      *.b {

      }

      *.bb_hash_267f25fb39 {

      }

      *.a {

      }

      *.aa {

      }|}
      let (__type_info_for_ppx_css :
        ?rewrite:(string * string) list ->
          ?dont_hash:string list ->
            ?dont_hash_prefixes:string list -> string -> unit)
        =
        fun ?rewrite:_ ->
          fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
      module type S  =
        sig val a : string val aa : string val b : string val bb : string end
      type t = (module S)
      module Default =
        struct
          let bb = {|bb_hash_267f25fb39|}
          let b = {|b|}
          let a = {|a|}
          let aa = {|aa|}
        end
      include Default
      let default : t = (module Default) |xxx}]
;;

let%expect_test "Both don't_hash and dont_hash are used" =
  test_struct [%expr stylesheet ~don't_hash:[ "b" ] ~dont_hash:[ "b" ] {|
    .b { }
  |}];
  [%expect
    {| ppx_css found unexpected arguments. Found two uses of alternate syntax in the same place which might be ambiguous/result in unexpected results. Only use 1 of "dont_hash" or "don't_hash". |}]
;;

let%expect_test "ppx_css hashes variables" =
  test_struct
    ~allow_potential_accidental_hashing:true
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
    let () =
      Inline_css.Private.append
        {|
    /* _none_ */

    *:root {
     --my-variable_hash_b519a9dc79:green
    }

    *.a_hash_b519a9dc79 {
     background-color:var(--my-variable_hash_b519a9dc79,default)
    }|}
    let (__type_info_for_ppx_css :
      ?rewrite:(string * string) list ->
        ?dont_hash:string list ->
          ?dont_hash_prefixes:string list -> string -> unit)
      =
      fun ?rewrite:_ ->
        fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
    module type S  =
      sig
        module Variables :
        sig val set : ?my_variable:string -> unit -> Virtual_dom.Vdom.Attr.t end
        val a : string
      end
    type t = (module S)
    module Default =
      struct
        module Variables =
          struct
            let set ?my_variable  () =
              let ppx_css_acc__005_ = [] in
              let ppx_css_acc__005_ =
                match my_variable with
                | None -> ppx_css_acc__005_
                | Some ppx_css_value__006_ ->
                    ({|--my-variable_hash_b519a9dc79|}, ppx_css_value__006_) ::
                    ppx_css_acc__005_ in
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__005_
          end
        let a = {|a_hash_b519a9dc79|}
      end
    include Default
    let default : t = (module Default) |xxx}]
;;

let%expect_test "nested variables" =
  test_struct
    ~allow_potential_accidental_hashing:true
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
    let () =
      Inline_css.Private.append
        {|
    /* _none_ */

    *:root {
     --a_hash_0ac8471275:green;
     --b_hash_0ac8471275:green;
     --c_hash_0ac8471275:green
    }

    *.navbar_hash_0ac8471275 {
     background-color:var(--a_hash_0ac8471275,var(--b_hash_0ac8471275,(var(--c_hash_0ac8471275))))
    }|}
    let (__type_info_for_ppx_css :
      ?rewrite:(string * string) list ->
        ?dont_hash:string list ->
          ?dont_hash_prefixes:string list -> string -> unit)
      =
      fun ?rewrite:_ ->
        fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
    module type S  =
      sig
        module Variables :
        sig
          val set :
            ?a:string ->
              ?b:string -> ?c:string -> unit -> Virtual_dom.Vdom.Attr.t
        end
        val navbar : string
      end
    type t = (module S)
    module Default =
      struct
        module Variables =
          struct
            let set ?a  ?b  ?c  () =
              let ppx_css_acc__007_ = [] in
              let ppx_css_acc__007_ =
                match a with
                | None -> ppx_css_acc__007_
                | Some ppx_css_value__008_ ->
                    ({|--a_hash_0ac8471275|}, ppx_css_value__008_) ::
                    ppx_css_acc__007_ in
              let ppx_css_acc__007_ =
                match b with
                | None -> ppx_css_acc__007_
                | Some ppx_css_value__008_ ->
                    ({|--b_hash_0ac8471275|}, ppx_css_value__008_) ::
                    ppx_css_acc__007_ in
              let ppx_css_acc__007_ =
                match c with
                | None -> ppx_css_acc__007_
                | Some ppx_css_value__008_ ->
                    ({|--c_hash_0ac8471275|}, ppx_css_value__008_) ::
                    ppx_css_acc__007_ in
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__007_
          end
        let navbar = {|navbar_hash_0ac8471275|}
      end
    include Default
    let default : t = (module Default) |xxx}]
;;

let%expect_test "css variables still respect [~rewrite]" =
  test_struct
    ~allow_potential_accidental_hashing:true
    [%expr
      stylesheet
        ~rewrite:[ "--a", "--a"; "--b", Other_library.b ]
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
    let () =
      Inline_css.Private.append
        (Base.Printf.sprintf
           {|
    /* _none_ */

    *:root {
     --a:green;
     %s:green;
     --c_hash_0ac8471275:green
    }

    *.navbar_hash_0ac8471275 {
     background-color:var(--a,var(%s,(var(--c_hash_0ac8471275))))
    }|}
           Other_library.b Other_library.b)
    let (__type_info_for_ppx_css :
      ?rewrite:(string * string) list ->
        ?dont_hash:string list ->
          ?dont_hash_prefixes:string list -> string -> unit)
      =
      fun ?rewrite:_ ->
        fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
    module type S  =
      sig
        module Variables :
        sig
          val set :
            ?a:string ->
              ?b:string -> ?c:string -> unit -> Virtual_dom.Vdom.Attr.t
        end
        val navbar : string
      end
    type t = (module S)
    module Default =
      struct
        module Variables =
          struct
            let set ?a  ?b  ?c  () =
              let ppx_css_acc__009_ = [] in
              let ppx_css_acc__009_ =
                match a with
                | None -> ppx_css_acc__009_
                | Some ppx_css_value__010_ -> ({|--a|}, ppx_css_value__010_) ::
                    ppx_css_acc__009_ in
              let ppx_css_acc__009_ =
                match b with
                | None -> ppx_css_acc__009_
                | Some ppx_css_value__010_ ->
                    (Other_library.b, ppx_css_value__010_) :: ppx_css_acc__009_ in
              let ppx_css_acc__009_ =
                match c with
                | None -> ppx_css_acc__009_
                | Some ppx_css_value__010_ ->
                    ({|--c_hash_0ac8471275|}, ppx_css_value__010_) ::
                    ppx_css_acc__009_ in
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__009_
          end
        let navbar = {|navbar_hash_0ac8471275|}
      end
    include Default
    let default : t = (module Default) |xxx}]
;;

let%expect_test "css variables still hashed under pseudo-selectors" =
  test_struct
    ~allow_potential_accidental_hashing:true
    [%expr stylesheet {|
    :not(.navbar) { }
  |} ~rewrite:[ "navbar", "navbar_hash" ]];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let () =
      Inline_css.Private.append {|
    /* _none_ */

    *:not(.navbar_hash) {

    }|}
    let (__type_info_for_ppx_css :
      ?rewrite:(string * string) list ->
        ?dont_hash:string list ->
          ?dont_hash_prefixes:string list -> string -> unit)
      =
      fun ?rewrite:_ ->
        fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
    module type S  = sig val navbar : string end
    type t = (module S)
    module Default = struct let navbar = {|navbar_hash|} end
    include Default
    let default : t = (module Default) |xxx}]
;;

let%expect_test "enumeration of supported selector functions" =
  test_struct
    [%expr
      stylesheet
        {|
    :has(.has) { }

    :not(.not) { }

    :where(.where) { }
  |}
        ~rewrite:[ "has", "has_hash"; "where", "where_hash"; "not", "not_hash" ]];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let () =
      Inline_css.Private.append
        {|
    /* _none_ */

    *:has(.has_hash) {

    }

    *:not(.not_hash) {

    }

    *:where(.where_hash) {

    }|}
    let (__type_info_for_ppx_css :
      ?rewrite:(string * string) list ->
        ?dont_hash:string list ->
          ?dont_hash_prefixes:string list -> string -> unit)
      =
      fun ?rewrite:_ ->
        fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
    module type S  = sig val has : string val not : string val where : string end
    type t = (module S)
    module Default =
      struct
        let has = {|has_hash|}
        let not = {|not_hash|}
        let where = {|where_hash|}
      end
    include Default
    let default : t = (module Default) |xxx}]
;;

let%expect_test "more complicated nested identifiers within selector functions" =
  test_struct
    ~allow_potential_accidental_hashing:true
    [%expr
      stylesheet
        {|
    :not(.a > :dir(.d) > .c:has(.navbar)) { }
  |}
        ~rewrite:[ "a", "a_hash"; "c", "c_hash"; "navbar", "navbar_hash" ]];
  (* Notice that ".d" is not hashed because it is placed in invalid places. *)
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let () =
      Inline_css.Private.append
        {|
    /* _none_ */

    *:not(.a_hash>:dir(.d)>.c_hash:has(.navbar_hash)) {

    }|}
    let (__type_info_for_ppx_css :
      ?rewrite:(string * string) list ->
        ?dont_hash:string list ->
          ?dont_hash_prefixes:string list -> string -> unit)
      =
      fun ?rewrite:_ ->
        fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
    module type S  = sig val a : string val c : string val navbar : string end
    type t = (module S)
    module Default =
      struct let c = {|c_hash|}
             let navbar = {|navbar_hash|}
             let a = {|a_hash|} end
    include Default
    let default : t = (module Default) |xxx}]
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
    let () =
      Inline_css.Private.append
        {|
    /* _none_ */

    has(.a) {
     background-color::has(.a);
     background-color:has(.a)
    }|}
    let (__type_info_for_ppx_css :
      ?rewrite:(string * string) list ->
        ?dont_hash:string list ->
          ?dont_hash_prefixes:string list -> string -> unit)
      =
      fun ?rewrite:_ ->
        fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
    module type S  = sig  end
    type t = (module S)
    module Default = struct  end
    include Default
    let default : t = (module Default) |xxx}]
;;

let%expect_test "behavior on sharing of id and class names" =
  test_struct [%expr stylesheet {|
    .a {}
    #a {}
                    |}];
  [%expect
    {xxx|
      [@@@ocaml.warning "-32"]
      let () =
        Inline_css.Private.append
          {|
      /* _none_ */

      *.a_hash_b7f7689df5 {

      }

      *#a_hash_b7f7689df5 {

      }|}
      let (__type_info_for_ppx_css :
        ?rewrite:(string * string) list ->
          ?dont_hash:string list ->
            ?dont_hash_prefixes:string list -> string -> unit)
        =
        fun ?rewrite:_ ->
          fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
      module type S  = sig val a : string end
      type t = (module S)
      module Default = struct let a = {|a_hash_b7f7689df5|} end
      include Default
      let default : t = (module Default) |xxx}];
  test_sig {|
    .a {}
    #a {}
                    |};
  [%expect
    {|
    sig
      module type S  = sig val a : string end
      type t = (module S)
      val default : t
      val a : string |}]
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
    let () =
      Inline_css.Private.append
        {|
    /* _none_ */

    *.a-class {
     --a-variable:red
    }

    *#an_id {

    }|}
    let (__type_info_for_ppx_css :
      ?rewrite:(string * string) list ->
        ?dont_hash:string list ->
          ?dont_hash_prefixes:string list -> string -> unit)
      =
      fun ?rewrite:_ ->
        fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
    module type S  =
      sig
        module Variables :
        sig val set : ?a_variable:string -> unit -> Virtual_dom.Vdom.Attr.t end
        val a_class : string
        val an_id : string
      end
    type t = (module S)
    module Default =
      struct
        module Variables =
          struct
            let set ?a_variable  () =
              let ppx_css_acc__011_ = [] in
              let ppx_css_acc__011_ =
                match a_variable with
                | None -> ppx_css_acc__011_
                | Some ppx_css_value__012_ ->
                    ({|--a-variable|}, ppx_css_value__012_) :: ppx_css_acc__011_ in
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__011_
          end
        let an_id = {|an_id|}
        let a_class = {|a-class|}
      end
    include Default
    let default : t = (module Default) |xxx}]
;;

let%expect_test "Clashes caused by the rename to snake case from kebab case trigger a \
                 warning"
  =
  test_struct [%expr stylesheet {| .a-a {}  .a_a {} |}];
  [%expect
    {xxx|
    Unsafe collision of names. Cannot rename 'a-a' to 'a_a' because 'a_a' already exists |xxx}]
;;

let%expect_test "Unused warnings also apply to [dont_hash]" =
  test_struct [%expr stylesheet {| .a {} |} ~dont_hash:[ "b" ]];
  [%expect {| Unused keys: (b) |}]
;;

let%expect_test "Duplicate values given to [dont_hash]" =
  test_struct [%expr stylesheet {| .a-a {}  |} ~dont_hash:[ "a-a"; "a-a" ]];
  [%expect {| Found duplicate values (a-a) inside of [dont_hash]. |}]
;;

let%expect_test "Duplicate values between [dont_hash] and [rewrite]" =
  test_struct [%expr stylesheet {| .a {}  |} ~dont_hash:[ "a" ] ~rewrite:[ "a", "a2" ]];
  [%expect {| Found duplicate value \"a\" between [dont_hash] and [rewrite]. |}]
;;

let%expect_test "[dont_hash] syntax error" =
  test_struct [%expr stylesheet {| .a {}  |} ~dont_hash:[ not_a_string_constant ]];
  [%expect
    {|
    The dont_hash argument to 'stylesheet' must be called with a list literal containing string literals.

    example:
      stylesheet ~dont_hash:[ "foo_bar" ] (* Does not hash instances of "foo_bar". *)
      stylesheet ~dont_hash:[ "--bg-color" ] (* Does not hash instances of "--bg-color". *) |}]
;;

let%expect_test "[dont_hash] syntax error" =
  test_struct [%expr stylesheet {| .a {}  |} ~dont_hash:not_a_string_literal];
  [%expect
    {|
    The dont_hash argument to 'stylesheet' must be called with a list literal containing string literals.

    example:
      stylesheet ~dont_hash:[ "foo_bar" ] (* Does not hash instances of "foo_bar". *)
      stylesheet ~dont_hash:[ "--bg-color" ] (* Does not hash instances of "--bg-color". *) |}]
;;

let%expect_test "dont_hash_prefixes" =
  test_struct
    ~allow_potential_accidental_hashing:true
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
    let () =
      Inline_css.Private.append
        {|
    /* _none_ */

    *.a_hash_785a41f00f {
     --bg-color:red;
     --fg-color_hash_785a41f00f:blue
    }|}
    let (__type_info_for_ppx_css :
      ?rewrite:(string * string) list ->
        ?dont_hash:string list ->
          ?dont_hash_prefixes:string list -> string -> unit)
      =
      fun ?rewrite:_ ->
        fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
    module type S  =
      sig
        module Variables :
        sig
          val set :
            ?bg_color:string ->
              ?fg_color:string -> unit -> Virtual_dom.Vdom.Attr.t
        end
        val a : string
      end
    type t = (module S)
    module Default =
      struct
        module Variables =
          struct
            let set ?bg_color  ?fg_color  () =
              let ppx_css_acc__013_ = [] in
              let ppx_css_acc__013_ =
                match bg_color with
                | None -> ppx_css_acc__013_
                | Some ppx_css_value__014_ ->
                    ({|--bg-color|}, ppx_css_value__014_) :: ppx_css_acc__013_ in
              let ppx_css_acc__013_ =
                match fg_color with
                | None -> ppx_css_acc__013_
                | Some ppx_css_value__014_ ->
                    ({|--fg-color_hash_785a41f00f|}, ppx_css_value__014_) ::
                    ppx_css_acc__013_ in
              Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__013_
          end
        let a = {|a_hash_785a41f00f|}
      end
    include Default
    let default : t = (module Default) |xxx}]
;;

let%expect_test "dont_hash_prefixes accidental shadowing" =
  test_struct
    ~allow_potential_accidental_hashing:true
    [%expr
      stylesheet
        {|
    .a {
      --bg-color: red;
      --fg-color: blue;
    }
                    |}
        ~dont_hash_prefixes:[ "--bg"; "--" ]];
  [%expect {xxx|
    Unused prefixes: (--bg) |xxx}]
;;

let%expect_test "dont_hash_prefixes no mention of prefixes" =
  test_struct
    ~allow_potential_accidental_hashing:true
    [%expr stylesheet {|
    .a { }
                    |} ~dont_hash_prefixes:[ "--" ]];
  [%expect {xxx|
    Unused prefixes: (--) |xxx}]
;;

let%expect_test "[~rewrite] takes priority over [~dont_hash_prefixes]." =
  test_struct
    ~allow_potential_accidental_hashing:true
    [%expr
      stylesheet
        {|
    .abcde { }
    .abc {}
                    |}
        ~rewrite:[ "abcde", "i-take-priority" ]
        ~dont_hash_prefixes:[ "ab" ]];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let () =
      Inline_css.Private.append
        {|
    /* _none_ */

    *.i-take-priority {

    }

    *.abc {

    }|}
    let (__type_info_for_ppx_css :
      ?rewrite:(string * string) list ->
        ?dont_hash:string list ->
          ?dont_hash_prefixes:string list -> string -> unit)
      =
      fun ?rewrite:_ ->
        fun ?dont_hash:_ -> fun ?dont_hash_prefixes:_ -> fun _ -> ()
    module type S  = sig val abc : string val abcde : string end
    type t = (module S)
    module Default = struct let abcde = {|i-take-priority|}
                            let abc = {|abc|} end
    include Default
    let default : t = (module Default) |xxx}]
;;

let%expect_test "Unused/possibly redundant/clashing [~dont_hash_prefixes] reuslts in a \
                 static warning"
  =
  test_struct
    ~allow_potential_accidental_hashing:true
    [%expr
      stylesheet
        {|
    .abcde { }
    .a12345 { }
                    |}
        ~rewrite:[ "abcde", "i-take-priority" ]
        (* "ab" is shadowed by rewrite's "abcde", and both "a12345" and "a123" apply to "a123456" so the longest one of them is rendundant. *)
        ~dont_hash_prefixes:[ "ab"; "a12345"; "a123" ]];
  [%expect {xxx|
    Unused prefixes: (a12345 ab) |xxx}]
;;

let%expect_test "[dont_hash_prefixes] syntax error" =
  test_struct
    [%expr stylesheet {| .a {}  |} ~dont_hash_prefixes:[ not_a_string_constant ]];
  [%expect
    {|
    The dont_hash_prefixes argument to 'stylesheet' must be called with a list literal containing string literals.

    example:
      stylesheet ~dont_hash_prefixes:[ "--bg" ] (* Does not hashes identifiers that start with "--bg" (e.g. "--bg-color"). *)
      stylesheet ~dont_hash_prefixes:[ "--" ] (* Does not hash css variables. *) |}]
;;

let%expect_test "[dont_hash_prefixes] syntax error" =
  test_struct [%expr stylesheet {| .a {}  |} ~dont_hash_prefixes:not_a_string_literal];
  [%expect
    {|
    The dont_hash_prefixes argument to 'stylesheet' must be called with a list literal containing string literals.

    example:
      stylesheet ~dont_hash_prefixes:[ "--bg" ] (* Does not hashes identifiers that start with "--bg" (e.g. "--bg-color"). *)
      stylesheet ~dont_hash_prefixes:[ "--" ] (* Does not hash css variables. *) |}]
;;

