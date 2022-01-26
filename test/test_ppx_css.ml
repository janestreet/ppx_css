open Core
open Ppxlib

let loc = Location.none

let catch_location_error ~f =
  try f () with
  | ex ->
    (match Location.Error.of_exn ex with
     | Some error -> print_endline (Location.Error.message error)
     | None -> raise ex)
;;

let test_struct expr =
  catch_location_error ~f:(fun () ->
    let transformed = Ppx_css.For_testing.generate_struct expr in
    let structure =
      match transformed.pmod_desc with
      | Pmod_structure s -> s
      | _ -> assert false
    in
    print_endline (Pprintast.string_of_structure structure))
;;

let test_sig ast =
  catch_location_error ~f:(fun () ->
    let transformed = Ppx_css.For_testing.generate_sig ast in
    print_endline
      (Pprintast.string_of_structure [ [%stri module type S = [%m transformed]] ]))
;;

let%expect_test "basic class" =
  test_struct [%expr {|
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
    module type S  = sig val foo : string end
    type t = (module S)
    module Default = struct let foo = {|foo_hash_33d98f18fa|} end
    include Default
    let default : t = (module Default) |xxx}]
;;

let%expect_test "charset" =
  test_struct [%expr {| @charset "UTF-8"; |}];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let () = Inline_css.Private.append {|
    /* _none_ */

    @charset" UTF-8";|}
    module type S  = sig  end
    type t = (module S)
    module Default = struct  end
    include Default
    let default : t = (module Default) |xxx}]
;;

let%expect_test "nested at rule" =
  test_struct
    [%expr
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
    module type S  = sig val my_foo : string end
    type t = (module S)
    module Default = struct let my_foo = {|my_foo_hash_89c052f1ae|} end
    include Default
    let default : t = (module Default) |xxx}]
;;

let%expect_test "not a string" =
  test_struct [%expr 5];
  [%expect {xxx|
    %css must take a single string as input |xxx}]
;;

let%expect_test "basic id" =
  test_struct [%expr {|
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
    module type S  = sig val foo : string end
    type t = (module S)
    module Default = struct let foo = {|foo_hash_734bfa5411|} end
    include Default
    let default : t = (module Default) |xxx}]
;;

let%expect_test "missing closing brace" =
  test_struct [%expr {|
     #foo {
       background-color: red;
      |}];
  [%expect {xxx|
    Parse error while reading token 'EOF' |xxx}]
;;

let%expect_test "listed identifiers" =
  test_sig [%type: a b c];
  [%expect
    {|
    module type S  =
      sig
        module type S  = sig val a : string val b : string val c : string end
        type t = (module S)
        val default : t
        val a : string
        val b : string
        val c : string
      end |}]
;;

let%expect_test "duplicates sig" =
  test_sig [%type: a a b];
  [%expect
    {|
    module type S  =
      sig
        module type S  = sig val a : string val b : string end
        type t = (module S)
        val default : t
        val a : string
        val b : string
      end |}]
;;

let%expect_test "politicians example" =
  test_sig [%type: politicians];
  [%expect
    {|
    module type S  =
      sig
        module type S  = sig val politicians : string end
        type t = (module S)
        val default : t
        val politicians : string
      end |}]
;;

let%expect_test "animation" =
  test_struct
    [%expr
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
    module type S  = sig val spinner : string end
    type t = (module S)
    module Default = struct let spinner = {|spinner_hash_89c1bc2218|} end
    include Default
    let default : t = (module Default) |xxx}]
;;
