open! Core

let () = Util.reinitialize ()
let () = Async_js.init ()

let to_string () =
  Js_of_ocaml.Js.Unsafe.js_expr
    {js|
     (function () {
       return [...document.adoptedStyleSheets].map((sheet) => sheet.text).join("\n");
     }()
     )
  |js}
  |> Js_of_ocaml.Js.to_string
;;

let print_for_testing () = to_string () |> print_endline

let%expect_test "appending preserves order and deduplicates" =
  [ "a"; "b"; "a"; "c"; "d"; "e"; "e"; "e"; "f"; "g"; "h" ]
  |> List.iter ~f:Inline_css.Private.append;
  print_for_testing ();
  [%expect
    {|
    a
    b
    a
    c
    d
    e
    e
    e
    f
    g
    h
    |}]
;;

let%expect_test "prepending inserts at the front of the list" =
  (* NOTE: prepend-b is _not_ de-duplicated. This is expected. *)
  [ "prepend-a"; "prepend-b"; "prepend-b" ] |> List.iter ~f:Inline_css.Private.prepend;
  print_for_testing ();
  [%expect
    {|
    prepend-b
    prepend-b
    prepend-a
    a
    b
    a
    c
    d
    e
    e
    e
    f
    g
    h
    |}]
;;

let%expect_test "[Strategy.update] is called many times" =
  let add_style_foo () =
    Inline_css.Private.append
      {|
.foo {
  background-color : blue;
}|}
  in
  add_style_foo ();
  add_style_foo ();
  print_for_testing ();
  [%expect
    {|
    prepend-b
    prepend-b
    prepend-a
    a
    b
    a
    c
    d
    e
    e
    e
    f
    g
    h

    .foo {
      background-color : blue;
    }

    .foo {
      background-color : blue;
    }
    |}]
;;
