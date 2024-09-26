open! Core

let print_for_testing =
  let regex = Re.Str.regexp "_hash_\\([a-z0-9]+\\)*" in
  fun () ->
    Inline_css.For_testing.to_string ()
    |> Re.Str.global_replace regex "_hash_replaced_in_test"
    |> print_endline
;;

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

let%expect_test "Which strategy is being used during tests?" =
  print_endline (Inline_css.For_testing.strategy_name ());
  [%expect {| testing-strategy |}]
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
