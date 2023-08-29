open! Core

let%expect_test "appending preserves order and deduplicates" =
  [ "a"; "b"; "a"; "c"; "d"; "e"; "e"; "e"; "f"; "g"; "h" ]
  |> List.iter ~f:Inline_css.Private.append;
  Inline_css.For_testing.print ();
  [%expect {|
    a
    b
    c
    d
    e
    f
    g
    h |}]
;;

let%expect_test "Which strategy is being used during tests?" =
  print_endline (Inline_css.For_testing.strategy_name ());
  [%expect {| testing-strategy |}]
;;

let%expect_test "[Strategy.update] is called many times" =
  let add_style_foo () =
    Inline_css.Private.append {|
.foo {
  background-color : blue;
}|}
  in
  add_style_foo ();
  add_style_foo ();
  Inline_css.For_testing.dump_strategy_state ();
  [%expect
    {|
    a
    b
    c
    d
    e
    f
    g
    h

    .foo {
      background-color : blue;
    } |}]
;;
