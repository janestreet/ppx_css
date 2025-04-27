open! Test_setup_lib

let%expect_test "make sure inline css laziness works" =
  clear_before_test ();
  print_css ();
  [%expect {| |}];
  let handle =
    vdom_handle
      (Vdom.Node.div ~attrs:[ [%css {|display: flex;|}] ] [ Vdom.Node.text "hi" ])
  in
  print_css ();
  (* Should be empty as the node hasn't actually rendered into the dom yet *)
  [%expect {| |}];
  Handle.show handle;
  [%expect
    {| <div class="test_laziness_forced_inline__inline_class_hash_replaced_in_test"> hi </div> |}];
  print_css ();
  [%expect
    {|
    /* ppx/ppx_css/test/test_laziness/test_laziness_forced_inline/test_laziness_forced_inline.ml */

    .test_laziness_forced_inline__inline_class_hash_d98773e646 {
      display: flex;
    }
    |}]
;;
