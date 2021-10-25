open! Core
open! Bonsai_web
open! Bonsai_web_test

let%expect_test "css censoring" =
  let module Style = [%css.raw ".foo {}"] in
  let handle =
    Handle.create
      (Result_spec.vdom Fn.id)
      (Bonsai.const (Vdom.Node.div ~attr:(Vdom.Attr.class_ Style.foo) []))
  in
  Handle.show handle;
  [%expect {| <div class="foo_hash_replaced_in_test"> </div> |}];
  Handle.disable_bonsai_hash_censoring handle;
  Handle.show handle;
  [%expect {| <div class="foo_hash_4d4438f0f3"> </div> |}]
;;
