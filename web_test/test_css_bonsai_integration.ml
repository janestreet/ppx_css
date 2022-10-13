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
  let handle =
    Handle.create
      (Result_spec.vdom ~censor_hash:false Fn.id)
      (Bonsai.const (Vdom.Node.div ~attr:(Vdom.Attr.class_ Style.foo) []))
  in
  Handle.show handle;
  [%expect {| <div class="foo_hash_6126cb4ca7"> </div> |}]
;;
