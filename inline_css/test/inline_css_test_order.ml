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

let%expect_test "prepending inserts at the front of the list" =
  [ "prepend-j"; "prepend-k"; "prepend-l" ] |> List.iter ~f:Inline_css.Private.prepend;
  print_for_testing ();
  [%expect
    {|
    prepend-l
    prepend-k
    prepend-j
    |}]
;;
