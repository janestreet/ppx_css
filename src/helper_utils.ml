open! Core

let css_identifier_to_ocaml_identifier =
  let swap_kebab_case =
    String.map ~f:(function
      | '-' -> '_'
      | x -> x)
  in
  Fn.compose swap_kebab_case (String.chop_prefix_if_exists ~prefix:"--")
;;
