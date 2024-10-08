open! Core

(* This file constains unit tests for the transform_css module inside ppx_css. *)

module%test Get_all_identifiers = struct
  open Ppx_css.For_testing.Traverse_css

  let test (s : string) =
    let stylesheet = Css_jane.Stylesheet.of_string s in
    let result = Get_all_identifiers.ocaml_identifiers stylesheet in
    print_s (Get_all_identifiers.sexp_of_result result)
  ;;

  let%expect_test "Testing identifiers" =
    test
      {|
        .class {}

        #an_id {}

        .with-kebab-case {}

        .a-b_c {}

        .both {}

        #both {}
    |};
    [%expect
      {|
      ((variables ())
       (identifiers
        ((an_id Only_id) (both Both) (a_b_c Only_class) (class Only_class)
         (with_kebab_case Only_class))))
      |}]
  ;;

  let%expect_test "Testing variables" =
    test
      {|
        :root {
           --with-kebab: 100px;
           --with_snakecase: 100px
        }

        html {
           color: var(--inside-a-var)
        }

    |};
    [%expect
      {| ((variables (with_snakecase with_kebab inside_a_var)) (identifiers ())) |}]
  ;;
end
