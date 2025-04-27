open! Core

(* This file constains unit tests for the transform_css module inside ppx_css. *)

module%test Get_all_identifiers = struct
  open Ppx_css.For_testing.Traverse_css

  let test (s : string) =
    let stylesheet =
      Css_parser.(
        parse_stylesheet ~parsing_config:Parsing_config.raise_on_recoverable_errors s)
      |> Ppx_css.Stable_stylesheet.of_stylesheet
    in
    let result = get_all_identifiers stylesheet in
    print_s [%message (result : Ppx_css.Css_identifier.Set.t)]
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
      (result
       ((Id an_id) (Id both) (Class a-b_c) (Class both) (Class class)
        (Class with-kebab-case)))
      |}]
  ;;

  let%expect_test "Testing variables" =
    test
      {|
        :root {
           --with-kebab: 100px;
           --with_snakecase: 100px;
        }

        html {
           color: var(--inside-a-var);
        }

    |};
    [%expect
      {|
      (result
       ((Variable --inside-a-var) (Variable --with-kebab)
        (Variable --with_snakecase)))
      |}]
  ;;
end
