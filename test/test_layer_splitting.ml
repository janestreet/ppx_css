open! Core

let test s =
  Css_parser.(
    parse_stylesheet ~parsing_config:Parsing_config.raise_on_recoverable_errors s)
  |> Ppx_css.For_testing.Traverse_css.split_layers
  |> Css_parser.stylesheet_to_string
  |> print_endline
;;

let%expect_test "Basic layer splitting" =
  test
    {|
    @layer foo {
      .foo { }
      .bar { }
    }
|};
  [%expect
    {|
    @layer foo {
      .foo {
      }
    }
    @layer foo {
      .bar {
      }
    }
    |}]
;;

let%expect_test "Comments" =
  test
    {| @layer foo {
        .foo { }
        .bar { }
        /* Hiii 1 */

         @layer bar {
          .bar-foo { }
          .bar-bar { }
          /* Hiii 2 */
         }
   }
|};
  [%expect
    {|
    @layer foo {
      .foo {
      }
    }
    @layer foo {
      .bar {
      }
    }
    @layer foo {
      /* Hiii 1 */
    }
    @layer foo {
      @layer bar {
        .bar-foo {
        }
      }
    }
    @layer foo {
      @layer bar {
        .bar-bar {
        }
      }
    }
    @layer foo {
      @layer bar {
        /* Hiii 2 */
      }
    }
    |}]
;;
