open! Test_setup_lib

module _ =
  [%css
  stylesheet
    {|
      .a {
        .b {
        }
      }

      @layer test {
        @layer test-2 {
          .c {
          }
        }
      }
    |}]

let%expect_test "sheet should exist and be auto-forced" =
  clear_before_test ();
  print_css ();
  (* All styles should be here *)
  [%expect
    {xxx|
    /* ppx/ppx_css/test/test_laziness/test_turn_off_laziness/test_turn_off_laziness.ml */

    .a_hash_d723b8a4ae {
      .b_hash_d723b8a4ae {
      }
    }

    /* ppx/ppx_css/test/test_laziness/test_turn_off_laziness/test_turn_off_laziness.ml */

    @layer test {
      @layer test-2 {
        .c_hash_d723b8a4ae {
        }
      }
    }
    |xxx}]
;;
