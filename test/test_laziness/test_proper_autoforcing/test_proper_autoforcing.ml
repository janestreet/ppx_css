open! Test_setup_lib

module _ =
  [%css
  stylesheet
    {|
      body {
        background-color: tomato;
      }

      @media print {
        .a {
        }
        .b {
        }
      }

      @layer test {
        @layer test-2 {
          div {
            .a {
            }
          }
          @layer autoforce-layer {
            a {
            }
            div {
            }
          }

          .dh-specific {
            .g {
              .f {
              }
            }
          }
        }
      }

      .dont-hash-a {
        .test {
        }
      }

      div:not(:has(.not-hashed)) {
      }

      div:is(.a) {
      }

      div::after {
      }

      body div {
        display: flex;
      }
    |}
    ~dont_hash:[ "dh-specific" ]
    ~dont_hash_prefixes:[ "dont-hash" ]]

let%expect_test "autoforcing is done properly (The entire sheet should be forced)" =
  clear_before_test ();
  print_css ();
  (* All auto-forced styles should be here *)
  [%expect
    {xxx|
    /* ppx/ppx_css/test/test_laziness/test_proper_autoforcing/test_proper_autoforcing.ml */

    body {
      background-color: tomato;
    }

    /* ppx/ppx_css/test/test_laziness/test_proper_autoforcing/test_proper_autoforcing.ml */

    @media print {
      .a_hash_eb5dcce9e6 {
      }
      .b_hash_eb5dcce9e6 {
      }
    }

    /* ppx/ppx_css/test/test_laziness/test_proper_autoforcing/test_proper_autoforcing.ml */

    @layer test {
      @layer test-2 {
        div {
          .a_hash_eb5dcce9e6 {
          }
        }
        @layer autoforce-layer {
          a {
          }
          div {
          }
        }
        .dh-specific {
          .g_hash_eb5dcce9e6 {
            .f_hash_eb5dcce9e6 {
            }
          }
        }
      }
    }

    /* ppx/ppx_css/test/test_laziness/test_proper_autoforcing/test_proper_autoforcing.ml */

    .dont-hash-a {
      .test_hash_eb5dcce9e6 {
      }
    }

    /* ppx/ppx_css/test/test_laziness/test_proper_autoforcing/test_proper_autoforcing.ml */

    div:not(:has(.not-hashed_hash_eb5dcce9e6)) {
    }

    /* ppx/ppx_css/test/test_laziness/test_proper_autoforcing/test_proper_autoforcing.ml */

    div:is(.a_hash_eb5dcce9e6) {
    }

    /* ppx/ppx_css/test/test_laziness/test_proper_autoforcing/test_proper_autoforcing.ml */

    div::after {
    }

    /* ppx/ppx_css/test/test_laziness/test_proper_autoforcing/test_proper_autoforcing.ml */

    body div {
      display: flex;
    }
    |xxx}]
;;
