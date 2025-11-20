open! Test_setup_lib

module Style =
  [%css
  stylesheet
    {|
      body {
        background-color: tomato;
      }

      @layer test {
        @layer nested-layer {
          .b {
            grid-template-columns: 1fr;
          }
          .a {
            display: flex;
          }
          @layer another-level {
            .c {
              grid-template-rows: 1fr;
              .d {
              }
              .e {
              }
              .d + .f {
              }
            }
          }
        }

        .q {
        }
        div {
        }
      }

      :where(.a) {
      }

      body div,
      .q {
        display: flex;
      }

      :has(div, .a) {
      }

      div > .a {
        cursor: pointer;
      }

      :has(.a) {
      }

      :has(.a, div) :has(.b, .c) {
      }

      :has(div, .a) :has(.b, .c) {
      }

      .d {
        color: green;

        &:hover {
          outline: blue;
        }
        .e {
          font-size: 14px;
        }
      }

      .a .b {
        display: inline-flex;
      }

      .b {
        flex-direction: column;
      }

      .c {
        display: table;

        &.d {
          text-align: center;
        }
      }
    |}]

let%expect_test "groups are forced properly" =
  clear_before_test ();
  compare_against_prev ();
  (* All auto-forced styles should be here *)
  [%expect
    {xxx|
    === DIFF HUNK ===
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_forced_module/test_laziness_forced_module.ml */
    +|
    +|body {
    +|  background-color: tomato;
    +|}
    +|
    +|
    +|
    +|
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_forced_module/test_laziness_forced_module.ml */
    +|
    +|@layer test {
    +|  div {
    +|  }
    +|}
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_forced_module/test_laziness_forced_module.ml */
    +|
    +|:where(.a_hash_9f9346819b) {
    +|}
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_forced_module/test_laziness_forced_module.ml */
    +|
    +|body div, .q_hash_9f9346819b {
    +|  display: flex;
    +|}
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_forced_module/test_laziness_forced_module.ml */
    +|
    +|:has(div, .a_hash_9f9346819b) {
    +|}
    +|
    +|
    +|
    +|
    +|
    +|
    +|
    |xxx}];
  let handle = css_attr_handle Style.a in
  compare_against_prev ();
  (* Should be the same as above *)
  [%expect {| No change in css |}];
  Handle.show handle;
  [%expect {| <div class="a_hash_replaced_in_test"> hi </div> |}];
  compare_against_prev ();
  (* .a and .b should also show up here *)
  [%expect
    {xxx|
    === DIFF HUNK ===

      /* ppx/ppx_css/test/test_laziness/test_laziness_forced_module/test_laziness_forced_module.ml */

      body {
        background-color: tomato;
      }

    -|
    -|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_forced_module/test_laziness_forced_module.ml */
    +|
    +|@layer test {
    +|  @layer nested-layer {
    +|    .b_hash_9f9346819b {
    +|      grid-template-columns: 1fr;
    +|    }
    +|  }
    +|}
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_forced_module/test_laziness_forced_module.ml */
    +|
    +|@layer test {
    +|  @layer nested-layer {
    +|    .a_hash_9f9346819b {
    +|      display: flex;
    +|    }
    +|  }
    +|}
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_forced_module/test_laziness_forced_module.ml */
    +|
    +|@layer test {
    +|  @layer nested-layer {
    +|    @layer another-level {
    +|      .c_hash_9f9346819b {
    +|        grid-template-rows: 1fr;
    +|        .d_hash_9f9346819b {
    +|        }
    +|        .e_hash_9f9346819b {
    +|        }
    +|        .d_hash_9f9346819b + .f_hash_9f9346819b {
    +|        }
    +|      }
    +|    }
    +|  }
    +|}


      /* ppx/ppx_css/test/test_laziness/test_laziness_forced_module/test_laziness_forced_module.ml */

      @layer test {
        div {
        }
      }

      /* ppx/ppx_css/test/test_laziness/test_laziness_forced_module/test_laziness_forced_module.ml */

      :where(.a_hash_9f9346819b) {
      }

      /* ppx/ppx_css/test/test_laziness/test_laziness_forced_module/test_laziness_forced_module.ml */

      body div, .q_hash_9f9346819b {
        display: flex;
      }

      /* ppx/ppx_css/test/test_laziness/test_laziness_forced_module/test_laziness_forced_module.ml */

      :has(div, .a_hash_9f9346819b) {
      }

    -|
    -|
    -|
    -|
    -|
    -|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_forced_module/test_laziness_forced_module.ml */
    +|
    +|div > .a_hash_9f9346819b {
    +|  cursor: pointer;
    +|}
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_forced_module/test_laziness_forced_module.ml */
    +|
    +|:has(.a_hash_9f9346819b) {
    +|}
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_forced_module/test_laziness_forced_module.ml */
    +|
    +|:has(.a_hash_9f9346819b, div) :has(.b_hash_9f9346819b, .c_hash_9f9346819b) {
    +|}
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_forced_module/test_laziness_forced_module.ml */
    +|
    +|:has(div, .a_hash_9f9346819b) :has(.b_hash_9f9346819b, .c_hash_9f9346819b) {
    +|}
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_forced_module/test_laziness_forced_module.ml */
    +|
    +|.d_hash_9f9346819b {
    +|  color: green;
    +|  &:hover {
    +|    outline: blue;
    +|  }
    +|  .e_hash_9f9346819b {
    +|    font-size: 14px;
    +|  }
    +|}
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_forced_module/test_laziness_forced_module.ml */
    +|
    +|.a_hash_9f9346819b .b_hash_9f9346819b {
    +|  display: inline-flex;
    +|}
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_forced_module/test_laziness_forced_module.ml */
    +|
    +|.b_hash_9f9346819b {
    +|  flex-direction: column;
    +|}
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_forced_module/test_laziness_forced_module.ml */
    +|
    +|.c_hash_9f9346819b {
    +|  display: table;
    +|  &.d_hash_9f9346819b {
    +|    text-align: center;
    +|  }
    +|}
    |xxx}];
  let handle = css_attr_handle Style.b in
  Handle.show handle;
  [%expect {| <div class="b_hash_replaced_in_test"> hi </div> |}];
  compare_against_prev ();
  [%expect {| No change in css |}];
  (* This one should show that ordering within the stylesheet is maintained whenever the
     style is forced. The [.d] class here should appear right after the autoforced ones *)
  let handle = css_attr_handle Style.d in
  Handle.show handle;
  [%expect {| <div class="d_hash_replaced_in_test"> hi </div> |}];
  compare_against_prev ();
  [%expect {xxx| No change in css |xxx}];
  let handle = css_attr_handle Style.b in
  Handle.show handle;
  [%expect {| <div class="b_hash_replaced_in_test"> hi </div> |}];
  compare_against_prev ();
  [%expect {| No change in css |}];
  (* This one should show that ordering within the stylesheet is maintained whenever the
     style is forced. The [.d] class here should appear right after the autoforced ones *)
  let handle = css_attr_handle Style.d in
  Handle.show handle;
  [%expect {| <div class="d_hash_replaced_in_test"> hi </div> |}];
  compare_against_prev ();
  [%expect {xxx| No change in css |xxx}];
  let handle = css_attr_handle Style.b in
  Handle.show handle;
  [%expect {| <div class="b_hash_replaced_in_test"> hi </div> |}];
  compare_against_prev ();
  [%expect {| No change in css |}];
  (* This one should show that ordering within the stylesheet is maintained whenever the
     style is forced. The [.d] class here should appear right after the autoforced ones *)
  let handle = css_attr_handle Style.d in
  Handle.show handle;
  [%expect {| <div class="d_hash_replaced_in_test"> hi </div> |}];
  compare_against_prev ();
  [%expect {xxx| No change in css |xxx}]
;;
