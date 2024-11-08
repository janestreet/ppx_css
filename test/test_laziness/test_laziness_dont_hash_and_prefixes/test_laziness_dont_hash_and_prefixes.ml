include Test_setup_lib

module Style =
  [%css
  stylesheet
    {|
      body {
        background-color: tomato;
      }

      @layer test {
        .dont-a {
          .b {
          }
          .e {
          }
        }

        @layer nested-layer {
          .b {
            grid-template-columns: 1fr;
          }
          .a {
            display: flex;
          }
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

        .q {
        }
        div {
        }
      }

      .dont-b {
        .b {
        }
        .c {
        }
      }

      body div {
        display: flex;
      }

      div > .a {
        pointer: cursor;
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
      |}
    ~dont_hash:[ "q"; "c" ]
    ~dont_hash_prefixes:[ "dont" ]]

let%expect_test "groups are forced properly" =
  clear_before_test ();
  compare_against_prev ();
  (* All auto-forced styles should be here *)
  [%expect
    {xxx|
    -1,0 +1,100
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_dont_hash_and_prefixes/test_laziness_dont_hash_and_prefixes.ml */
    +|
    +|body {
    +| background-color:tomato
    +|}
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_dont_hash_and_prefixes/test_laziness_dont_hash_and_prefixes.ml */
    +|
    +|@layer test{
    +| *.dont-a {
    +|  *.b_hash_16e3b6ad26 {
    +|
    +|  }
    +|  ;
    +|  *.e_hash_16e3b6ad26 {
    +|
    +|  }
    +|
    +| }
    +|
    +|}
    +|
    +|
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_dont_hash_and_prefixes/test_laziness_dont_hash_and_prefixes.ml */
    +|
    +|@layer test{
    +| @layer nested-layer{
    +|  *.c {
    +|   grid-template-rows:1fr;
    +|   *.d_hash_16e3b6ad26 {
    +|
    +|   }
    +|   ;
    +|   *.e_hash_16e3b6ad26 {
    +|
    +|   }
    +|   ;
    +|   *.d_hash_16e3b6ad26+*.f_hash_16e3b6ad26 {
    +|
    +|   }
    +|
    +|  }
    +|
    +| }
    +|
    +|
    +|}
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_dont_hash_and_prefixes/test_laziness_dont_hash_and_prefixes.ml */
    +|
    +|@layer test{
    +| *.q {
    +|
    +| }
    +|
    +|}
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_dont_hash_and_prefixes/test_laziness_dont_hash_and_prefixes.ml */
    +|
    +|@layer test{
    +| div {
    +|
    +| }
    +|
    +|}
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_dont_hash_and_prefixes/test_laziness_dont_hash_and_prefixes.ml */
    +|
    +|*.dont-b {
    +| *.b_hash_16e3b6ad26 {
    +|
    +| }
    +| ;
    +| *.c {
    +|
    +| }
    +|
    +|}
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_dont_hash_and_prefixes/test_laziness_dont_hash_and_prefixes.ml */
    +|
    +|body div {
    +| display:flex
    +|}
    +|
    +|
    +|
    +|
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_dont_hash_and_prefixes/test_laziness_dont_hash_and_prefixes.ml */
    +|
    +|*.c {
    +| display:table;
    +| *&.d_hash_16e3b6ad26 {
    +|  text-align:center
    +| }
    +|
    +|}
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
    -8,33 +8,57
      /* ppx/ppx_css/test/test_laziness/test_laziness_dont_hash_and_prefixes/test_laziness_dont_hash_and_prefixes.ml */

      @layer test{
       *.dont-a {
        *.b_hash_16e3b6ad26 {

        }
        ;
        *.e_hash_16e3b6ad26 {

        }

       }

      }

    +|/* ppx/ppx_css/test/test_laziness/test_laziness_dont_hash_and_prefixes/test_laziness_dont_hash_and_prefixes.ml */

    +|@layer test{
    +| @layer nested-layer{
    +|  *.b_hash_16e3b6ad26 {
    +|   grid-template-columns:1fr
    +|  }
    +|
    +| }
    +|
    +|
    +|}
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_dont_hash_and_prefixes/test_laziness_dont_hash_and_prefixes.ml */
    +|
    +|@layer test{
    +| @layer nested-layer{
    +|  *.a_hash_16e3b6ad26 {
    +|   display:flex
    +|  }
    +|
    +| }
    +|
    +|
    +|}

      /* ppx/ppx_css/test/test_laziness/test_laziness_dont_hash_and_prefixes/test_laziness_dont_hash_and_prefixes.ml */

      @layer test{
       @layer nested-layer{
        *.c {
         grid-template-rows:1fr;
         *.d_hash_16e3b6ad26 {

         }
         ;
         *.e_hash_16e3b6ad26 {

         }
         ;
         *.d_hash_16e3b6ad26+*.f_hash_16e3b6ad26 {
    -72,29 +96,44
       *.b_hash_16e3b6ad26 {

       }
       ;
       *.c {

       }

      }

      /* ppx/ppx_css/test/test_laziness/test_laziness_dont_hash_and_prefixes/test_laziness_dont_hash_and_prefixes.ml */

      body div {
       display:flex
      }

    +|/* ppx/ppx_css/test/test_laziness/test_laziness_dont_hash_and_prefixes/test_laziness_dont_hash_and_prefixes.ml */

    -|
    -|
    +|div>*.a_hash_16e3b6ad26 {
    +| pointer:cursor
    +|}
    +|
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_dont_hash_and_prefixes/test_laziness_dont_hash_and_prefixes.ml */
    +|
    +|*.a_hash_16e3b6ad26 *.b_hash_16e3b6ad26 {
    +| display:inline-flex
    +|}
    +|
    +|/* ppx/ppx_css/test/test_laziness/test_laziness_dont_hash_and_prefixes/test_laziness_dont_hash_and_prefixes.ml */
    +|
    +|*.b_hash_16e3b6ad26 {
    +| flex-direction:column
    +|}

      /* ppx/ppx_css/test/test_laziness/test_laziness_dont_hash_and_prefixes/test_laziness_dont_hash_and_prefixes.ml */

      *.c {
       display:table;
       *&.d_hash_16e3b6ad26 {
        text-align:center
       }

      }
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
  [%expect
    {xxx|
    -102,32 +102,45
       }

      }

      /* ppx/ppx_css/test/test_laziness/test_laziness_dont_hash_and_prefixes/test_laziness_dont_hash_and_prefixes.ml */

      body div {
       display:flex
      }

      /* ppx/ppx_css/test/test_laziness/test_laziness_dont_hash_and_prefixes/test_laziness_dont_hash_and_prefixes.ml */

      div>*.a_hash_16e3b6ad26 {
       pointer:cursor
      }

    +|/* ppx/ppx_css/test/test_laziness/test_laziness_dont_hash_and_prefixes/test_laziness_dont_hash_and_prefixes.ml */
    +|
    +|*.d_hash_16e3b6ad26 {
    +| color:green;
    +| *&:hover {
    +|  outline:blue
    +| }
    +| ;
    +| *.e_hash_16e3b6ad26 {
    +|  font-size:14px
    +| }
    +|
    +|}

      /* ppx/ppx_css/test/test_laziness/test_laziness_dont_hash_and_prefixes/test_laziness_dont_hash_and_prefixes.ml */

      *.a_hash_16e3b6ad26 *.b_hash_16e3b6ad26 {
       display:inline-flex
      }

      /* ppx/ppx_css/test/test_laziness/test_laziness_dont_hash_and_prefixes/test_laziness_dont_hash_and_prefixes.ml */

      *.b_hash_16e3b6ad26 {
       flex-direction:column
      }

      /* ppx/ppx_css/test/test_laziness/test_laziness_dont_hash_and_prefixes/test_laziness_dont_hash_and_prefixes.ml */

      *.c {
    |xxx}]
;;
