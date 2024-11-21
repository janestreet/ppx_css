open! Core

(* This file constains unit tests for the stylesheet processing within traverse_css in
   ppx_css *)

module%test Process_stylesheet = struct
  open Ppx_css.For_testing.Traverse_css

  let test (s : string) =
    let stylesheet = Css_jane.Stylesheet.of_string s in
    let processed_stylesheet = split_layers stylesheet in
    Css_jane.Stylesheet.to_string_hum processed_stylesheet |> print_endline
  ;;

  let%expect_test "Splits layer [At_rule]s and maintains order" =
    test
      {|
        @layer test-layer {
          .inside-layer1 {

          }

          .inside-layer2 {

          }

          .inside-layer3 {

          }

          .inside-layer4 {

          }

          .inside-layer5 {
            .nested-layer-1 {
            }

          }

          .inside-layer5 + .a {

          }
        }


        @layer another-layer{
          .class2 {
          } 
          #id-inside {}
        }

    |};
    [%expect
      {|
      @layer test-layer{
       *.inside-layer1 {

       }

      }


      @layer test-layer{
       *.inside-layer2 {

       }

      }


      @layer test-layer{
       *.inside-layer3 {

       }

      }


      @layer test-layer{
       *.inside-layer4 {

       }

      }


      @layer test-layer{
       *.inside-layer5 {
        *.nested-layer-1 {

        }

       }

      }


      @layer test-layer{
       *.inside-layer5+*.a {

       }

      }


      @layer another-layer{
       *.class2 {

       }

      }


      @layer another-layer{
       *#id-inside {

       }

      }
      |}]
  ;;

  let%expect_test "Does not split nested [At_rule]s" =
    test
      {|

        @layer another-layer{
          @layer nested-layer {
          .class2 {
          } 

          #id-inside {}
          }

          .a {}

          .b {}
        }

    |};
    [%expect
      {|
      @layer another-layer{
       @layer nested-layer{
        *.class2 {

        }

       }


      }


      @layer another-layer{
       @layer nested-layer{
        *#id-inside {

        }

       }


      }


      @layer another-layer{
       *.a {

       }

      }


      @layer another-layer{
       *.b {

       }

      }
      |}]
  ;;

  let%expect_test "Does not split other [At_rule]s" =
    test
      {|
        @media print {
          .inside-layer1 {

          }

          .inside-layer2 {

          }

          .inside-layer3 {

          }

          .inside-layer4 {

          }

          .inside-layer5 {
            .nested-layer-1 {
            }

          }

          .inside-layer5 + .a {

          }
        }


        @layer another-layer{
          .class2 {
          } 
          #id-inside {}
        }

    |};
    [%expect
      {|
      @media print{
       *.inside-layer1 {

       }

       *.inside-layer2 {

       }

       *.inside-layer3 {

       }

       *.inside-layer4 {

       }

       *.inside-layer5 {
        *.nested-layer-1 {

        }

       }

       *.inside-layer5+*.a {

       }

      }


      @layer another-layer{
       *.class2 {

       }

      }


      @layer another-layer{
       *#id-inside {

       }

      }
      |}]
  ;;

  let%expect_test "Does not split [Style_rule]s" =
    test
      {|
        @layer test-layer {
          .inside-layer {

          }
        }

        .class {
          .inner_class {
          }
          
          .inner_class_2 {}
        }

        
        div {
        }

        .class #id {
        }

        .class .class2 {
        }

        .class .class {
          .class2 {
            #id-inside {}
            #inner-class{}
          } 
        }

    |};
    [%expect
      {|
      @layer test-layer{
       *.inside-layer {

       }

      }


      *.class {
       *.inner_class {

       }
       ;
       *.inner_class_2 {

       }

      }

      div {

      }

      *.class *#id {

      }

      *.class *.class2 {

      }

      *.class *.class {
       *.class2 {
        *#id-inside {

        }
        ;
        *#inner-class {

        }

       }

      }
      |}]
  ;;
end
