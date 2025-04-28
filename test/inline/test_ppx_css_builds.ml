module Normal =
  [%css
  stylesheet
    {|
      #an_id {
      }
      .a_class {
      }
    |}]

module With_variables =
  [%css
  stylesheet
    {|
      :root {
        --bg-color: red;
      }

      .a_class #an_id {
        color: var(--fg-color);
      }
    |}]

module Clashing_id_and_class =
  [%css
  stylesheet
    {|
      .a #a {
      }
    |}]

module Variable_clashes_called_set =
  [%css
  stylesheet
    {|
      .foo {
        color: var(--set);
        width: var(--width);
      }
    |}]

module _ = Normal
module _ = With_variables
module _ = Clashing_id_and_class
module _ = Variable_clashes_called_set
