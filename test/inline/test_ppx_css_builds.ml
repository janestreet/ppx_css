module Normal = [%css stylesheet {|
#an_id {}
.a_class {}
                      |}]

module With_variables =
  [%css
    stylesheet
      {|
:root {
  --bg-color: red;
}

.a_class #an_id {
   color: var(--fg-color)
}
                      |}]

module Clashing_id_and_class = [%css stylesheet {|
.a #a {}
                      |}]
