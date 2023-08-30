(** This file is a small test suite to check for successful compilation of ppx_css. *)

module Normal :
  module type of [%css stylesheet {|
#an_id {}
.a_class {}
                      |}]

module With_variables :
  module type of
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

module Clashing_id_and_class :
  module type of [%css stylesheet {|
.a #a {}
                      |}]

module Variable_clashes_called_set :
  module type of
    [%css
    stylesheet
      {|
.foo {
  color: var(--set);
  width: var(--width);
}
                      |}]
