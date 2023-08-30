module Foo =
[%css
stylesheet
  {|
.class1 { color: var(--background-color); }

.class2 { background-color: var(--another-color); }

|}]

module _ = Foo
