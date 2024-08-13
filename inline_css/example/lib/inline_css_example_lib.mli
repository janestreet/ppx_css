module Style :
    module type of
      [%css
      stylesheet
        {|
          .foo #foo #bar {
            background: 5;
            red: blue;
          }
          |}]

val app : string
