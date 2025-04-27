module Style =
  [%css
  stylesheet
    {|
      .a {
        display: flex;
      }
    |}]

module _ = Style
