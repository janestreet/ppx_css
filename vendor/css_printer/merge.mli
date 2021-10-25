type t

val empty
  : t

val add_css
  : t -> Css_parser.Types.Stylesheet.t -> t

val extract_css
  : t -> Css_parser.Types.Stylesheet.t
