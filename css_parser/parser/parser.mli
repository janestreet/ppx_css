open! Core
module Token = Css_parser_common.Token

val stylesheet
  :  parsing_config:Parsing_config.t
  -> (Token.t * Location.t) list
  -> Types.Stylesheet.t

val style_block_contents
  :  parsing_config:Parsing_config.t
  -> (Token.t * Location.t) list
  -> Types.Style_block.t

val for_apply_style : (Token.t * Location.t) list -> Types.For_apply_style.t
