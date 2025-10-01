open! Core
module Lexer = Css_parser_lexer
module Recoverable_error = Recoverable_error
module Partial_parsing_behavior = Partial_parsing_behavior
module Parsing_config = Parsing_config
include module type of Types
module Traverse = Traverse

val parse_stylesheet
  :  ?pos:Css_parser_common.Position.t
  -> parsing_config:Parsing_config.t
  -> string
  -> Types.Stylesheet.t

val stylesheet_to_string : Types.Stylesheet.t -> string

val parse_style_block_contents
  :  ?pos:Css_parser_common.Position.t
  -> parsing_config:Parsing_config.t
  -> string
  -> Types.Style_block.t

val style_block_contents_to_string : Types.Style_block.t -> string

val parse_for_apply_style
  :  ?pos:Css_parser_common.Position.t
  -> string
  -> Types.For_apply_style.t

val for_apply_style_to_string : Types.For_apply_style.t -> string

module For_testing : sig
  module Parse_queue = Parse_queue

  val get_tokens_with_positions : string -> (Css_parser_common.Token.t * Location.t) list
end
