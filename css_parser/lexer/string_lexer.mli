open! Core
open Css_parser_common

val get_double_quote_string : Lex_buffer.t -> (Token.t * Location.t) option
val get_single_quote_string : Lex_buffer.t -> (Token.t * Location.t) option
