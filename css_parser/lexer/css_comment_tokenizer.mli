open! Core
open Css_parser_common

val tokenize : Lex_buffer.t -> (Token.t * Location.t) option
