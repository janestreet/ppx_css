open! Core
open Css_parser_common

(* Tokenizes numbers. This function is located in the [Ident] module because [number]
   followed by [ident] is considered a dimension token, so the number tokenizer needs 
   access to the [ident] regex
*)
val tokenize_number : Lex_buffer.t -> (Token.t * Location.t) option

(* Tokenizes anything that is [ident-like] such as at-keyword, function, hash *)
val tokenize : Lex_buffer.t -> (Token.t * Location.t) option
