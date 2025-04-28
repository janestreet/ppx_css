open! Core
open Css_parser_common
module Errors = Errors

val get_next_token : Lex_buffer.t -> Token.t * Location.t
val of_utf8_string : ?filename:string -> ?pos:Lexing.position -> string -> Lex_buffer.t

val get_tokens_with_positions
  :  ?filename:string
  -> ?pos:Lexing.position
  -> string
  -> (Token.t * Location.t) list

(** [rsplit_on_hash] will split a string like `EXPR#Foo` into [Some ("EXPR", "Foo")].

    [rsplit_on_hash] will correctly ignore escaped `#`'s in situations like:

    `EXPR "#"`. *)
val rsplit_on_hash : string -> (string * string) option
