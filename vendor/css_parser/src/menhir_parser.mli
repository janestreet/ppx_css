(* The type of tokens. *)

type token =
  | WHITESPACE
  | URI of string
  | UNICODE_RANGE of string
  | STRING of string
  | SEMI_COLON
  | RIGHT_PAREN
  | RIGHT_BRACKET
  | RIGHT_BRACE
  | PERCENTAGE
  | OPERATOR of string
  | NUMBER of string
  | NESTED_AT_RULE of string
  | LEFT_PAREN
  | LEFT_BRACKET
  | LEFT_BRACE
  | IMPORTANT
  | IDENT of string
  | HASH of string
  | FUNCTION of string
  | FLOAT_DIMENSION of (string * string * Types.dimension)
  | EOF
  | DOT
  | DIMENSION of (string * string)
  | DELIM of string
  | COLON
  | AT_RULE_WITHOUT_BODY of string
  | AT_RULE of string

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val stylesheet : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Types.Stylesheet.t

val declaration_list
  :  (Lexing.lexbuf -> token)
  -> Lexing.lexbuf
  -> Types.Declaration_list.t
