open! Core
open Types

module Sigil : sig
  type t =
    | Percent
    | Hash
  [@@deriving equal, compare, variants, sexp_of, typed_variants]

  val to_sigil_string : t -> string
end

type t =
  | WHITESPACE of string
  | URL of string
  | STRING of String_token.t
  | SEMICOLON
  | RIGHT_PAREN
  | RIGHT_BRACKET
  | RIGHT_BRACE
  | PERCENTAGE of (string * Numeric_value.t * Exponent.t option)
  | OCAML_CODE of (string * Sigil.t)
  | NUMBER of (string * Numeric_value.t * Exponent.t option)
  | LEFT_PAREN
  | LEFT_BRACKET
  | LEFT_BRACE
  | IDENT of Ident_like.t
  | HASH of (Ident_like.t * Hash_flag.t)
  | FUNCTION of Ident_like.t
    (* Function tokens do not need to be ident-like due to the fact that they're
       terminated by a ( *)
  | EOF
  | DIMENSION of (string * Numeric_value.t * Exponent.t option * Ident_like.t)
  | DELIM of string
  | COMMENT of string
  | COMMA
  | COLON
  | CDO
  | CDC
  | BAD_URL of string
  | BAD_STRING of string
  | AT_KEYWORD of Ident_like.t
[@@deriving equal, compare, variants, sexp_of, typed_variants]

val to_string : t -> string
