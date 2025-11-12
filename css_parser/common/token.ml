open! Core
open Types

module Sigil = struct
  type t =
    | Percent
    | Hash
  [@@deriving equal, compare, variants, sexp_of, typed_variants, to_string]

  let to_sigil_string = function
    | Percent -> "%"
    | Hash -> "#"
  ;;
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

let to_string = function
  | EOF -> "EOF"
  | AT_KEYWORD keyword -> {%string|AT_KEYWORD(%{keyword#Ident_like})|}
  | WHITESPACE _ -> "WHITESPACE"
  | SEMICOLON -> "SEMICOLON"
  | HASH (hash, hash_flag) ->
    {%string|HASH(%{hash#Ident_like},FLAG(%{hash_flag#Hash_flag}))|}
  | IDENT ident -> {%string|IDENT(%{ident#Ident_like})|}
  | RIGHT_PAREN -> "RIGHT_PAREN"
  | LEFT_PAREN -> "LEFT_PAREN"
  | RIGHT_BRACKET -> "RIGHT_BRACKET"
  | LEFT_BRACKET -> "LEFT_BRACKET"
  | RIGHT_BRACE -> "RIGHT_BRACE"
  | LEFT_BRACE -> "LEFT_BRACE"
  | COMMA -> "COMMA"
  | COLON -> "COLON"
  | BAD_URL url -> {%string|BAD_URL(%{url})|}
  | BAD_STRING str -> {%string|BAD_STRING(%{str})|}
  | URL url -> {%string|URL(%{url})|}
  | STRING str -> {%string|STRING(%{str#String_token})|}
  | OCAML_CODE (code, sigil) -> {%string|OCAML_CODE(%{code}, %{sigil#Sigil})|}
  | COMMENT comment -> {%string|COMMENT(%{comment})|}
  | DELIM code_point -> {%string|DELIM(%{code_point})|}
  | CDO -> "CDO"
  | CDC -> "CDC"
  | PERCENTAGE (value, numeric_value_type, exponent_value) ->
    let exponent =
      Core.Option.value_map
        ~default:""
        exponent_value
        ~f:(fun { Exponent.value; capitalized } ->
          [%string ", EXPONENT(%{value}, is_capitalized=%{capitalized#Bool})"])
    in
    {%string|PERCENTAGE((%{value}%{exponent}), TYPE(%{numeric_value_type#Numeric_value}))|}
  | NUMBER (value, numeric_value_type, exponent) ->
    let exponent =
      Core.Option.value_map
        ~default:""
        exponent
        ~f:(fun { Exponent.value; capitalized } ->
          [%string ", EXPONENT(%{value}, is_capitalized=%{capitalized#Bool})"])
    in
    {%string|NUMBER(%{value}, TYPE(%{numeric_value_type#Numeric_value})%{exponent})|}
  | DIMENSION (value, numeric_value_type, exponent_value, dimension_unit) ->
    let exponent =
      Core.Option.value_map
        ~default:""
        exponent_value
        ~f:(fun { Exponent.value; capitalized } ->
          [%string ", EXPONENT(%{value}, is_capitalized=%{capitalized#Bool})"])
    in
    {%string|DIMENSION((%{value}%{exponent}), TYPE(%{numeric_value_type#Numeric_value}), UNIT(%{dimension_unit#Ident_like}))|}
  | FUNCTION fn -> {%string|FUNCTION(%{fn#Ident_like})|}
;;
