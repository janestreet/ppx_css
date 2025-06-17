open! Core
open Css_parser_common
module Sedlexing = Lex_buffer

let css_newline_single_char = [%sedlex.regexp? '\n' | '\r' | "\u{000C}"]
let css_newline = [%sedlex.regexp? "\r\n" | css_newline_single_char]
let css_whitespace = [%sedlex.regexp? css_newline | '\t' | " "]
let css_whitespace_single_char = [%sedlex.regexp? css_newline_single_char | '\t' | " "]
let ascii = [%sedlex.regexp? Latin1 '\000' .. '\177']
let non_ascii = [%sedlex.regexp? Compl ascii]
let ident_start_code_point = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z' | '_' | non_ascii]
let ident_code_point = [%sedlex.regexp? ident_start_code_point | '0' .. '9' | '-']
let start_of_valid_escape_sequence = [%sedlex.regexp? '\\', Compl css_newline_single_char]
let start_of_invalid_escape_sequence = [%sedlex.regexp? '\\', css_newline]
let hex_digit = [%sedlex.regexp? '0' .. '9' | 'a' .. 'f' | 'A' .. 'F']
let up_to_6_hex_digits = [%sedlex.regexp? Rep (hex_digit, 1 .. 6)]

let not_newline_or_hex_digit =
  [%sedlex.regexp? Compl (css_newline_single_char | hex_digit)]
;;

let escape = [%sedlex.regexp? '\\', (not_newline_or_hex_digit | up_to_6_hex_digits)]
let hyphen_valid = [%sedlex.regexp? '-', (ident_start_code_point | '-' | escape)]
let non_hex_escape = [%sedlex.regexp? '\\', not_newline_or_hex_digit]

let start_of_ident_sequence =
  [%sedlex.regexp? hyphen_valid | ident_start_code_point | escape]
;;

let start_of_ident_sequence_no_hex =
  [%sedlex.regexp? hyphen_valid | ident_start_code_point | non_hex_escape]
;;

let ident_trailing_letter_no_hex = [%sedlex.regexp? ident_code_point | non_hex_escape]
let hex_escape = [%sedlex.regexp? '\\', up_to_6_hex_digits]
let ident_trailing_letter = [%sedlex.regexp? ident_code_point | escape]
let ident = [%sedlex.regexp? start_of_ident_sequence, Star ident_trailing_letter]
let hash_id = [%sedlex.regexp? '#', ident]
let hash_unrestricted = [%sedlex.regexp? '#', Plus ident_trailing_letter]
let starts_with_number = [%sedlex.regexp? Opt ('+' | '-'), Opt '.', '0' .. '9']
let int_digit = [%sedlex.regexp? '0' .. '9']
let valid_int = [%sedlex.regexp? Opt ('+' | '-'), Plus int_digit]

(* Tail-call recursion doesn't work properly with sedlex, for some reason
   it returns an empty lexeme if done that way. That should be fine unless
   for some reason the user is using a billion escape sequences in their
   selectors, which is doubtful because unicode escape sequences are rarely used
*)
let rec parse_trailing_ident buf =
  match%sedlex buf with
  (* Have to match one or more times. 0 or more will infinite-loop *)
  | Plus ident_trailing_letter_no_hex ->
    let value = Ident_like.String (Lex_buffer.utf8 buf) in
    value :: parse_trailing_ident buf
  | hex_escape ->
    let value = Ident_like.Hex_escape (Lex_buffer.utf8 buf) in
    value :: parse_trailing_ident buf
  | hex_escape, css_whitespace ->
    let value = Ident_like.Hex_escape (Lex_buffer.utf8 ~end_:1 buf) in
    value :: parse_trailing_ident buf
  | _ -> []
;;

(* Rollback doesn't work properly if the match arm doesn't match anything which 
   is why we have to return DELIM @ and DELIM # here

   See above for why we are not using tail-call recursion
*)
let parse_ident buf =
  match%sedlex buf with
  | start_of_ident_sequence_no_hex, Star ident_trailing_letter_no_hex ->
    let value = Ident_like.String (Lex_buffer.utf8 buf) in
    Some (value :: parse_trailing_ident buf)
  | hex_escape ->
    let value = Ident_like.Hex_escape (Lex_buffer.utf8 buf) in
    Some (value :: parse_trailing_ident buf)
  | hex_escape, css_whitespace ->
    let value = Ident_like.Hex_escape (Lex_buffer.utf8 ~end_:1 buf) in
    Some (value :: parse_trailing_ident buf)
  | _ -> None
;;

let exponent_value buf =
  match%sedlex buf with
  | 'e', valid_int ->
    Some { Exponent.value = Lex_buffer.utf8 ~start:1 buf; capitalized = false }
  | 'E', valid_int ->
    Some { Exponent.value = Lex_buffer.utf8 ~start:1 buf; capitalized = true }
  | _ -> None
;;

let tokenize_number buf =
  let open Token in
  let%map.Option number, numeric_value =
    match%sedlex buf with
    | valid_int ->
      let int_value = Lex_buffer.utf8 buf in
      Some (int_value, Numeric_value.Integer)
    | Opt ('+' | '-'), Star int_digit, '.', Plus int_digit ->
      let float_value = Lex_buffer.utf8 buf in
      Some (float_value, Numeric_value.Float)
    | _ -> None
  in
  (* This seems a bit unintuitive, but this works because we call [lexing_positions] 
     _before_ the second match occurs. This means that we retrieve the start position
     of the previous match, which is [number]
  *)
  Lex_buffer.with_loc buf ~f:(fun () ->
    let exponent_value = exponent_value buf in
    let number = number, numeric_value, exponent_value in
    match%sedlex buf with
    | "%{" ->
      Lex_buffer.rollback buf;
      NUMBER number
    | '%' -> PERCENTAGE number
    | _ ->
      (match parse_ident buf with
       | Some ident ->
         let base, kind, exponent = number in
         DIMENSION (base, kind, exponent, ident)
       | None -> NUMBER number))
;;

let next_token_open_paren buf =
  match%sedlex buf with
  | '(' -> true
  | _ -> false
;;

let tokenize buf =
  let open Token in
  match%sedlex buf with
  | '@' ->
    Some
      (Lex_buffer.with_loc buf ~f:(fun () ->
         match parse_ident buf with
         | None -> DELIM "@"
         | Some ident -> AT_KEYWORD ident))
  | '#' ->
    Some
      (Lex_buffer.with_loc buf ~f:(fun () ->
         match parse_ident buf with
         | None ->
           (match parse_trailing_ident buf with
            | [] -> DELIM "#"
            | unrestricted_hash -> HASH (unrestricted_hash, Hash_flag.Unrestricted))
         | Some ident -> HASH (ident, Hash_flag.Id)))
  | _ ->
    (* We can't use [with_loc] as we're using the previous end
       position as the start position due to the fact that we haven't
       actually matched on a lexeme yet.
    *)
    let _, start_pos = Lex_buffer.lexing_positions buf in
    let%map.Option ident = parse_ident buf in
    let token =
      match next_token_open_paren buf with
      | true -> FUNCTION ident
      | false -> IDENT ident
    in
    let _, end_pos = Lex_buffer.lexing_positions buf in
    token, Lex_buffer.make_loc start_pos end_pos
;;
