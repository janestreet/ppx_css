open! Core
open Css_parser_common
module Sedlexing = Lex_buffer

let non_printable =
  [%sedlex.regexp? '\x00' .. '\x08' | '\x0B' | '\x0E' .. '\x1F' | '\x7F']
;;

let invalid_url_char_minus_escape = [%sedlex.regexp? non_printable | '"' | '\'' | '(']
let css_newline_single_char = [%sedlex.regexp? '\n' | '\r' | "\u{000C}"]
let css_newline = [%sedlex.regexp? "\r\n" | css_newline_single_char]
let css_whitespace = [%sedlex.regexp? css_newline | '\t' | " "]
let css_whitespace_single_char = [%sedlex.regexp? css_newline_single_char | '\t' | " "]
let ws = [%sedlex.regexp? Star css_whitespace]
let start_of_invalid_escape_sequence = [%sedlex.regexp? '\\', css_newline]

let invalid_url_char =
  [%sedlex.regexp? invalid_url_char_minus_escape | start_of_invalid_escape_sequence]
;;

let hex_digit = [%sedlex.regexp? '0' .. '9' | 'a' .. 'f' | 'A' .. 'F']
let up_to_6_hex_digits = [%sedlex.regexp? Rep (hex_digit, 1 .. 6)]

let not_newline_or_hex_digit =
  [%sedlex.regexp? Compl (css_newline_single_char | hex_digit)]
;;

let escape =
  [%sedlex.regexp?
    '\\', (not_newline_or_hex_digit | up_to_6_hex_digits, Opt css_whitespace)]
;;

let valid_url_char =
  [%sedlex.regexp?
    Sub (Compl invalid_url_char_minus_escape, (css_whitespace_single_char | ')')) | escape]
;;

let invalid_url_char_no_whitespace = [%sedlex.regexp? invalid_url_char | valid_url_char]
let invalid_url = [%sedlex.regexp? Star (invalid_url_char, ws), ')']
let valid_url = [%sedlex.regexp? Star valid_url_char, ws, ')']
let quot = [%sedlex.regexp? Chars {|'"|}]
let bad_url_leading_char = [%sedlex.regexp? Sub (Compl quot, ')')]
let bad_url_matcher = [%sedlex.regexp? bad_url_leading_char, Compl ')' | '\\', ')']
let url = [%sedlex.regexp? ('u' | 'U'), ('r' | 'R'), ('l' | 'L'), '(', ws]

let tokenize buf =
  let open Token in
  match%sedlex buf with
  | url ->
    let start_pos, _ = Lex_buffer.lexing_positions buf in
    let name = Lex_buffer.utf8_between buf ~start:0 ~end_:3 in
    let%map.Option token =
      match%sedlex buf with
      | eof ->
        let _, end_pos = Lex_buffer.lexing_positions buf
        and message = [%string "Unterminated URL at EOF"] in
        raise (Errors.Lexing_error { start_pos; end_pos; message })
      (* Lookahead to see if this should be a function token or a URL token *)
      | quot ->
        Lex_buffer.rollback buf;
        Some (FUNCTION [ Ident_like.String name ])
      | valid_url -> Some (URL (Lex_buffer.utf8 ~end_:1 buf))
      | Star bad_url_matcher, ')' ->
        let token = BAD_URL (Lex_buffer.utf8 ~end_:1 buf) in
        let _, end_pos = Lex_buffer.lexing_positions buf
        and message = [%string "Bad URL detected with value %{to_string token}"] in
        raise (Errors.Lexing_error { start_pos; end_pos; message })
      | any ->
        let token =
          STRING { String_token.value = Lex_buffer.utf8 buf; quote_type = Single }
        in
        let _, end_pos = Lex_buffer.lexing_positions buf
        and message = [%string "Found invalid char in URL: %{to_string token}"] in
        raise (Errors.Lexing_error { start_pos; end_pos; message })
      | _ -> assert false
    in
    let _, end_pos = Lex_buffer.lexing_positions buf in
    let loc = Lex_buffer.make_loc start_pos end_pos in
    token, loc
  | _ -> None
;;
