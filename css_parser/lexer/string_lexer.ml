open! Core
open Css_parser_common
module Sedlexing = Lex_buffer

let css_newline_single_char = [%sedlex.regexp? '\n' | '\r' | "\u{000C}"]
let css_newline = [%sedlex.regexp? "\r\n" | css_newline_single_char]
let css_whitespace = [%sedlex.regexp? css_newline | '\t' | " "]
let up_to_6_hex_digits = [%sedlex.regexp? Rep (hex_digit, 1 .. 6)]

let not_newline_or_hex_digit =
  [%sedlex.regexp? Compl (css_newline_single_char | hex_digit)]
;;

let escape =
  [%sedlex.regexp?
    '\\', (not_newline_or_hex_digit | up_to_6_hex_digits, Opt css_whitespace)]
;;

(* Disallowing literal '\' because just a '\' and then EOF is considered a parse error. We 
   will allow for valid escape characters by joining on [escape] *)
let disallowed_chars_in_string = [%sedlex.regexp? css_newline_single_char | '\\']

let allowed_double_quote_chars =
  [%sedlex.regexp? Compl ('"' | disallowed_chars_in_string)]
;;

let allowed_single_quote_chars =
  [%sedlex.regexp? Compl ('\'' | disallowed_chars_in_string)]
;;

let string_double_quote_body = [%sedlex.regexp? allowed_double_quote_chars | escape]
let string_single_quote_body = [%sedlex.regexp? allowed_single_quote_chars | escape]

let get_double_quote_string buf =
  (* [string_double_quote_body] should catch any character that is not double-quote, \n, 
     or EOF  *)
  let open Token in
  let%map.Option token =
    match%sedlex buf with
    | '"', Star string_double_quote_body, '"' ->
      Some
        (STRING
           { String_token.value = Lex_buffer.utf8 ~start:1 ~end_:1 buf
           ; quote_type = Double
           })
    | '"', Star (Compl '"' | {|\"|}), css_newline ->
      let token =
        STRING
          { String_token.value = Lex_buffer.utf8 ~start:1 ~end_:1 buf
          ; quote_type = Double
          }
      in
      let start_pos, end_pos = Lex_buffer.lexing_positions buf
      and message =
        [%string "Unterminated string before newline character %{to_string token}"]
      in
      raise (Errors.Lexing_error { start_pos; end_pos; message })
    | '"', Star (Compl '"' | {|\"|}), eof ->
      let token =
        STRING { String_token.value = Lex_buffer.utf8 ~start:1 buf; quote_type = Double }
      in
      let start_pos, end_pos = Lex_buffer.lexing_positions buf
      and message = [%string "Unterminated string at EOF %{token#Token}"] in
      raise (Errors.Lexing_error { start_pos; end_pos; message })
    | '"', any ->
      let token =
        STRING { String_token.value = Lex_buffer.utf8 buf; quote_type = Double }
      in
      let start_pos, end_pos = Lex_buffer.lexing_positions buf in
      let message =
        [%string
          "Error while attempting to tokenize string. Please contact the maintainers of \
           the CSS parser. Last char: %{to_string token} "]
      in
      raise (Errors.Unknown_error { start_pos; end_pos; message })
    | _ -> None
  in
  Lex_buffer.with_loc buf ~f:(fun () -> token)
;;

let get_single_quote_string buf =
  let open Token in
  let%map.Option token =
    match%sedlex buf with
    (* [string_single_quote_body] should catch any character that is not ['], \n, or EOF *)
    | '\'', Star string_single_quote_body, '\'' ->
      Some
        (STRING
           { String_token.value = Lex_buffer.utf8 ~start:1 ~end_:1 buf
           ; quote_type = Single
           })
    | '\'', Star (Compl '\'' | {|\'|}), css_newline ->
      let token =
        STRING
          { String_token.value = Lex_buffer.utf8 ~start:1 ~end_:1 buf
          ; quote_type = Single
          }
      in
      let start_pos, end_pos = Lex_buffer.lexing_positions buf
      and message =
        [%string "Unterminated string before newline character %{to_string token}"]
      in
      raise (Errors.Lexing_error { start_pos; end_pos; message })
    | '\'', Star (Compl '\'' | {|\'|}), eof ->
      let token =
        STRING { String_token.value = Lex_buffer.utf8 ~start:1 buf; quote_type = Single }
      in
      let start_pos, end_pos = Lex_buffer.lexing_positions buf
      and message = [%string "Unterminated string at EOF %{token#Token}"] in
      raise (Errors.Lexing_error { start_pos; end_pos; message })
    | '\'', any ->
      let token =
        STRING { String_token.value = Lex_buffer.utf8 buf; quote_type = Single }
      in
      let start_pos, end_pos = Lex_buffer.lexing_positions buf
      and message =
        [%string
          "Error while attempting to tokenize string. Please contact the maintainers of \
           the CSS parser. Last char: %{to_string token} "]
      in
      raise (Errors.Unknown_error { start_pos; end_pos; message })
    | _ -> None
  in
  Lex_buffer.with_loc buf ~f:(fun () -> token)
;;
