open! Core
open Css_parser_common
module Sedlexing = Lex_buffer
module Errors = Errors

(* Keeping the below as a reference. The CSS spec says you need to preprocess the
   input by replacing all of these characters with \n, but that seems to mess with
   the source code positions. We're already matching on these in the lexer itself,
   so it shouldn't really matter
*)
(* Converts \r\n, \f, and \r to \n *)
let filtered_code_points_to_line_feed =
  [%sedlex.regexp? "\u{000D}\u{000A}" | "\u{000C}" | "\u{000D}"]
;;

(* Keeping the below two as a reference. The CSS spec says you need to preprocess the
   input by replacing this character with the null character, but it seems to mess with
   the source code positions. We don't really process this or the null character any 
   differently so it shouldn't really matter
*)
let filter_null_to_replacement = [%sedlex.regexp? "\u{0000}"]

let catchall_bug_in_css_parser ~(here : [%call_pos]) () =
  raise_s
    [%message
      "BUG in css parser. If you see this error message this is a bug in the CSS parser. \
       Please report this error message."
        (here : Source_code_position.t)]
;;

(* We replaced most of these above, but matching on them anyways *)
let css_newline_single_char = [%sedlex.regexp? '\n' | '\r' | "\u{000C}"]
let css_newline = [%sedlex.regexp? "\r\n" | css_newline_single_char]
let css_whitespace = [%sedlex.regexp? css_newline | '\t' | " "]
let ws = [%sedlex.regexp? Star css_whitespace]
let not_closing_brace = [%sedlex.regexp? Compl "}"]

let get_ocaml_code =
  let match_closing_brace_for_ocaml ~start_pos buf =
    match%sedlex buf with
    | Star not_closing_brace, "}" ->
      let code_string_part = Lex_buffer.utf8 buf in
      code_string_part
    | _ ->
      let end_pos, _ = Lex_buffer.lexing_positions buf in
      let message =
        [%string
          "Error while parsing interpolated OCaml code. Unable to find closing brace"]
      in
      raise (Errors.Lexing_error { start_pos; end_pos; message })
  in
  let f ~sigil ~start_pos buf =
    let left_braces = ref 1 in
    let right_braces = ref 0 in
    let potential_ocaml_code = ref "" in
    while !left_braces <> !right_braces do
      let ocaml_code_part = match_closing_brace_for_ocaml ~start_pos buf in
      potential_ocaml_code := !potential_ocaml_code ^ ocaml_code_part;
      let ocaml_code_to_tokenize =
        (* We still have to use the [ %{ ] sigil here because [ #{ ] isn't a standard 
           string interpolation sigil. We omit interpolation open + close in AST node 
           for [OCAML_CODE] anyways *)
        "%{" ^ !potential_ocaml_code
      in
      match Ocaml_tokenizer.tokens_with_loc ocaml_code_to_tokenize with
      | Error _ -> ()
      | Ok tokens ->
        (* This should be incrementing by 1 every loop, but calculating 
           the same way as [left_braces] is more consistent
        *)
        right_braces
        := Core.List.count tokens ~f:(function
             | Ocaml_common.Parser.RBRACE, _ -> true
             | _ -> false);
        left_braces
        := Core.List.count tokens ~f:(function
             | Ocaml_common.Parser.LBRACE, _ -> true
             | _ -> false)
    done;
    (* Need to remove the trailing right brace as it matches the omitted opening 
       interpolation block *)
    let ocaml_code = !potential_ocaml_code |> Core.String.chop_suffix_exn ~suffix:"}" in
    Token.OCAML_CODE (ocaml_code, sigil)
  in
  fun buf ->
    match%sedlex buf with
    | "%{" ->
      let token =
        Lex_buffer.with_loc' buf ~f:(fun ~start_pos ->
          f ~sigil:Token.Sigil.Percent ~start_pos buf)
      in
      Some token
    | "#{" ->
      let token =
        Lex_buffer.with_loc' buf ~f:(fun ~start_pos ->
          f ~sigil:Token.Sigil.Hash ~start_pos buf)
      in
      Some token
    | _ -> None
;;

let default_tokenizer buf =
  let open Token in
  let%map.Option token =
    match%sedlex buf with
    | eof -> Some EOF
    | ws -> Some (WHITESPACE (Lex_buffer.utf8 buf))
    | "*/" ->
      let start_pos, end_pos = Lex_buffer.lexing_positions buf
      and message = "Unmatched closing CSS comment" in
      raise (Errors.Lexing_error { start_pos; end_pos; message })
    | ';' -> Some SEMICOLON
    | ':' -> Some COLON
    | '}' -> Some RIGHT_BRACE
    | '{' -> Some LEFT_BRACE
    | ']' -> Some RIGHT_BRACKET
    | '[' -> Some LEFT_BRACKET
    | ')' -> Some RIGHT_PAREN
    | '(' -> Some LEFT_PAREN
    | ',' -> Some COMMA
    (* NOTE: HTML-like comments are surprisingly in the CSS spec! We support them here too.
     Spec: https://www.w3.org/TR/css-syntax-3/#parser-entry-points *)
    | "<!--" -> Some CDO
    | "-->" -> Some CDC
    | any -> Some (DELIM (Lex_buffer.utf8 buf))
    | _ -> None
  in
  Lex_buffer.with_loc buf ~f:(fun () -> token)
;;

(* This pattern allows us to choose the order in which the tokenizers are applied. Sedlex 
   does not really honor the ordering of the match arms within its ppx match statement, so
   this is a way around that.

   This also allows us to somewhat split the code into modules. Unfortunately due to how
   sedlex works, we have to redeclare some of the regexes, but the code is much more readable
   without us having to sift through 100 lines of regexes in a single file.

   Note that this pattern is slightly less efficient, as it creates multiple automata that 
   the lexer has to step through in order to find the matching token, but it should only be
   marginally less efficient.
*)
let matching_token buf =
  List.find_map
    ~f:(fun tokenizer -> tokenizer buf)
    [ Css_comment_tokenizer.tokenize
    ; get_ocaml_code
    ; Url.tokenize
    ; Ident.tokenize_number
    ; Ident.tokenize
      (* [Ident] contains [FUNCTION] token, which is more permissive than [url] and has 
       to go after [url] *)
    ; String_lexer.get_double_quote_string
    ; String_lexer.get_single_quote_string
    ; default_tokenizer
    ]
;;

let of_utf8_string = Sedlexing.of_utf8_string

let get_next_token buf =
  match matching_token buf with
  | Some token -> token
  | _ -> catchall_bug_in_css_parser ()
;;

let get_tokens_with_positions ?filename ?pos (css_string : string)
  : (Token.t * Location.t) list
  =
  let buffer = of_utf8_string ?filename ?pos css_string in
  let first_token = get_next_token buffer in
  let latest_token = ref (fst first_token) in
  let token_list = ref Reversed_list.[ first_token ] in
  while not (Token.is_eof !latest_token) do
    let token = get_next_token buffer in
    latest_token := fst token;
    token_list := token :: !token_list
  done;
  Reversed_list.rev !token_list
;;

let rsplit_on_hash = Ocaml_tokenizer.rsplit_on_hash
