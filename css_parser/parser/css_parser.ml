open! Core
module Lexer = Css_parser_lexer
module Recoverable_error = Recoverable_error
module Parsing_config = Parsing_config
include Types
module Traverse = Traverse

let get_filename_and_position (pos : Css_parser_common.Position.t option) =
  match pos with
  | None -> None, None
  | Some ({ pos_fname = filename; _ } as pos) -> Some filename, Some pos
;;

let parse_stylesheet ?pos ~parsing_config (css : string) =
  let filename, pos = get_filename_and_position pos in
  let first_pass =
    Parser.stylesheet ~parsing_config (Lexer.get_tokens_with_positions ?pos ?filename css)
  in
  let second_pass =
    Printer.stylesheet_to_string first_pass
    |> Lexer.get_tokens_with_positions ?pos ?filename
    |> Parser.stylesheet ~parsing_config
  in
  match Stylesheet.equal first_pass second_pass with
  | true -> first_pass
  | false ->
    let stylesheet_location = Tuple2.get2 first_pass in
    Parse_queue.raise
      ~loc:stylesheet_location
      "Error while parsing stylesheet. Unable to assert roundtripability of stylesheet. \
       This is an error in the CSS parser, please contact the maintainers of the parser."
;;

let stylesheet_to_string parsed = Printer.stylesheet_to_string parsed

let parse_style_block_contents ?pos ~parsing_config (css : string) =
  let filename, pos = get_filename_and_position pos in
  let first_pass =
    Parser.style_block_contents
      ~parsing_config
      (Lexer.get_tokens_with_positions ?pos ?filename css)
  in
  let second_pass =
    Printer.style_block_contents_to_string first_pass
    |> Lexer.get_tokens_with_positions ?pos
    |> Parser.style_block_contents ~parsing_config
  in
  match Style_block.equal first_pass second_pass with
  | true -> first_pass
  | false ->
    let style_block_contents_location = Tuple2.get2 first_pass in
    Parse_queue.raise
      ~loc:style_block_contents_location
      "Error while parsing style block contents. Unable to assert roundtripability of \
       style block contents. This is an error in the CSS parser, please contact the \
       maintainers of the parser."
;;

let style_block_contents_to_string = Printer.style_block_contents_to_string

let parse_for_apply_style ?pos css =
  let filename, pos = get_filename_and_position pos in
  let tokens = Lexer.get_tokens_with_positions ?pos ?filename css in
  Parser.for_apply_style tokens
;;

let for_apply_style_to_string = Printer.for_apply_style_to_string

module For_testing = struct
  module Parse_queue = Parse_queue

  let get_tokens_with_positions =
    Lexer.get_tokens_with_positions ~filename:"<n/a>" ?pos:None
  ;;
end
