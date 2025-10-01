open! Core
open Css_parser_common
module Sedlexing = Lex_buffer

(* Keeping this here as a reference. Unfortunately sedlex doesn't allow for non-greedy
   matching. Due to this, we have to non-tail-recursively construct the string. We can
   prevent the stack from growing too much by stopping each call on [*] and parsing 
   those individually
*)
let comment_doesn't_work = [%sedlex.regexp? '/', '*', Opt (Star any), '*', '/']
let not_asterisk = [%sedlex.regexp? Compl "*"]

let rec consume_comment_body buf =
  match%sedlex buf with
  | "*/" -> ""
  | "" ->
    let start_pos, end_pos = Lex_buffer.lexing_positions buf
    and message = "Unterminated CSS comment" in
    raise (Errors.Lexing_error { start_pos; end_pos; message })
  | Star not_asterisk ->
    let str = Lex_buffer.utf8 buf in
    str ^ consume_comment_body buf
  | "*" ->
    (* NOTE: We have a separate branch for [*] and [Not *] instead of [any] so that:

       ```
         /* comment 1 */ SOME_OTHER_CSS_VALUE /* comment 2 */
       ```

       parses into:

       ```
       COMMENT; SOME_OTHER_CSS_VALUE; COMMENT
       ```

       instead of:

       ```
       A_BIG_COMMENT_THAT_SUBSUMES_SOME_OTHER_CSS_VALUE
       ```
    *)
    let asterisk = Lex_buffer.utf8 buf in
    asterisk ^ consume_comment_body buf
  | _ -> assert false
;;

let tokenize buf =
  match%sedlex buf with
  | "/*" ->
    let token =
      Lex_buffer.with_loc buf ~f:(fun () -> Token.COMMENT (consume_comment_body buf))
    in
    Some token
  | _ -> None
;;
