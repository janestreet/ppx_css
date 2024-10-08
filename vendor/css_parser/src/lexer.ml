(** CSS lexer.
  * Reference:
  * https://www.w3.org/TR/css-syntax-3/
  * https://github.com/yahoo/css-js/blob/master/src/l/css.3.l *)

module Sedlexing = Lex_buffer

(** Signals a lexing error at the provided source location.  *)
exception LexingError of (Lexing.position * string)

(** Signals a parsing error at the provided token and its start and end
 * locations. *)
exception ParseError of (Menhir_parser.token * Lexing.position * Lexing.position)

(** Signals a grammar error at the provided location. *)
exception GrammarError of (string * Location.t)

let position_to_string pos =
  Printf.sprintf
    "[%d,%d+%d]"
    pos.Lexing.pos_lnum
    pos.Lexing.pos_bol
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
;;

let location_to_string loc =
  Printf.sprintf
    "%s..%s"
    (position_to_string loc.Location.loc_start)
    (position_to_string loc.Location.loc_end)
;;

let dimension_to_string = function
  | Types.Length -> "length"
  | Angle -> "angle"
  | Time -> "time"
  | Frequency -> "frequency"
;;

let token_to_string = function
  | Menhir_parser.EOF -> "EOF"
  | LEFT_BRACE -> "{"
  | RIGHT_BRACE -> "}"
  | LEFT_PAREN -> "("
  | RIGHT_PAREN -> ")"
  | LEFT_BRACKET -> "["
  | RIGHT_BRACKET -> "]"
  | COLON -> ":"
  | DOT -> "."
  | AMPERSAND -> "&"
  (* Whitespaces are detected only in selectors, before ":", ".", and "#", to
   * disambiguate between "p :first-child" and "p:first-child", these
   * whitespaces are replaced with "*" *)
  | WHITESPACE -> "*"
  | SEMI_COLON -> ";"
  | PERCENTAGE -> "%"
  | IMPORTANT -> "!important"
  | IDENT s -> "IDENT(" ^ s ^ ")"
  | STRING s -> "STRING(" ^ s ^ ")"
  | URI s -> "URI(" ^ s ^ ")"
  | OPERATOR s -> "OPERATOR(" ^ s ^ ")"
  | DELIM s -> "DELIM(" ^ s ^ ")"
  | NESTED_AT_RULE s -> "NESTED_AT_RULE(" ^ s ^ ")"
  | AT_RULE_WITHOUT_BODY s -> "AT_RULE_WITHOUT_BODY(" ^ s ^ ")"
  | AT_RULE s -> "AT_RULE(" ^ s ^ ")"
  | FUNCTION s -> "FUNCTION(" ^ s ^ ")"
  | SELECTOR_FUNCTION s -> "SELECTOR_FUNCTION(" ^ s ^ ")"
  | HASH s -> "HASH(" ^ s ^ ")"
  | NUMBER s -> "NUMBER(" ^ s ^ ")"
  | UNICODE_RANGE s -> "UNICODE_RANGE(" ^ s ^ ")"
  | FLOAT_DIMENSION (n, s, d) ->
    "FLOAT_DIMENSION(" ^ n ^ ", " ^ s ^ ", " ^ dimension_to_string d ^ ")"
  | DIMENSION (n, d) -> "DIMENSION(" ^ n ^ ", " ^ d ^ ")"
;;

let () =
  Location.register_error_of_exn (function
    | LexingError (pos, msg) ->
      let loc = Lex_buffer.make_loc_and_fix pos pos in
      Some (Location.error ~loc msg)
    | ParseError (token, start_pos, end_pos) ->
      let loc = Lex_buffer.make_loc_and_fix start_pos end_pos in
      let msg =
        Printf.sprintf "Parse error while reading token '%s'" (token_to_string token)
      in
      Some (Location.error ~loc msg)
    | GrammarError (msg, loc) -> Some (Location.error ~loc msg)
    | _ -> None)
;;

(* Regexes *)
let newline = [%sedlex.regexp? '\n' | "\r\n" | '\r' | '\012']
let white_space = [%sedlex.regexp? " " | '\t' | newline]
let ws = [%sedlex.regexp? Star white_space]
let hex_digit = [%sedlex.regexp? '0' .. '9' | 'a' .. 'f' | 'A' .. 'F']
let digit = [%sedlex.regexp? '0' .. '9']
let non_ascii = [%sedlex.regexp? '\160' .. '\255']
let up_to_6_hex_digits = [%sedlex.regexp? Rep (hex_digit, 1 .. 6)]
let unicode = [%sedlex.regexp? '\\', up_to_6_hex_digits, Opt white_space]

let unicode_range =
  [%sedlex.regexp?
    Rep ((hex_digit | '?'), 1 .. 6) | up_to_6_hex_digits, '-', up_to_6_hex_digits]
;;

let escape = [%sedlex.regexp? unicode | '\\', Compl ('\r' | '\n' | '\012' | hex_digit)]
let ident_start = [%sedlex.regexp? '_' | 'a' .. 'z' | 'A' .. 'Z' | non_ascii | escape]

let ident_char =
  [%sedlex.regexp? '_' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | non_ascii | escape]
;;

let ident = [%sedlex.regexp? (Opt '-', ident_start | '-', '-'), Star ident_char]

let string_quote =
  [%sedlex.regexp?
    '"', Star (Compl ('\n' | '\r' | '\012' | '"') | '\\', newline | escape), '"']
;;

let string_apos =
  [%sedlex.regexp?
    '\'', Star (Compl ('\n' | '\r' | '\012' | '\'') | '\\', newline | escape), '\'']
;;

let string = [%sedlex.regexp? string_quote | string_apos]
let name = [%sedlex.regexp? Plus ident_char]

let number =
  [%sedlex.regexp?
    ( ( Opt ('+' | '-')
      , Plus digit
      , Opt ('.', Plus digit)
      , Opt (('e' | 'E'), ('+' | '-'), Plus digit) )
    | Opt ('+' | '-'), '.', Plus digit, Opt (('e' | 'E'), ('+' | '-'), Plus digit) )]
;;

let non_printable =
  [%sedlex.regexp? '\x00' .. '\x08' | '\x0B' | '\x0E' .. '\x1F' | '\x7F']
;;

let url_unquoted =
  [%sedlex.regexp? Star (Compl ('"' | '\'' | '(' | ')' | '\\' | non_printable) | escape)]
;;

let url = [%sedlex.regexp? url_unquoted | string]
let operator = [%sedlex.regexp? "~=" | "|=" | "^=" | "$=" | "*=" | "||"]
let at_rule = [%sedlex.regexp? "@", ident]
let at_rule_without_body = [%sedlex.regexp? "@", ("charset" | "import" | "namespace")]
let vendor_prefix = [%sedlex.regexp? "-webkit-" | "-moz-" | "-o-" | "-ms-"]

let nested_at_rule =
  [%sedlex.regexp?
    ( "@"
    , ( "document" | "keyframes"
      | vendor_prefix, "keyframes"
      | "media" | "supports" | "scope" | "layer" ) )]
;;

let _a = [%sedlex.regexp? 'A' | 'a']
let _b = [%sedlex.regexp? 'B' | 'b']
let _c = [%sedlex.regexp? 'C' | 'c']
let _d = [%sedlex.regexp? 'D' | 'd']
let _e = [%sedlex.regexp? 'e' | 'E']
let _f = [%sedlex.regexp? 'F' | 'f']
let _g = [%sedlex.regexp? 'G' | 'g']
let _h = [%sedlex.regexp? 'H' | 'h']
let _i = [%sedlex.regexp? 'I' | 'i']
let _j = [%sedlex.regexp? 'J' | 'j']
let _k = [%sedlex.regexp? 'K' | 'k']
let _l = [%sedlex.regexp? 'L' | 'l']
let _m = [%sedlex.regexp? 'M' | 'm']
let _n = [%sedlex.regexp? 'N' | 'n']
let _o = [%sedlex.regexp? 'O' | 'o']
let _p = [%sedlex.regexp? 'P' | 'p']
let _q = [%sedlex.regexp? 'Q' | 'q']
let _r = [%sedlex.regexp? 'R' | 'r']
let _s = [%sedlex.regexp? 'S' | 's']
let _t = [%sedlex.regexp? 'T' | 't']
let _u = [%sedlex.regexp? 'U' | 'u']
let _v = [%sedlex.regexp? 'V' | 'v']
let _w = [%sedlex.regexp? 'W' | 'w']
let _x = [%sedlex.regexp? 'X' | 'x']
let _y = [%sedlex.regexp? 'Y' | 'y']
let _z = [%sedlex.regexp? 'Z' | 'z']
let important = [%sedlex.regexp? "!", ws, _i, _m, _p, _o, _r, _t, _a, _n, _t]

let length =
  [%sedlex.regexp?
    ( _c, _a, _p
    | _c, _h
    | _e, _m
    | _e, _x
    | _i, _c
    | _l, _h
    | _r, _e, _m
    | _r, _l, _h
    | _v, _h
    | _v, _w
    | _v, _i
    | _v, _b
    | _v, _m, _i, _n
    | _v, _m, _a, _x
    | _c, _m
    | _m, _m
    | _q
    | _i, _n
    | _p, _c
    | _p, _t
    | _p, _x
    | _f, _r )]
;;

let angle = [%sedlex.regexp? _d, _e, _g | _g, _r, _a, _d | _r, _a, _d | _t, _u, _r, _n]
let time = [%sedlex.regexp? _s | _m, _s]
let frequency = [%sedlex.regexp? _h, _z | _k, _h, _z]

(* Returns true if white spaces were discarded *)
let discard_comments_and_white_spaces buf =
  let rec discard_white_spaces buf spaces_detected =
    match%sedlex buf with
    | Plus white_space -> discard_white_spaces buf true
    | "/*" -> discard_comments buf spaces_detected
    | _ -> spaces_detected
  and discard_comments buf spaces_detected =
    match%sedlex buf with
    | eof -> raise (LexingError (buf.Lex_buffer.pos, "Unterminated comment at EOF"))
    | "*/" -> discard_white_spaces buf spaces_detected
    | any -> discard_comments buf spaces_detected
    | _ -> assert false
  in
  discard_white_spaces buf false
;;

let rec get_next_tokens buf spaces_detected =
  let open Menhir_parser in
  match%sedlex buf with
  | eof -> [ EOF ]
  | ';' -> [ SEMI_COLON ]
  | '}' -> [ RIGHT_BRACE ]
  | '{' -> [ LEFT_BRACE ]
  | ':' -> if spaces_detected then [ WHITESPACE; COLON ] else [ COLON ]
  | '.' -> if spaces_detected then [ WHITESPACE; DOT ] else [ DOT ]
  | '&' -> if spaces_detected then [ WHITESPACE; AMPERSAND ] else [ AMPERSAND ]
  | '(' -> [ LEFT_PAREN ]
  | ')' -> [ RIGHT_PAREN ]
  | '[' -> [ LEFT_BRACKET ]
  | ']' -> [ RIGHT_BRACKET ]
  | '%' -> [ PERCENTAGE ]
  | operator -> [ OPERATOR (Lex_buffer.latin1 buf) ]
  | string -> [ STRING (Lex_buffer.latin1 ~skip:1 ~drop:1 buf) ]
  | "url(" -> [ get_url "" buf ]
  | important -> [ IMPORTANT ]
  | nested_at_rule -> [ NESTED_AT_RULE (Lex_buffer.latin1 ~skip:1 buf) ]
  | at_rule_without_body -> [ AT_RULE_WITHOUT_BODY (Lex_buffer.latin1 ~skip:1 buf) ]
  | at_rule -> [ AT_RULE (Lex_buffer.latin1 ~skip:1 buf) ]
  (* NOTE: should be placed above ident, otherwise pattern with
   * '-[0-9a-z]{1,6}' cannot be matched *)
  | _u, '+', unicode_range -> [ UNICODE_RANGE (Lex_buffer.latin1 buf) ]
  | ident, '(' ->
    let f = Lex_buffer.latin1 ~drop:1 buf in
    (match f with
     (* NOTE: These functions have a selector payload and are treated differently in the
        at parse-time as the way their payload gets parsed is different. *)
     | "has" | "not" | "where" | "host" | "host-context" | "is" -> [ SELECTOR_FUNCTION f ]
     | _ -> [ FUNCTION f ])
  | ident -> [ IDENT (Lex_buffer.latin1 buf) ]
  | '#', name ->
    if spaces_detected
    then [ WHITESPACE; HASH (Lex_buffer.latin1 ~skip:1 buf) ]
    else [ HASH (Lex_buffer.latin1 ~skip:1 buf) ]
  | number -> [ get_dimension (Lex_buffer.latin1 buf) buf ]
  | any -> [ DELIM (Lex_buffer.latin1 buf) ]
  | _ -> assert false

and get_dimension n buf =
  match%sedlex buf with
  | length -> FLOAT_DIMENSION (n, Lex_buffer.latin1 buf, Types.Length)
  | angle -> FLOAT_DIMENSION (n, Lex_buffer.latin1 buf, Types.Angle)
  | time -> FLOAT_DIMENSION (n, Lex_buffer.latin1 buf, Types.Time)
  | frequency -> FLOAT_DIMENSION (n, Lex_buffer.latin1 buf, Types.Frequency)
  | ident -> DIMENSION (n, Lex_buffer.latin1 buf)
  | _ -> NUMBER n

and get_url url buf =
  match%sedlex buf with
  | ws -> get_url url buf
  | url -> get_url (Lex_buffer.latin1 buf) buf
  | ")" -> URI url
  | eof -> raise (LexingError (buf.Lex_buffer.pos, "Incomplete URI"))
  | any ->
    raise
      (LexingError
         ( buf.Lex_buffer.pos
         , "Unexpected token: " ^ Lex_buffer.latin1 buf ^ " parsing an URI" ))
  | _ -> assert false
;;

let token_queue = Queue.create ()

let queue_next_tokens_with_location buf =
  let spaces_detected = discard_comments_and_white_spaces buf in
  let loc_start = Lex_buffer.next_loc buf in
  let tokens = get_next_tokens buf spaces_detected in
  let loc_end = Lex_buffer.next_loc buf in
  List.iter (fun t -> Queue.add (t, loc_start, loc_end) token_queue) tokens
;;

let parse buf p =
  let last_token = ref (Menhir_parser.EOF, Lexing.dummy_pos, Lexing.dummy_pos) in
  let next_token () =
    if Queue.is_empty token_queue then queue_next_tokens_with_location buf;
    last_token := Queue.take token_queue;
    !last_token
  in
  try MenhirLib.Convert.Simplified.traditional2revised p next_token with
  | LexingError _ as e -> raise e
  | _ -> raise (ParseError !last_token)
;;

let parse_string ?container_lnum ?pos s p =
  (match container_lnum with
   | None -> ()
   | Some lnum -> Lex_buffer.container_lnum_ref := lnum);
  parse (Lex_buffer.of_ascii_string ?pos s) p
;;
