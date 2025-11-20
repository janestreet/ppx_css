open! Core

type t = Sedlexing.lexbuf

(* This marker occasionally shows up at the beginning of a file and denotes endianness for
   certain encodings. We need to remove it or it's parsed as an identifier by the lexer
   and causes the parser to error out due to identifiers not being where they're expected.

   utf8 does not have endianness which is why this marker can be ignored
*)
let byte_order_mark = "\u{FEFF}"

let of_utf8_string ?(filename = "<n/a>") ?pos str =
  let buf =
    let str = String.chop_prefix_if_exists ~prefix:byte_order_mark str in
    Sedlexing.Utf8.from_string str
  in
  let pos =
    match pos with
    | None ->
      { Lexing.pos_fname =
          (* This value is ignored by Sedlexing in [set_position]. The actual filename is
             set below using [set_filename] *)
          filename
      ; pos_lnum = 1
      ; (* line number *)
        pos_bol = 0
      ; (* offset of beginning of current line *)
        pos_cnum = 0 (* total offset *)
      }
    | Some p -> p
  in
  Sedlexing.set_position buf pos;
  (match filename with
   | "<n/a>" -> ()
   | filename -> Sedlexing.set_filename buf filename);
  buf
;;

let __private__next_int = Sedlexing.__private__next_int
let mark = Sedlexing.mark
let rollback = Sedlexing.rollback
let start = Sedlexing.start
let next = Sedlexing.next
let backtrack = Sedlexing.backtrack
let lexing_positions = Sedlexing.lexing_positions

let utf8 ?(start = 0) ?(end_ = 0) lexbuf =
  let len = Sedlexing.lexeme_length lexbuf - start - end_ in
  Sedlexing.Utf8.sub_lexeme lexbuf start len
;;

let utf8_between ~start ~end_ lexbuf = Sedlexing.Utf8.sub_lexeme lexbuf start end_

let make_loc ?(loc_ghost = false) start_pos end_pos =
  { Location.loc_ghost; loc_start = start_pos; loc_end = end_pos }
;;

let with_loc' buf ~f =
  let start_pos, _ = lexing_positions buf in
  let out = f ~start_pos in
  let _, end_pos = lexing_positions buf in
  let loc = make_loc start_pos end_pos in
  out, loc
;;

let with_loc buf ~f = with_loc' buf ~f:(fun ~start_pos:_ -> f ())
