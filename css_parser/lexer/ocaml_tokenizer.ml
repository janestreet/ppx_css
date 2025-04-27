open! Core
open Ocaml_common

let string_lexbuf string = Lexing.from_string string ~with_positions:true

let rec fold_on_tokens lexbuf acc ~f =
  match (Ocaml_common.Lexer.token_with_comments lexbuf : Parser.token) with
  | EOF as tok ->
    let loc = Location.curr lexbuf in
    let acc = f acc (tok, loc) in
    acc
  | tok ->
    let loc =
      match tok with
      | COMMENT (_, loc) -> loc
      | DOCSTRING ds -> Docstrings.docstring_loc ds
      | _ -> Location.curr lexbuf
    in
    let acc = f acc (tok, loc) in
    fold_on_tokens lexbuf acc ~f
;;

let tokens_of string =
  fold_on_tokens (string_lexbuf string) [] ~f:(fun acc x -> x :: acc) |> List.rev
;;

let string_tokens_exn string =
  tokens_of string
  |> List.map ~f:fst
  |> List.filter ~f:(function
    | EOF -> false
    | _ -> true)
;;

let try_with_syntax_error f =
  match Ocaml_common.Warnings.without_warnings f with
  | x -> Ok x
  | exception
      (( Ocaml_common.Lexer.Error _
       | Syntaxerr.Error _
       | Stdlib.Parsing.Parse_error
       | Language_extension.Error.Error _ ) as exn) -> Error (Base.Error.of_exn exn)
;;

let tokens_with_loc str = try_with_syntax_error (fun () -> tokens_of str)

let string_tokens : string -> Parser.token list Base.Or_error.t =
  fun string -> try_with_syntax_error (fun () -> string_tokens_exn string)
;;

type rsplit_acc = { last_seen_hash_index : int option }

let rsplit_on_hash : string -> (string * string) option =
  fun string ->
  Option.try_with_join
  @@ fun () ->
  let { last_seen_hash_index } =
    fold_on_tokens
      (Lexing.from_string string)
      { last_seen_hash_index = None }
      ~f:
        (fun
          acc
          ( token
          , { loc_start = { pos_fname = _; pos_lnum = _; pos_bol = _; pos_cnum }
            ; loc_end = _
            ; loc_ghost = _
            } )
        ->
        match token with
        | HASH | HASH_SUFFIX -> { last_seen_hash_index = Some pos_cnum }
        | _ -> acc)
  in
  Option.map last_seen_hash_index ~f:(fun index ->
    let left = String.sub string ~pos:0 ~len:index in
    let right =
      String.sub
        string
        ~pos:(index + 1)
        ~len:(String.length string - 1 - String.length left)
    in
    left, right)
;;
