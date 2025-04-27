open! Core

type error =
  { start_pos : Types.Position.t
  ; end_pos : Types.Position.t
  ; message : string
  }
[@@deriving sexp_of]

exception Lexing_error of error [@@deriving sexp_of]
exception Unknown_error of error [@@deriving sexp_of]
exception Parse_error of error [@@deriving sexp_of]

let make_loc ?(loc_ghost = false) start_pos end_pos : Location.t =
  { Warnings.loc_start = start_pos; loc_end = end_pos; loc_ghost }
;;

let () =
  Ocaml_common.Location.register_error_of_exn (function
    | Lexing_error { start_pos; end_pos; message } ->
      let loc = make_loc start_pos end_pos in
      Some (Ocaml_common.Location.error ~loc message)
    | Unknown_error { start_pos; end_pos; message } ->
      let loc = make_loc start_pos end_pos in
      Some (Ocaml_common.Location.error ~loc message)
    | Parse_error { start_pos; end_pos; message } ->
      let loc = make_loc start_pos end_pos in
      Some (Ocaml_common.Location.error ~loc message)
    | _ -> None)
;;
