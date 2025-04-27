open! Core

type error =
  { start_pos : Types.Position.t
  ; end_pos : Types.Position.t
  ; message : string
  }
[@@deriving sexp_of]

exception Lexing_error of error
exception Unknown_error of error
exception Parse_error of error
