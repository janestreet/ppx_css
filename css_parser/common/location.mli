open! Core

type t = Warnings.loc =
  { loc_start : Lexing.position
  ; loc_end : Lexing.position
  ; loc_ghost : bool
  }
[@@deriving sexp_of, equal, to_string]

val of_positions : start:Lexing.position -> end_:Lexing.position -> t
val merge : start:t -> end_:t -> t
val between_tokens : start:Token.t * t -> end_:Token.t * t -> t
val from_token : Token.t * t -> t
