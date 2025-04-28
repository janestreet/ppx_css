open! Core
open Ocaml_common

(** [string_of_tokens s] will return the OCaml tokens for s as a list. If there is a
    syntax error, then it'll return an error. *)
val string_tokens : string -> Parser.token list Or_error.t

(** [rsplit_on_hash] will split a string like `EXPR#Foo` into [Some ("EXPR", "Foo")].

    [rsplit_on_hash] will correctly ignore escaped `#`'s in situations like:

    `EXPR "#"`. *)
val rsplit_on_hash : string -> (string * string) option

val tokens_with_loc : string -> (Parser.token * Warnings.loc) list Or_error.t
