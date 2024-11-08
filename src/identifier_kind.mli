open! Core

type t =
  | Pattern
  | Expression
[@@deriving variants, compare, sexp]

include Comparable.S with type t := t

val all : Set.t
