open! Core

module T = struct
  type t =
    | Pattern
    | Expression
  [@@deriving variants, compare, sexp, enumerate]
end

include Comparable.Make (T)
include T

let all = Set.of_list all
