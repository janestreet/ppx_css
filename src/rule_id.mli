open! Core

type t [@@deriving sexp]

include Comparable.S with type t := t

val identify_stylesheet : Css_parser.Rule.t list -> Css_parser.Rule.t Map.t
val to_string : t -> string
val to_int : t -> int
val of_int_potentially_unsafe : int -> t
