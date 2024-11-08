open! Core

type t =
  | Id of string
  | Class of string
  | Variable of string
[@@deriving compare, variants, sexp, hash, equal]

include Comparable.S with type t := t
include Hashable.S with type t := t

(* Returns the identifier as the original CSS string identifier *)
val extract_css_identifier : t -> string
val extract_ocaml_identifier : t -> string
val set_includes_class_or_id : Set.t -> [ `Both | `Only_id | `Only_class | `None ]
val group_by_ocaml_identifier : t List.t -> Set.t String.Map.t
val to_css_selector : t -> string
