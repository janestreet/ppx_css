open! Core

type t =
  { vertices : Css_identifier.Hash_set.t
  ; edges : Css_identifier.t list Css_identifier.Table.t
  }

include Graph.Components.G with type t := t and type V.t = Css_identifier.t

val create : unit -> t
val add_edge : t -> Css_identifier.t -> Css_identifier.t -> unit
val add_vertex_if_needed : t -> Css_identifier.t -> unit
