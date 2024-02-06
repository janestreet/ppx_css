open! Core

type t

include Strategy_intf.S with type t := t

val delete_stylesheet : t -> unit
