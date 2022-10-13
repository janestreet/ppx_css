open! Core
open! Ppxlib

type t = { dont_hash : String.Set.t }

val parse : expression -> loc:location -> string * t
