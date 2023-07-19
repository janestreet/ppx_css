open! Core

(** This module contains the parameters sent in via the jbuild/dune file's [preprocess]
    field.

    The way of interfacing/reading/setting is side-effecty. The arguments 
    are read at the top-level when the module is loaded, and then read with [get]
*)

type t =
  { dont_hash_prefixes : String.Set.t
  ; dont_hash : String.Set.t
  ; rewrite : string String.Map.t
  }

val get : unit -> t
val add_dont_hash : string -> unit
val add_dont_hash_prefixes : string -> unit
val add_rewrite : from:string -> to_:string -> unit
