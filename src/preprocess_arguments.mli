open! Core

type t =
  { dont_hash_prefixes : String.Set.t
  ; dont_hash : String.Set.t
  ; rewrite : string String.Map.t
  }

val get : unit -> t
val add_dont_hash : string -> unit
val add_dont_hash_prefixes : string -> unit
val add_rewrite : from:string -> to_:string -> unit
