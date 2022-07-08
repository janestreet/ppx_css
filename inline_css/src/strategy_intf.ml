open! Core

module type S = sig
  type t

  val name : string
  val initialize : unit -> t Or_error.t
  val update : t -> string -> unit
end

type t =
  | T :
      { state : 'a
      ; strategy : (module S with type t = 'a)
      }
      -> t
