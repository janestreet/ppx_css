open! Core

module type S = sig
  type t

  val name : string
  val initialize : unit -> t Or_error.t
  val update : t -> string -> unit

  module For_testing : sig
    (* Meant to let us see how many times [update] is called. *)
    val dump_testing_state : t -> string
  end
end

type t =
  | T :
      { state : 'a
      ; strategy : (module S with type t = 'a)
      }
      -> t
