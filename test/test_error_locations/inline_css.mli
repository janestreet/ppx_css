open! Core

module Private : sig
  val create_stylesheet : unit -> unit
  val append : string -> unit
  val update_stylesheet : _ -> string -> unit
end
