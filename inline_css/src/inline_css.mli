open! Core

module Private : sig
  val append : string -> unit
end

module For_testing : sig
  val print : unit -> unit
  val strategy_name : unit -> string
  val dump_strategy_state : unit -> unit
end
