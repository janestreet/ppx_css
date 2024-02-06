open! Core

module Private : sig
  val append : string -> unit
  val prepend : string -> unit

  module Dynamic = Inline_css_dynamic
end

module For_testing : sig
  val to_string : unit -> string
  val strategy_name : unit -> string
  val dump_strategy_state : unit -> unit
end
