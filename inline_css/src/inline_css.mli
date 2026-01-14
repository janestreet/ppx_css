open! Core

module Private : sig
  (** [append css_string] will append a stylesheet to the webpage *)
  val append : string -> unit

  (** [prepend] is like [append], but instead of the added CSS being added to the back of
      the list of stylesheets, it'll be added to the front. *)
  val prepend : string -> unit

  val create_stylesheet : unit -> Stylesheet.t Or_error.t
  val append_stylesheet : Stylesheet.t Or_error.t -> unit
  val prepend_stylesheet : Stylesheet.t Or_error.t -> unit
  val update_stylesheet : Stylesheet.t Or_error.t -> string -> unit

  module Dynamic = Inline_css_dynamic
end

(* Runtime module for ppx_css that makes sure that commonly-shadowed functions cannot be
   shadowed *)
module Ppx_css_runtime : sig
  val force : 'a lazy_t -> 'a
end

module For_testing : sig
  val reinitialize_stylesheet : unit -> unit
end
