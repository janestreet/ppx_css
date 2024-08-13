open! Core

module Private : sig
  (** [append css_string] will:

      1. Add [css_string] to a global map.
      2. Will concatenate all of the css strings from the global map into a single big css string.
      3. Will register this big css string into the web page.

      Steps [2] and [3] are slow.
  *)
  val append : string -> unit

  (** [prepend] is like [append], but instead of the added CSS being added to the "bottom"
      of the stylesheet, it'll be added to the top of the stylesheet. *)
  val prepend : string -> unit

  (** [append_but_do_not_update] is a performance optimization.

     ppx_css used to always call [append] for __EACH__ [%css] instance. This was slow as
     steps [2] and [3] really only need to happen once! [append_but_do_not_update]. Will
     only do step [1], and assumes that steps [2] and [3]. are done later - after all
     other CSS has been registered.

     You can perform steps [2] and [3] with [update_if_not_already_updated].
  *)
  val append_but_do_not_update : string -> unit

  (** [prepend_but_do_not_update] is like [prepend], but with [append_but_do_not_update]'s
      behavior. *)
  val prepend_but_do_not_update : string -> unit

  (** [update_if_not_already_updated] will perform steps [2] and [3]. Incr_dom.start and
      Bonsai_web.start call this function. If you are not using incr_dom/bonsai's start app,
      you will need to explicitly call this function within you "main.bc.js" entry-point
      for your web app. *)
  val update_if_not_already_updated : unit -> unit

  module Dynamic = Inline_css_dynamic
end

module For_testing : sig
  val to_string : unit -> string
  val strategy_name : unit -> string
  val dump_strategy_state : unit -> unit
end
