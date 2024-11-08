open! Core
open! Js_of_ocaml

module Style_sheet : sig
  class type t = object
    method replaceSync : Js.js_string Js.t -> unit Js.meth
  end
end

type t = Style_sheet.t Js.t

val update_stylesheet : t -> string -> unit
val delete_stylesheet : t -> unit
val create_stylesheet : unit -> t Or_error.t
val append_stylesheet : t -> unit
val prepend_stylesheet : t -> unit

module For_testing : sig
  val reinitialize : unit -> unit
end
