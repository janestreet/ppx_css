open! Core
open! Ppxlib

(** This module parses a call to [ppx_css] into nicer parts to make the implementation of
    [ppx_css] cleaner. *)

type t =
  { rewrite : expression String.Map.t
  (** For a given (key, value) pair, value will "rewrite" itself wherever "key" is roughly: [s/$key/$value/g]

      Equivalent to the [~rewrite] parameter in the call to [stylesheet] on [ppx_css].
  *)
  ; css_string : string
  (** The contained CSS string. Equivalent to the string parameter given to ppx_css. *)
  ; stylesheet_location : location
  (** Equivalent to where the "stylesheet" identifier is located from within the call to
      ppx_css. This is used to potentially give merlin information for MerlinTypeOf to
      know the type of the [stylesheet] function. *)
  ; dont_hash_prefixes : String.Set.t
  }

(** Given the AST of an expression like [stylesheet ~rewrite:[] ""] will result in a
    "parsed" [t]. *)
val parse : expression -> t

val empty : css_string:string -> t

module Serializable_options : sig
  type options := t

  type t =
    { rewrite : string String.Map.t
    ; dont_hash : string list
    ; dont_hash_prefixes : string list
    }
  [@@deriving of_sexp]

  val to_options : t -> css_string:string -> options
end