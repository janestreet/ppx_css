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
  ; dont_hash_prefixes : string list
  }

(** Given the AST of an expression like [stylesheet ~rewrite:[] ""] will result in a
    "parsed" [t].

    [stylesheet] is the [%css stylesheet {|stylesheet...|}] syntax and expands to a module declaration.
*)
val parse_stylesheet : expression -> t

(** Given the AST of an expression like ["" ~rewrite:[]] will result in a "parsed" [t]

    [inline] is the syntax [%css {|declarations...|}] and expands to an expression. *)
val parse_inline_expression : expression -> t

val empty_stylesheet : css_string:string -> t

module Serializable_options : sig
  type options := t

  type t =
    { rewrite : string String.Map.t
    ; dont_hash : string list
    ; dont_hash_prefixes : string list
    }
  [@@deriving of_sexp]

  val to_stylesheet_options : t -> css_string:string -> options
end
