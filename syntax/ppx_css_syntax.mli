open! Core
open! Ppxlib
module String_constant = String_constant

(** This module parses a call to [ppx_css] into nicer parts to make the implementation of
    [ppx_css] cleaner. *)

type t =
  { rewrite : expression String.Map.t
  (** For a given (key, value) pair, value will "rewrite" itself wherever "key" is roughly: [s/$key/$value/g]

      Equivalent to the [~rewrite] parameter in the call to [stylesheet] on [ppx_css].
  *)
  ; css_string : String_constant.t
  (** The contained CSS string. Equivalent to the string parameter given to ppx_css. *)
  ; dont_hash_prefixes : string list
  }

(** Given the AST of an expression like [stylesheet ~rewrite:[] ""] will result in a
    "parsed" [t].

    [stylesheet] is the [%css stylesheet {|stylesheet...|}] syntax and expands to a module declaration.
*)
val parse_stylesheet_exn : expression -> t

(** Given the AST of an expression like ["" ~rewrite:[]] will result in a "parsed" [t]

    [inline] is the syntax [%css {|declarations...|}] and expands to an expression. *)
val parse_inline_expression_exn : expression -> t

val empty_stylesheet : css_string:String_constant.t -> t

module Serializable_options : sig
  type options := t

  type t =
    { rewrite : string String.Map.t
    ; dont_hash : string list
    ; dont_hash_prefixes : string list
    }
  [@@deriving of_sexp]

  val to_stylesheet_options : t -> css_string:String_constant.t -> options
end

module Preprocess_arguments = Preprocess_arguments
