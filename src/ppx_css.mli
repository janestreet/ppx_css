open! Core
open! Ppxlib
open Css_jane
module Options = Options

module With_hoisted_expression : sig
  (** ['a t]'s [ppx_css_string_expression] contains the css string that should be appended
      at the end of the file after all ppx'es have been expanded. *)
  type 'a t =
    { txt : 'a
    ; ppx_css_string_expression : expression
    }
end

module For_css_inliner : sig
  val gen_struct
    :  rewrite:expression String.Map.t
    -> css_string:string
    -> dont_hash_prefixes:string list
    -> stylesheet_location:location
    -> string

  val gen_sig : string -> string
end

module For_testing : sig
  val generate_struct : expression -> structure With_hoisted_expression.t
  val generate_inline_expression : expression -> expression With_hoisted_expression.t

  val map_style_sheet
    :  Stylesheet.t
    -> rewrite:expression String.Map.t
    -> dont_hash_prefixes:string list
    -> f:([ `Class of label | `Id of label | `Variable of label ] -> location -> label)
    -> Stylesheet.t

  module Traverse_css = Traverse_css

  val ppx_css_expression_to_structure_item : loc:location -> expression -> structure_item
end
