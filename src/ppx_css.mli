open! Core
open! Ppxlib
open Css_jane

module With_hoisted_expression : sig
  (** ['a t]'s [ppx_css_string_expression] contains the css string that should be appended
      at the end of the file after all ppx'es have been expanded. *)
  type 'a t =
    { txt : 'a
    ; ppx_css_string_expression : expression
    ; css_string_for_testing : string
    }
end

module For_css_inliner : sig
  val gen_struct
    :  dont_hash:String.Set.t
    -> css_string:string
    -> dont_hash_prefixes:string list
    -> stylesheet_location:location
    -> string

  val gen_sig : string -> string
end

module For_testing : sig
  val generate_struct : expression -> structure With_hoisted_expression.t
  val generate_inline_expression : expression -> expression With_hoisted_expression.t
  val generate_css_stylesheet_string : loc:location -> expression -> string
  val generate_css_inline_string : loc:location -> expression -> string

  val map_style_sheet
    :  Stylesheet.t
    -> dont_hash:String.Set.t
    -> dont_hash_prefixes:string list
    -> f:([ `Class of label | `Id of label | `Variable of label ] -> location -> label)
    -> Stylesheet.t

  module Traverse_css = Traverse_css

  val ppx_css_expression_to_structure_item : loc:location -> expression -> structure_item
  val reset_anonymous_variable_identifiers : unit -> unit
end
