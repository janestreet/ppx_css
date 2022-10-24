open! Core
open! Ppxlib
open Css_jane
module Options = Options

module For_css_inliner : sig
  val gen_struct : options:Options.t -> string
  val gen_sig : string -> string
end

module For_testing : sig
  val generate_struct
    :  allow_potential_accidental_hashing:bool
    -> expression
    -> module_expr

  val map_style_sheet
    :  Stylesheet.t
    -> allow_potential_accidental_hashing:bool
    -> rewrite:expression String.Map.t
    -> f:([ `Class of label | `Id of label | `Variable of label ] -> location -> label)
    -> Stylesheet.t

  module Traverse_css = Traverse_css
end
