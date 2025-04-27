open! Core
open! Ppxlib
module Rule_id = Rule_id
module Stable_stylesheet = Stable_stylesheet
module Css_identifier = Css_identifier

module With_hoisted_expression : sig
  (** ['a t]'s [ppx_css_string_expression] contains the css string that should be appended
      at the end of the file after all ppx'es have been expanded. *)
  type 'a t =
    { txt : 'a
    ; hoisted_structure_items : structure_item list
    ; css_string_for_testing : string Lazy.t
    }
end

module For_css_inliner : sig
  type result =
    { ml_file : string
    ; css_string_for_testing : string Lazy.t
    }

  val gen_struct
    :  dont_hash:String.Set.t
    -> css_string:string
    -> dont_hash_prefixes:string list
    -> stylesheet_location:location
    -> lazy_loading_optimization:
         Ppx_css_syntax.Preprocess_arguments.lazy_loading_optimization
    -> disable_hashing:bool
    -> result

  val gen_sig : stylesheet_location:location -> string -> string
end

module For_testing : sig
  val generate_struct
    :  loc:location
    -> disable_hashing:bool
    -> expression
    -> structure With_hoisted_expression.t

  val generate_inline_expression
    :  loc:location
    -> disable_hashing:bool
    -> expression
    -> expression With_hoisted_expression.t

  val generate_css_stylesheet_string
    :  loc:location
    -> disable_hashing:bool
    -> expression
    -> string

  val generate_css_inline_string
    :  loc:location
    -> disable_hashing:bool
    -> expression
    -> string

  val create_should_hash_identifier
    :  dont_hash:String.Set.t
    -> dont_hash_prefixes:string list
    -> always_hash:String.Set.t
    -> (Css_identifier.t -> [ `Hash | `Dont_hash | `Dont_hash_prefixes of string ])
         Staged.t

  val map_style_sheet
    :  Stable_stylesheet.t
    -> f:(Css_identifier.t -> location -> label)
    -> Stable_stylesheet.t

  module Traverse_css = Traverse_css

  val ppx_css_expression_to_structure_item : loc:location -> expression -> structure_item
  val reset_anonymous_variable_identifiers : unit -> unit
end
