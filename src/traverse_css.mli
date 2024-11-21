open! Core
open! Ppxlib
open Css_jane

module Original_and_post_processed : sig
  type 'a t =
    { original : 'a
    ; post_processed : 'a
    }
end

module Transform : sig
  type result =
    { stylesheet : Rule.t Original_and_post_processed.t Rule_id.Map.t * location
    ; identifier_mapping : expression Css_identifier.Table.t
    }

  val generate_hash : pos:position -> Stable_stylesheet.t -> string

  (* Transform takes a string of css and produces a new css string, with
     all of the identifiers ammended with a hash.  The mapping from
     original identifier to hashed identifier is also returned.

     [pos] is the source-code-position of the css string in the ocaml file, so
     that error messages from parsing the css show up in the right location. *)
  val f
    :  pos:position
    -> should_hash_identifier:
         (Css_identifier.t -> [ `Hash | `Dont_hash | `Dont_hash_prefixes of string ])
    -> Stable_stylesheet.t
    -> result
end

module Graph : sig
  module Group_type : sig
    type t =
      | Autoforced
      | Group of int
    [@@deriving sexp_of, string, compare, hash]

    include Comparable.S with type t := t
    include Hashable.S with type t := t
  end

  type result =
    { identifiers : Css_identifier.Set.t
    ; get_group_for_identifier : Css_identifier.t -> Group_type.t
    ; group_to_rule_indices :
        (Rule_id.t, Rule_id.comparator_witness) Nonempty_set.t Group_type.Map.t
    ; group_to_rules : Rule.t list Group_type.Map.t
    }

  (* Iterates through a pre-hashed stylesheet and checks to see which top-level rules 
     form strongly-connected components. The algorithm is as follows:

     1. Iterate through all identifiers:
       a. Retrieve all identifiers and top-level identifiers for rule
       b. If all top-level identifiers for rule satisfy the following, remove rule from 
          consideration within the graph and add it to the autoforced group

          i. Identifier is a top-level tag such as html, div, span, body, etc.
          ii. Identifier is included in ~dont_hash or ~dont_hash_prefixes

     Identifiers within selector functions such as :where are ignored by default, but some 
     are special-cased so that we retrieve the identifiers within them.

     Some at-rules are special-cased so that if they include one style rule within 
     them, the direct child style-rule is considered as the top-level rule for that 
     at-rule

     Example:
     ```css
     @layer test {
       .a + .b {
        .c {}
       }
     }
     ```

     The top-level identifiers for [@layer test] would be [a] and [b].

     The identifiers used in this function are the CSS identifiers converted to ocaml 
     identifiers.
  *)
  val map_stylesheet_for_graph
    :  lazy_loading_optimization:
         Ppx_css_syntax.Preprocess_arguments.lazy_loading_optimization
    -> should_hash_identifier:
         (Css_identifier.t -> [ `Hash | `Dont_hash | `Dont_hash_prefixes of string ])
    -> Stable_stylesheet.t
    -> result
end

val get_all_identifiers : Stable_stylesheet.t -> Css_identifier.Set.t

val raise_if_unused_dont_hash_or_prefixes_or_collisions
  :  loc:location
  -> unused_allow_set:String.Set.t
  -> unused_dont_hash:String.Set.t
  -> unused_dont_hash_prefixes:String.Set.t
  -> should_hash_identifier:
       (Css_identifier.t -> [ `Hash | `Dont_hash | `Dont_hash_prefixes of string ])
  -> Stable_stylesheet.t
  -> unit

(* Takes a [Stylesheet.t] and pre-processes it so that some rules are split into
     multiple top-level rules *)
val split_layers : Stylesheet.t -> Stylesheet.t

module For_testing : sig
  val map_style_sheet
    :  Stable_stylesheet.t
    -> f:(Css_identifier.t -> location -> label)
    -> Stable_stylesheet.t
end
