open! Core
open! Ppxlib
open Css_jane

module Identifier_kind : sig
  type t =
    | Class
    | Id
    | Variable
  [@@deriving compare, sexp]

  include Comparable.S with type t := t
end

module Transform : sig
  type result =
    { css_string : string
    ; identifier_mapping : (Identifier_kind.Set.t * expression) String.Table.t
    ; reference_order : expression list
    }

  (* Transform takes a string of css and produces a new css string, with
     all of the identifiers ammended with a hash.  The mapping from
     original identifier to hashed identifier is also returned.

     [pos] is the source-code-position of the css string in the ocaml file, so
     that error messages from parsing the css show up in the right location. *)
  val f : loc:location -> pos:position -> options:Options.t -> result
end

module Get_all_identifiers : sig
  type result =
    { variables : string list
    ; identifiers : (string * [ `Both | `Only_class | `Only_id ]) list
    }
  [@@deriving sexp_of]

  val f : Stylesheet.t -> result
end

module For_testing : sig
  val map_style_sheet
    :  Stylesheet.t
    -> rewrite:expression String.Map.t
    -> dont_hash_prefixes:string list
    -> f:([ `Class of label | `Id of label | `Variable of label ] -> location -> label)
    -> Stylesheet.t
end
