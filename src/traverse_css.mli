open! Core
open! Ppxlib
open Css_jane

module Transform : sig
  type result =
    { css_string : string
    ; identifier_mapping :
        [ `Identifier of expression | `Variable of expression ] String.Table.t
    ; reference_order : expression list
    }

  (* Transform takes a string of css and produces a new css string, with
     all of the identifiers ammended with a hash.  The mapping from
     original identifier to hashed identifier is also returned.

     [pos] is the source-code-position of the css string in the ocaml file, so
     that error messages from parsing the css show up in the right location. *)
  val f
    :  allow_potential_accidental_hashing:bool
    -> loc:location
    -> pos:position
    -> options:Options.t
    -> result
end

module Get_all_identifiers : sig
  type result =
    { variables : string list
    ; identifiers : string list
    }
  [@@deriving sexp_of]

  val f : Stylesheet.t -> result
end

module For_testing : sig
  val map_style_sheet
    :  Stylesheet.t
    -> allow_potential_accidental_hashing:bool
    -> rewrite:expression String.Map.t
    -> f:([ `Class of label | `Id of label | `Variable of label ] -> location -> label)
    -> Stylesheet.t
end
