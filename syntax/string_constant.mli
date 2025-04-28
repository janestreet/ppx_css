open! Core
open Ppxlib

(** [String_constant.t] represents an OCaml string constant from ppxlib's AST.

    from ppxlib's docs:

    {v
       Constant string such as ["constant"]
       [{delim|other constant|delim}].

       The location span the content of the string, without the delimiters.
    v} *)
type t =
  { css_string : string
  ; string_loc : location
  (** Equivalent to where the "stylesheet" identifier is located from within the call to
      ppx_css. This is used to potentially give merlin information for MerlinTypeOf to
      know the type of the [stylesheet] function. *)
  ; delimiter : string option
  }
