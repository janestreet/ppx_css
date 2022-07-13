open! Core
open! Ppxlib

type t =
  { css_string : string
  ; mapping : string String.Table.t
  }

(* Transform takes a string of css and produces a new css string, with
   all of the identifiers ammended with a hash.  The mapping from
   original identifier to hashed identifier is also returned.

   [pos] is the source-code-position of the css string in the ocaml file, so
   that error messages from parsing the css show up in the right location. *)
val transform : pos:position -> dont_hash_these:String.Set.t -> string -> t
