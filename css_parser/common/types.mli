open! Core

module Hash_flag : sig
  type t =
    | Id
    | Unrestricted
  [@@deriving to_string, sexp_of, equal, compare]
end

module Numeric_value : sig
  type t =
    | Integer
    | Float
  [@@deriving to_string, sexp_of, equal, compare]
end

(* We need to maintain the capitalization here so that we can keep the E consistent for
   things such as unicode ranges

   Without the capitalization parameter, we'd be formatting this unicode range: U+0EFF

   to

   U+0eFF

   Unicode ranges can only exist within values per this spec:
   https://www.w3.org/TR/css-syntax-3/#urange-syntax
*)
module Exponent : sig
  type t =
    { value : string
    ; capitalized : bool
    }
  [@@deriving sexp_of, to_string, equal, compare]
end

module Position : sig
  type t = Lexing.position [@@deriving sexp_of, to_string, equal, compare]
end

module String_token : sig
  type quote_type =
    | Single
    | Double
  [@@deriving sexp_of, to_string, equal, compare]

  type t =
    { value : string
    ; quote_type : quote_type
    }
  [@@deriving sexp_of, to_string, equal, compare]
end

module Ident_like : sig
  type ident_like_part =
    | String of string
    | Hex_escape of string
  [@@deriving equal, compare, variants, sexp_of, to_string]

  type t = ident_like_part list [@@deriving equal, compare, sexp_of, to_string]
end

(** Semantic CSS context at an arbitrary position in the parse tree *)
module Context : sig
  type t =
    | Interpolation (* Ocaml interpolations like `color: %{<value>}` *)
    | Declaration of { property_name : string }
      (* Property value declarations like `color: <value>` *)
    | Block (* Style blocks like `.div {<value>}` *)
    | Rules (* Rule lists like the root of a stylesheet *)
    | Ambiguous of t list
      (* A combination of multiple contexts when the parsing is ambiguous *)
  [@@deriving sexp_of]
end
