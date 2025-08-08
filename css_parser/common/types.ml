open Core

module Hash_flag = struct
  type t =
    | Id
    | Unrestricted
  [@@deriving to_string, sexp_of, equal, compare]
end

module Numeric_value = struct
  type t =
    | Integer
    | Float
  [@@deriving to_string, sexp_of, equal, compare]
end

module Exponent = struct
  type t =
    { value : string
    ; capitalized : bool
    }
  [@@deriving sexp_of, equal, compare]

  let to_string { value; capitalized } =
    let leading_e = if capitalized then "E" else "e" in
    [%string "%{leading_e}%{value}"]
  ;;
end

module Position = struct
  type t = Lexing.position =
    { pos_fname : string
    ; pos_lnum : int
    ; pos_bol : int
    ; pos_cnum : int
    }
  [@@deriving sexp_of, equal, compare]

  let to_string t =
    let filename =
      match t.pos_fname with
      | "<n/a>" | "" -> ""
      | filename -> filename ^ ":"
    in
    let col_num = t.pos_cnum - t.pos_bol + 1 in
    [%string {|%{filename}%{t.pos_lnum#Int}:%{col_num#Int}|}]
  ;;
end

module String_token = struct
  type quote_type =
    | Single
    | Double
  [@@deriving sexp_of, to_string, equal, compare]

  type t =
    { value : string
    ; quote_type : quote_type
    }
  [@@deriving sexp_of, equal, compare]

  let to_string { value; quote_type } =
    match quote_type with
    | Double ->
      (* Make sure to escape the same type of quotes if they're contained within *)
      let value = String.split ~on:'"' value |> String.concat ~sep:{|\"|} in
      [%string {|"%{value}"|}]
    | Single ->
      (* Make sure to escape the same type of quotes if they're contained within *)
      let value = String.split ~on:'\'' value |> String.concat ~sep:{|\'|} in
      [%string {|'%{value}'|}]
  ;;
end

(* Hexadecimal escape sequences allow for an optional space afterwards to delimit where 
   the escaped value is ending as they map to unicode values.

   This module is necessary so that we can ensure that we are maintaining the same AST 
   regardless of if the optional space is included or left out of the end of a hexadecimal escape
   sequence in an ident-like token. If we just maintain the strings as-is without the variants,

   .class\de{ 

   will be parsed and then printed into 

   .class\de { 

   which will then be parsed and printed into 

   .class\de  {

   All of which have different ASTs.


   Both of these:
   [\26 123] [\000026123] <-- Hexadecimal values can only have up to 6 digits so the 
   space is not required for the latter

   Parse into [&123]

   However, [\26123] parses into [https://unicodeplus.com/U+26123]

   To ensure that we are maintaining a consistent AST, all hexadecimal escape sequences are
   being output as [<sequence><space>] when turned into a selector/pretty-printed. This is
   to avoid the need of escaping characters if we convert the unicode value to the character
   it represents. We can always change this later on if desired
*)
module Ident_like = struct
  type ident_like_part =
    | String of string
    | Hex_escape of string
  [@@deriving equal, compare, variants, sexp_of]

  type t = ident_like_part list [@@deriving equal, compare, sexp_of]

  let string_of_ident_like_part = function
    | String value -> value
    | Hex_escape value ->
      (* We have to add this space after the value to maintain the semantic meaning
         of the ident_like token. *)
      {%string|%{value} |}
  ;;

  let to_string t = List.map t ~f:string_of_ident_like_part |> String.concat ~sep:""
end

module Context = struct
  type t =
    | Interpolation
    | Declaration of { property_name : string }
    | Block
    | Rules
    | Ambiguous of t list
  [@@deriving sexp_of]
end
