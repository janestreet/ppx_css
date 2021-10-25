open! Core

type 'a with_loc = 'a * Location.t

module rec Dimension : sig
  type t =
    | Length
    | Angle
    | Time
    | Frequency
  [@@deriving sexp_of]
end

and Component_value : sig
  type t =
    | Paren_block of t with_loc list
    | Bracket_block of t with_loc list
    | Percentage of string
    | Ident of string
    | String of string
    | Uri of string
    | Operator of string
    | Delim of string
    | Function of string with_loc * t with_loc list with_loc
    | Hash of string
    | Number of string
    | Unicode_range of string
    | Float_dimension of (string * string * Dimension.t)
    | Dimension of (string * string)
  [@@deriving sexp_of]
end

and Brace_block : sig
  type t =
    | Empty
    | Declaration_list of Declaration_list.t
    | Stylesheet of Stylesheet.t
  [@@deriving sexp_of]
end

and At_rule : sig
  type t =
    { name : string with_loc
    ; prelude : Component_value.t with_loc list with_loc
    ; block : Brace_block.t
    ; loc : Location.t
    }
  [@@deriving sexp_of]
end

and Declaration : sig
  type t =
    { name : string with_loc
    ; value : Component_value.t with_loc list with_loc
    ; important : bool with_loc
    ; loc : Location.t
    }
  [@@deriving sexp_of]
end

and Declaration_list : sig
  type kind =
    | Declaration of Declaration.t
    | At_rule of At_rule.t
  [@@deriving sexp_of]

  type t = kind list with_loc [@@deriving sexp_of]
end

and Style_rule : sig
  type t =
    { prelude : Component_value.t with_loc list with_loc
    ; block : Declaration_list.t
    ; loc : Location.t
    }
  [@@deriving sexp_of]
end

and Rule : sig
  type t =
    | Style_rule of Style_rule.t
    | At_rule of At_rule.t
  [@@deriving sexp_of]
end

and Stylesheet : sig
  type t = Rule.t list with_loc [@@deriving sexp_of]

  val to_string_hum : t -> string
  val to_string_minified : t -> string
  val of_string : ?pos:Source_code_position.t -> string -> t
end
