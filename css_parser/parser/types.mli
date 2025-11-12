open! Core
open Css_parser_common

type comment = string * Location.t
type 'a with_loc = 'a * Location.t
type 'a with_comments = 'a * comment list
type num_with_exponent = string * Numeric_value.t * Exponent.t option
type dimension_with_exponent = string * Numeric_value.t * Exponent.t option * string

module Interpolation_sigil = Token.Sigil

module rec Stylesheet : sig
  type t = Rule.t list with_loc [@@deriving sexp_of, equal]
end

and Rule : sig
  type t =
    | Qualified_rule of Qualified_rule.t
    | At_rule of At_rule.t
    | Style_rule of Style_rule.t
    | Comment of comment
  [@@deriving sexp_of, equal]
end

and Declaration_list : sig
  module Element : sig
    type t =
      | At_rule of At_rule.t with_loc
      | Declaration of Declaration.t with_loc
      | Qualified_rule of Qualified_rule.t with_loc
      | Comment of comment
    [@@deriving sexp_of, equal]
  end

  type t = Element.t list with_loc [@@deriving sexp_of, equal]
end

and At_rule : sig
  type block_type =
    | Declaration_list of Declaration_list.t
    | Style_block of Style_block.t
  [@@deriving sexp_of, equal]

  type t =
    { name : string
    ; prelude : Component_value.t with_loc list with_loc option
    ; block : block_type option
    ; loc : Location.t
    }
  [@@deriving sexp_of, equal]
end

and Qualified_rule : sig
  type t =
    { prelude : Component_value.t with_loc list with_loc
    ; block : Declaration_list.t
    ; loc : Location.t
    }
  [@@deriving sexp_of, equal]
end

and Style_rule : sig
  type t =
    { selectors : Selector_list.t
    ; block : Style_block.t
    ; loc : Location.t
    }
  [@@deriving sexp_of, equal]
end

and Simple_block : sig
  type t =
    | Brace of Component_value.t with_loc list with_loc
    | Bracket of Component_value.t with_loc list with_loc
    | Paren of Component_value.t with_loc list with_loc
  [@@deriving sexp_of, equal]
end

and Component_value : sig
  type t =
    | Paren_block of t with_loc list
    | Bracket_block of t with_loc list
    | Brace_block of t with_loc list
    | Percentage of num_with_exponent
    | At_keyword of string
    | Ident of string with_loc
    | String of String_token.t
    | Url of string
    | Delim of string
    | Ampersand
    | Comment of comment
    | Ocaml_code of (string * Interpolation_sigil.t) with_loc
    | Function of string with_loc * t with_loc list
    | Hash of (string * Hash_flag.t)
    | Number of num_with_exponent
    | Dimension of dimension_with_exponent
    | Whitespace of string
    | Semicolon
    | Colon
    | Comma
    | Cdo
    | Cdc
    | Bad_string of string
    | Bad_url of string
    | Right_brace
    | Right_paren
    | Right_bracket
  [@@deriving sexp_of, equal]
end

and Declaration : sig
  type t =
    { name : string with_loc with_comments
    ; value : Component_value.t with_loc list with_loc
    ; important : bool with_comments
    }
  [@@deriving sexp_of, equal]
end

and Style_block : sig
  module Block_element : sig
    type t =
      | Rule of Rule.t with_loc
      | Declaration of Declaration.t with_loc
      | Comment of comment
    [@@deriving sexp_of, equal]
  end

  type t = Block_element.t list with_loc [@@deriving sexp_of, equal]
end

and Selector : sig
  type t =
    | Id of (string * Hash_flag.t)
    | Class of string
    | Attribute of Component_value.t with_loc list with_loc
    | Pseudoclass of Pseudoclass_element_selector.t
    | Pseudoelement of Pseudoclass_element_selector.t
    | Type of (string * string option)
    | Ampersand
  [@@deriving sexp_of, equal]
end

and Pseudoclass_element_selector : sig
  type t =
    | Ident of string
    | Function of (string with_loc * Component_value.t with_loc list)
    | Function_with_selectors of (string with_loc with_comments * Selector_list.t)
  [@@deriving sexp_of, equal]
end

and Combinator : sig
  type t =
    | Descendant
    | Child
    | Subsequent_sibling
    | Next_sibling
    | Column
  [@@deriving sexp_of, compare, equal]
end

and Compound_selector : sig
  type t = Selector.t with_loc with_comments list [@@deriving sexp_of, equal]
end

and Complex_selector : sig
  type selector_or_combinator =
    | Selector of Compound_selector.t with_loc
    | Combinator of Combinator.t with_loc
  [@@deriving sexp_of]

  type t = selector_or_combinator with_comments list with_loc [@@deriving sexp_of, equal]
end

and Selector_list : sig
  type t = Complex_selector.t with_comments list with_loc [@@deriving sexp_of, equal]
end

module For_apply_style : sig
  type t = Component_value.t with_loc list with_loc [@@deriving sexp_of, equal]
end
