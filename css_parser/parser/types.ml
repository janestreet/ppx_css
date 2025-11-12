open! Core
open Css_parser_common

type 'a with_loc = 'a * (Location.t[@equal.ignore]) [@@deriving equal]

let sexp_of_with_loc sexp_of_a (a, _) = sexp_of_a a

type comment = string * (Location.t[@equal.ignore]) [@@deriving equal]
and 'a with_comments = 'a * comment list [@@deriving equal]

let sexp_of_comment (comment, _) = [%sexp (comment : string)]

let sexp_of_with_comments sexp_of_a (a, comments) =
  let sexp_a = sexp_of_a a in
  match comments with
  | [] -> sexp_of_a a
  | comments -> [%sexp (sexp_a : Sexp.t), Comments (comments : comment list)]
;;

type num_with_exponent = string * Numeric_value.t * Exponent.t option
[@@deriving sexp_of, equal]

and dimension_with_exponent = string * Numeric_value.t * Exponent.t option * string
[@@deriving sexp_of, equal]

and numeric_value = Numeric_value.t
and exponent = Exponent.t

module Interpolation_sigil = Token.Sigil

module rec Stylesheet : sig
  type t = Rule.t list with_loc [@@deriving sexp_of, equal]
end = struct
  type t = Rule.t list with_loc

  let equal (a, _) (b, _) = List.equal Rule.equal a b
  let sexp_of_t t = [%sexp (t : Rule.t list with_loc)]
end

and Rule : sig
  type t =
    | Qualified_rule of Qualified_rule.t
    | At_rule of At_rule.t
    | Style_rule of Style_rule.t
    | Comment of comment
  [@@deriving sexp_of, equal]
end = struct
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
end = struct
  module Element = struct
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
end = struct
  type block_type =
    | Declaration_list of Declaration_list.t
    | Style_block of Style_block.t
  [@@deriving sexp_of, equal]

  and t =
    { name : string
    ; prelude : Component_value.t with_loc list with_loc option
    ; block : block_type option
    ; loc : (Location.t[@equal.ignore])
    }
  [@@deriving equal]

  let sexp_of_t { name; prelude; block; loc = _ } =
    [%sexp
      { name : string
      ; prelude : Component_value.t with_loc list with_loc option
      ; block : block_type option
      }]
  ;;
end

and Qualified_rule : sig
  type t =
    { prelude : Component_value.t with_loc list with_loc
    ; block : Declaration_list.t
    ; loc : Location.t
    }
  [@@deriving sexp_of, equal]
end = struct
  type t =
    { prelude : Component_value.t with_loc list with_loc
    ; block : Declaration_list.t
    ; loc : (Location.t[@equal.ignore])
    }
  [@@deriving equal]

  let sexp_of_t { prelude; block; loc = _ } =
    [%sexp
      { prelude : Component_value.t with_loc list with_loc; block : Declaration_list.t }]
  ;;
end

and Style_rule : sig
  type t =
    { selectors : Selector_list.t
    ; block : Style_block.t
    ; loc : Location.t
    }
  [@@deriving sexp_of, equal]
end = struct
  type t =
    { selectors : Selector_list.t
    ; block : Style_block.t
    ; loc : (Location.t[@equal.ignore])
    }
  [@@deriving equal]

  let sexp_of_t { selectors; block; loc = _ } =
    [%sexp { selectors : Selector_list.t; block : Style_block.t }]
  ;;
end

and Simple_block : sig
  type t =
    | Brace of Component_value.t with_loc list with_loc
    | Bracket of Component_value.t with_loc list with_loc
    | Paren of Component_value.t with_loc list with_loc
  [@@deriving sexp_of, equal]
end = struct
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
end = struct
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
    (* Have to add an override here because whitespace values are not significant in CSS,
       we are just allowing the component values to contain the actual whitespace for 
       printing purposes *)
    | Whitespace of (string[@equal.ignore])
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
  [@@deriving equal]

  let sexp_of_t = function
    | Paren_block t_list -> [%sexp Paren_block (t_list : Component_value.t with_loc list)]
    | Bracket_block t_list ->
      [%sexp Bracket_block (t_list : Component_value.t with_loc list)]
    | Brace_block t_list -> [%sexp Brace_block (t_list : Component_value.t with_loc list)]
    | Percentage percentage -> [%sexp Percentage (percentage : num_with_exponent)]
    | Ident ident -> [%sexp Ident (ident : string with_loc)]
    | String str_ -> [%sexp String (str_ : String_token.t)]
    | Url url -> [%sexp Url (url : string)]
    | Delim delim -> [%sexp Delim (delim : string)]
    | Ampersand -> [%sexp Ampersand]
    | Comment comment -> [%sexp Comment (comment : comment)]
    | Ocaml_code code ->
      [%sexp Ocaml_code (code : (string * Interpolation_sigil.t) with_loc)]
    | Function (fn_, args) ->
      [%sexp Function ((fn_, args) : string with_loc * Component_value.t with_loc list)]
    | Hash (hash, kind) -> [%sexp Hash ((hash, kind) : string * Hash_flag.t)]
    | Number num -> [%sexp Number (num : num_with_exponent)]
    | Dimension num -> [%sexp Dimension (num : dimension_with_exponent)]
    | Whitespace _ -> [%sexp Whitespace]
    | Semicolon -> [%sexp Semicolon]
    | Colon -> [%sexp Colon]
    | Comma -> [%sexp Comma]
    | Cdo -> [%sexp Cdo]
    | Cdc -> [%sexp Cdc]
    | At_keyword name -> [%sexp At_keyword (name : string)]
    | Bad_string str -> [%sexp Bad_string (str : string)]
    | Bad_url url -> [%sexp Bad_url (url : string)]
    | Right_brace -> [%sexp Right_brace]
    | Right_paren -> [%sexp Right_paren]
    | Right_bracket -> [%sexp Right_bracket]
  ;;
end

and Declaration : sig
  type t =
    { name : string with_loc with_comments
    ; value : Component_value.t with_loc list with_loc
    ; important : bool with_comments
    }
  [@@deriving sexp_of, equal]
end = struct
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
end = struct
  module Block_element = struct
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
end = struct
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
end = struct
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
end = struct
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
end = struct
  type t = Selector.t with_loc with_comments list [@@deriving sexp_of, equal]
end

and Complex_selector : sig
  type selector_or_combinator =
    | Selector of Compound_selector.t with_loc
    | Combinator of Combinator.t with_loc
  [@@deriving sexp_of, equal]

  type t = selector_or_combinator with_comments list with_loc [@@deriving sexp_of, equal]
end = struct
  type selector_or_combinator =
    | Selector of Compound_selector.t with_loc
    | Combinator of Combinator.t with_loc
  [@@deriving sexp_of, equal]

  and t = selector_or_combinator with_comments list with_loc [@@deriving equal]

  let sexp_of_t (t, _) =
    [%sexp Complex_selector (t : selector_or_combinator with_comments list)]
  ;;
end

and Selector_list : sig
  type t = Complex_selector.t with_comments list with_loc [@@deriving sexp_of, equal]
end = struct
  type t = Complex_selector.t with_comments list with_loc [@@deriving equal]

  let sexp_of_t (t, _) = [%sexp Selector_list (t : Complex_selector.t with_comments list)]
end

module For_apply_style = struct
  type t = Component_value.t with_loc list with_loc [@@deriving sexp_of, equal]
end
