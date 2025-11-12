open! Core
open Css_parser_common
open Types

type comment = string * location
and 'a with_loc = 'a * location
and 'a with_comments = 'a * comment list

and hash_flag = Hash_flag.t =
  | Id
  | Unrestricted

and numeric_value = Numeric_value.t =
  | Integer
  | Float

and exponent = Exponent.t =
  { value : string
  ; capitalized : bool
  }

and string_token_quote_type = String_token.quote_type =
  | Single
  | Double

and string_token = String_token.t =
  { value : string
  ; quote_type : string_token_quote_type
  }

and num_with_exponent = string * numeric_value * exponent option
and dimension_with_exponent = string * numeric_value * exponent option * string
and location = Location.t
and stylesheet = rule list with_loc

and rule = Rule.t =
  | Qualified_rule of qualified_rule
  | At_rule of at_rule
  | Style_rule of style_rule
  | Comment of comment

and declaration_list = declaration_list_element list with_loc

and declaration_list_element = Declaration_list.Element.t =
  | At_rule of at_rule with_loc
  | Declaration of declaration with_loc
  | Qualified_rule of qualified_rule with_loc
  | Comment of comment

and at_rule_block_type = At_rule.block_type =
  | Declaration_list of declaration_list
  | Style_block of style_block

and at_rule = At_rule.t =
  { name : string
  ; prelude : component_value with_loc list with_loc option
  ; block : at_rule_block_type option
  ; loc : location
  }

and qualified_rule = Qualified_rule.t =
  { prelude : component_value with_loc list with_loc
  ; block : declaration_list
  ; loc : location
  }

and style_rule = Style_rule.t =
  { selectors : selector_list
  ; block : style_block
  ; loc : location
  }

and simple_block = Simple_block.t =
  | Brace of component_value with_loc list with_loc
  | Bracket of component_value with_loc list with_loc
  | Paren of component_value with_loc list with_loc

and component_value = Component_value.t =
  | Paren_block of component_value with_loc list
  | Bracket_block of component_value with_loc list
  | Brace_block of component_value with_loc list
  | Percentage of num_with_exponent
  | At_keyword of string
  | Ident of string with_loc
  | String of string_token
  | Url of string
  | Delim of string
  | Ampersand
  | Comment of comment
  | Ocaml_code of (string * interpolation_sigil) with_loc
  | Function of string with_loc * component_value with_loc list
  | Hash of (string * hash_flag)
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

and declaration = Declaration.t =
  { name : string with_loc with_comments
  ; value : component_value with_loc list with_loc
  ; important : bool with_comments
  }

and style_block = style_block_block_element list with_loc

and style_block_block_element = Style_block.Block_element.t =
  | Rule of rule with_loc
  | Declaration of declaration with_loc
  | Comment of comment

and selector_with_loc = selector with_loc

and selector = Selector.t =
  | Id of (string * hash_flag)
  | Class of string
  | Attribute of component_value with_loc list with_loc
  | Pseudoclass of pseudoclass_element_selector
  | Pseudoelement of pseudoclass_element_selector
  | Type of (string * string option)
  | Ampersand

and pseudoclass_element_selector = Pseudoclass_element_selector.t =
  | Ident of string
  | Function of (string with_loc * component_value with_loc list)
  | Function_with_selectors of (string with_loc with_comments * selector_list)

and combinator = Combinator.t =
  | Descendant
  | Child
  | Subsequent_sibling
  | Next_sibling
  | Column

and compound_selector = selector_with_loc with_comments list

and selector_or_combinator = Complex_selector.selector_or_combinator =
  | Selector of compound_selector with_loc
  | Combinator of combinator with_loc

and complex_selector = selector_or_combinator with_comments list with_loc
and for_apply_style = component_value with_loc list with_loc
and selector_list = complex_selector with_comments list with_loc

and interpolation_sigil = Interpolation_sigil.t =
  | Percent
  | Hash
[@@deriving traverse_map, traverse_iter]

class map' =
  object
    inherit map
    method bool : bool -> bool = Fn.id
    method list : 'a. ('a -> 'a) -> 'a list -> 'a list = fun f -> List.map ~f
    method string : string -> string = Fn.id
    method location__t : Location.t -> Location.t = Fn.id
    method option : 'a. ('a -> 'a) -> 'a option -> 'a option = fun f -> Option.map ~f
  end

class map = map'

class iter' =
  object
    inherit iter
    method bool : bool -> unit = Fn.const ()
    method list : 'a. ('a -> unit) -> 'a list -> unit = fun f -> List.iter ~f
    method string : string -> unit = Fn.const ()
    method location__t : Location.t -> unit = Fn.const ()
    method option : 'a. ('a -> unit) -> 'a option -> unit = fun f -> Option.iter ~f
  end

class iter = iter'
