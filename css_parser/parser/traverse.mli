open! Core
open Css_parser_common
open Types

class map : object
  method bool : bool -> bool
  method list : 'a. ('a -> 'a) -> 'a list -> 'a list
  method string : string -> string
  method option : 'a. ('a -> 'a) -> 'a option -> 'a option
  method string_token : String_token.t -> String_token.t
  method numeric_value : Numeric_value.t -> Numeric_value.t
  method location : Location.t -> Location.t
  method hash_flag : Hash_flag.t -> Hash_flag.t
  method exponent : Exponent.t -> Exponent.t
  method with_loc : 'a. ('a -> 'a) -> 'a with_loc -> 'a with_loc
  method num_with_exponent : num_with_exponent -> num_with_exponent
  method dimension_with_exponent : dimension_with_exponent -> dimension_with_exponent
  method comment : comment -> comment
  method with_comments : 'a. ('a -> 'a) -> 'a with_comments -> 'a with_comments
  method numeric_value : Numeric_value.t -> Numeric_value.t
  method location : Location.t -> Location.t
  method exponent : Exponent.t -> Exponent.t
  method compound_selector : Compound_selector.t -> Compound_selector.t
  method combinator : Combinator.t -> Combinator.t
  method complex_selector : Complex_selector.t -> Complex_selector.t
  method selector_with_loc : Selector.t with_loc -> Selector.t with_loc
  method selector : Selector.t -> Selector.t
  method component_value : Component_value.t -> Component_value.t
  method selector_list : Selector_list.t -> Selector_list.t
  method rule : Rule.t -> Rule.t
  method declaration : Declaration.t -> Declaration.t
  method declaration_list : Declaration_list.t -> Declaration_list.t
  method at_rule : At_rule.t -> At_rule.t
  method style_rule : Style_rule.t -> Style_rule.t
  method qualified_rule : Qualified_rule.t -> Qualified_rule.t
  method string_token_quote_type : String_token.quote_type -> String_token.quote_type
  method location__t : Location.t -> Location.t
  method simple_block : Simple_block.t -> Simple_block.t
  method at_rule_block_type : At_rule.block_type -> At_rule.block_type

  method selector_or_combinator :
    Complex_selector.selector_or_combinator -> Complex_selector.selector_or_combinator

  method style_block_block_element :
    Style_block.Block_element.t -> Style_block.Block_element.t

  method style_block : Style_block.t -> Style_block.t

  method declaration_list_element :
    Declaration_list.Element.t -> Declaration_list.Element.t

  method pseudoclass_element_selector :
    Pseudoclass_element_selector.t -> Pseudoclass_element_selector.t

  method stylesheet : Stylesheet.t -> Stylesheet.t
  method for_apply_style : For_apply_style.t -> For_apply_style.t
end

class iter : object
  method bool : bool -> unit
  method list : 'a. ('a -> unit) -> 'a list -> unit
  method string : string -> unit
  method option : 'a. ('a -> unit) -> 'a option -> unit
  method string_token : String_token.t -> unit
  method numeric_value : Numeric_value.t -> unit
  method location : Location.t -> unit
  method hash_flag : Hash_flag.t -> unit
  method exponent : Exponent.t -> unit
  method with_loc : 'a. ('a -> unit) -> 'a with_loc -> unit
  method num_with_exponent : num_with_exponent -> unit
  method dimension_with_exponent : dimension_with_exponent -> unit
  method comment : comment -> unit
  method with_comments : 'a. ('a -> unit) -> 'a with_comments -> unit
  method numeric_value : Numeric_value.t -> unit
  method location : Location.t -> unit
  method exponent : Exponent.t -> unit
  method compound_selector : Compound_selector.t -> unit
  method combinator : Combinator.t -> unit
  method complex_selector : Complex_selector.t -> unit
  method selector_with_loc : Selector.t with_loc -> unit
  method selector : Selector.t -> unit
  method component_value : Component_value.t -> unit
  method selector_list : Selector_list.t -> unit
  method rule : Rule.t -> unit
  method declaration : Declaration.t -> unit
  method declaration_list : Declaration_list.t -> unit
  method at_rule : At_rule.t -> unit
  method style_rule : Style_rule.t -> unit
  method qualified_rule : Qualified_rule.t -> unit
  method string_token_quote_type : String_token.quote_type -> unit
  method location__t : Location.t -> unit
  method simple_block : Simple_block.t -> unit
  method at_rule_block_type : At_rule.block_type -> unit
  method selector_or_combinator : Complex_selector.selector_or_combinator -> unit
  method style_block_block_element : Style_block.Block_element.t -> unit
  method style_block : Style_block.t -> unit
  method declaration_list_element : Declaration_list.Element.t -> unit
  method pseudoclass_element_selector : Pseudoclass_element_selector.t -> unit
  method stylesheet : Stylesheet.t -> unit
  method for_apply_style : For_apply_style.t -> unit
end
