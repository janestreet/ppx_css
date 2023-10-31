%{

(* Workaround for this dune bug: https://github.com/ocaml/dune/issues/2450 *)
module Css = struct end

open Types

%}

%token EOF
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token COLON
%token DOT
%token AMPERSAND
(* Whitespaces are detected only in selectors, before ":", ".", and "#", to
 * disambiguate between "p :first-child" and "p:first-child", these
 * whitespaces are replaced with "*" *)
%token WHITESPACE
%token SEMI_COLON
%token PERCENTAGE
%token IMPORTANT
%token <string> IDENT
%token <string> STRING
%token <string> URI
%token <string> OPERATOR
%token <string> DELIM
%token <string> NESTED_AT_RULE
%token <string> AT_RULE_WITHOUT_BODY
%token <string> AT_RULE
%token <string> FUNCTION
%token <string> HASH
%token <string> NUMBER
%token <string> UNICODE_RANGE
%token <string * string * Types.dimension> FLOAT_DIMENSION
%token <string * string> DIMENSION

%start <Types.Stylesheet.t> stylesheet
%start <Types.Declaration_list.t> declaration_list

%%

stylesheet:
  s = stylesheet_without_eof; EOF { s }
  ;

stylesheet_without_eof:
  rs = list(rule) { (rs, Lex_buffer.make_loc_and_fix $startpos $endpos) }
  ;

declaration_list:
  ds = declarations_with_loc; EOF { ds }
  ;

rule:
  | r = at_rule { Rule.At_rule r }
  | r = style_rule { Rule.Style_rule r }
  ;

at_rule:
  | name = AT_RULE_WITHOUT_BODY; xs = prelude_with_loc; SEMI_COLON {
      { At_rule.name = (name, Lex_buffer.make_loc_and_fix $startpos(name) $endpos(name));
        prelude = xs;
        block = Brace_block.Empty;
        loc = Lex_buffer.make_loc_and_fix $startpos $endpos;
      }
    }
  | name = NESTED_AT_RULE; xs = prelude_with_loc; LEFT_BRACE; s = stylesheet_without_eof; RIGHT_BRACE {
      { At_rule.name = (name, Lex_buffer.make_loc_and_fix $startpos(name) $endpos(name));
        prelude = xs;
        block = Brace_block.Stylesheet s;
        loc = Lex_buffer.make_loc_and_fix $startpos $endpos;
      }
    }
  | name = AT_RULE; xs = prelude_with_loc; LEFT_BRACE; ds = declarations_with_loc; RIGHT_BRACE {
      { At_rule.name = (name, Lex_buffer.make_loc_and_fix $startpos(name) $endpos(name));
        prelude = xs;
        block = Brace_block.Declaration_list ds;
        loc = Lex_buffer.make_loc_and_fix $startpos $endpos;
      }
    }
  ;

style_rule:
  | xs = prelude_with_loc; LEFT_BRACE; RIGHT_BRACE {
      { Style_rule.prelude = xs;
        block = [], Location.none;
        loc = Lex_buffer.make_loc_and_fix $startpos $endpos;
      }
    }
  | xs = prelude_with_loc; LEFT_BRACE; ds = declarations_with_loc; RIGHT_BRACE {
      { Style_rule.prelude = xs;
        block = ds;
        loc = Lex_buffer.make_loc_and_fix $startpos $endpos;
      }
    }
  ;

prelude_with_loc:
  xs = prelude { (xs, Lex_buffer.make_loc_and_fix $startpos $endpos) }
  ;

prelude:
  xs = list(component_value_with_loc_in_prelude) { xs }
  ;

declarations_with_loc:
  | ds = declarations { (ds, Lex_buffer.make_loc_and_fix ~loc_ghost:true $startpos $endpos) }
  ;

declarations:
  | ds = declarations_without_ending_semi_colon_but_needs_semicolon_to_continue { List.rev ds }
  | ds = declarations_without_ending_semi_colon_but_needs_semicolon_to_continue; SEMI_COLON { List.rev ds }
  | ds = declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue { List.rev ds}
  | ds = declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue; SEMI_COLON { List.rev ds}
  ;

declarations_without_ending_semi_colon_but_needs_semicolon_to_continue:
  | d = declaration_or_at_rule { [d] }
  | ds = declarations_without_ending_semi_colon_but_needs_semicolon_to_continue; SEMI_COLON; d = declaration_or_at_rule { d :: ds }
  | ds = declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue; d = declaration_or_at_rule { d :: ds }
  | ds = declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue; SEMI_COLON; d = declaration_or_at_rule { d :: ds }
  ;

declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue:
  | r = nested_rule { [ r ] }
  | ds = declarations_without_ending_semi_colon_but_needs_semicolon_to_continue; SEMI_COLON; r = nested_rule { r :: ds }
  | ds = declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue; r = nested_rule { r :: ds }
  ;

nested_rule: 
  | r = nested_style_rule { Declaration_list.Style_rule r }  
  ;

declaration_or_at_rule:
  | d = declaration { Declaration_list.Declaration d }
  | r = at_rule { Declaration_list.At_rule r }
  | r = nested_rule { r }
  ;

declaration:
  n = IDENT; option(WHITESPACE); COLON; v = list(component_value_with_loc); i = boption(IMPORTANT) {
    { Declaration.name = (n, Lex_buffer.make_loc_and_fix $startpos(n) $endpos(n));
      value = (v, Lex_buffer.make_loc_and_fix $startpos(v) $endpos(v));
      important = (i, Lex_buffer.make_loc_and_fix $startpos(i) $endpos(i));
      loc = Lex_buffer.make_loc_and_fix $startpos $endpos;
    }
  }
  ;

paren_block:
  LEFT_PAREN; xs = list(component_value_with_loc); RIGHT_PAREN { xs }
  ;

bracket_block:
  LEFT_BRACKET; xs = list(component_value_with_loc); RIGHT_BRACKET { xs }
  ;

component_value_with_loc:
  | c = component_value { (c, Lex_buffer.make_loc_and_fix $startpos $endpos) }

component_value:
  | b = paren_block { Component_value.Paren_block b }
  | b = bracket_block { Component_value.Bracket_block b }
  | n = NUMBER; PERCENTAGE { Component_value.Percentage n }
  | i = IDENT { Component_value.Ident i }
  | s = STRING { Component_value.String s }
  | u = URI { Component_value.Uri u }
  | o = OPERATOR { Component_value.Operator o }
  | d = DELIM { Component_value.Delim d }
  | option(WHITESPACE); COLON { Component_value.Delim ":" }
  | option(WHITESPACE); DOT { Component_value.Delim "." }
  | f = FUNCTION; xs = list(component_value_with_loc); RIGHT_PAREN {
      Component_value.Function ((f, Lex_buffer.make_loc_and_fix $startpos(f) $endpos(f)),
                                (xs, Lex_buffer.make_loc_and_fix $startpos(xs) $endpos(xs)))
    }
  | option(WHITESPACE); h = HASH { Component_value.Hash h }
  | n = NUMBER { Component_value.Number n }
  | r = UNICODE_RANGE { Component_value.Unicode_range r }
  | d = FLOAT_DIMENSION { Component_value.Float_dimension d }
  | d = DIMENSION { Component_value.Dimension d }
  ;


component_value_with_loc_in_prelude:
  | c = component_value_in_prelude { (c, Lex_buffer.make_loc_and_fix $startpos $endpos) }

component_value_in_prelude:
  (* NOTE: Importantly, this does not have &. *)
  | b = paren_block { Component_value.Paren_block b }
  | b = bracket_block { Component_value.Bracket_block b }
  | c = common_component_values_in_prelude { c }
  | f = FUNCTION; xs = list(component_value_with_loc); RIGHT_PAREN {
      Component_value.Function ((f, Lex_buffer.make_loc_and_fix $startpos(f) $endpos(f)),
                                (xs, Lex_buffer.make_loc_and_fix $startpos(xs) $endpos(xs))) }
  | i = IDENT { Component_value.Ident i }
  ;

nested_style_rule:
  | xs = nested_prelude_with_loc; LEFT_BRACE; RIGHT_BRACE {
      { Style_rule.prelude = xs;
        block = [], Location.none;
        loc = Lex_buffer.make_loc_and_fix $startpos $endpos;
      }
    }
  | xs = nested_prelude_with_loc; LEFT_BRACE; ds = declarations_with_loc; RIGHT_BRACE {
      { Style_rule.prelude = xs;
        block = ds;
        loc = Lex_buffer.make_loc_and_fix $startpos $endpos;
      }
    }
  ;

nested_prelude_with_loc:
  xs = nested_prelude { (xs, Lex_buffer.make_loc_and_fix $startpos $endpos) }
  ;

nested_prelude:
  | hd = starting_component_value_with_loc_in_prelude; xs = list(component_value_with_loc_in_nested_prelude) { hd ::  xs }
  ;

component_value_with_loc_in_nested_prelude:
  | c = component_value_in_nested_prelude { (c, Lex_buffer.make_loc_and_fix $startpos $endpos) }
  ;

starting_component_value_with_loc_in_prelude:
  | c = starting_component_value_in_nested_prelude { (c, Lex_buffer.make_loc_and_fix $startpos $endpos) }
  ;


nested_paren_block:
  LEFT_PAREN; xs = list(component_value_with_loc_in_nested_prelude); RIGHT_PAREN { xs }
  ;

nested_bracket_block:
  LEFT_BRACKET; xs = list(component_value_with_loc_in_nested_prelude); RIGHT_BRACKET { xs }
  ;

common_component_values_in_prelude:
  | n = NUMBER; PERCENTAGE { Component_value.Percentage n }
  | s = STRING { Component_value.String s }
  | o = OPERATOR { Component_value.Operator o }
  | d = DELIM { Component_value.Delim d }
  | WHITESPACE { Component_value.Delim "*" }
  | COLON { Component_value.Delim ":" }
  | DOT { Component_value.Delim "." }
  | h = HASH { Component_value.Hash h }
  | n = NUMBER { Component_value.Number n }
  | r = UNICODE_RANGE { Component_value.Unicode_range r }
  | d = FLOAT_DIMENSION { Component_value.Float_dimension d }
  | d = DIMENSION { Component_value.Dimension d }
  ;

common_nested_component_value:
  | c = common_component_values_in_prelude { c }
  | b = nested_paren_block { Component_value.Paren_block b }
  | b = nested_bracket_block { Component_value.Bracket_block b }
  | AMPERSAND { Component_value.Ampersand }
  ;

starting_component_value_in_nested_prelude:
  (* NOTE: According to [1]. A selector starting with an identifier is 
     disallowed to prevent ambiguity. This is the reason for this different
     "starting" clause.

     [1] https://www.w3.org/TR/css-nesting-1/
  *)
  | c = common_nested_component_value { c }
  ;


component_value_in_nested_prelude_with_loc:
  | c = component_value_in_nested_prelude { (c, Lex_buffer.make_loc_and_fix $startpos $endpos) }

component_value_in_nested_prelude:
  (* Importantly, this one allows for nested identifiers and ampersands. *)
  | c = common_nested_component_value { c }
  | i = IDENT { Component_value.Ident i }
  | f = FUNCTION; xs = list(component_value_in_nested_prelude_with_loc); RIGHT_PAREN {
      Component_value.Function ((f, Lex_buffer.make_loc_and_fix $startpos(f) $endpos(f)),
                                (xs, Lex_buffer.make_loc_and_fix $startpos(xs) $endpos(xs))) }
  ;


