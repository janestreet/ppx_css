open! Core
open Css_parser

type 'a with_loc = 'a * Location.t

let sexp_of_with_loc sexp_of_a (a, _) = sexp_of_a a

module Sexpers = struct
  (* All the sexp-of functions are written in a single let-rec 
     so that I can avoid having to deal with mutually recursive modules. *)

  let rec sexp_of_dimension : Types.dimension -> Sexp.t = function
    | Length -> [%sexp Length]
    | Angle -> [%sexp Angle]
    | Time -> [%sexp Time]
    | Frequency -> [%sexp Frequency]

  and sexp_of_component_value : Types.Component_value.t -> Sexp.t = function
    | Paren_block t_list -> [%sexp Paren_block (t_list : component_value with_loc list)]
    | Bracket_block t_list ->
      [%sexp Bracket_block (t_list : component_value with_loc list)]
    | Percentage s -> [%sexp Percentage (s : string)]
    | Ident s -> [%sexp Ident (s : string)]
    | String s -> [%sexp String (s : string)]
    | Uri s -> [%sexp Uri (s : string)]
    | Operator s -> [%sexp Operator (s : string)]
    | Delim s -> [%sexp Delim (s : string)]
    | Function (s, t_list) ->
      [%sexp
        Function (s, t_list : string with_loc * component_value with_loc list with_loc)]
    | Hash s -> [%sexp Hash (s : string)]
    | Number s -> [%sexp Number (s : string)]
    | Unicode_range s -> [%sexp Unicode_range (s : string)]
    | Float_dimension (s1, s2, dim) ->
      [%sexp Float_dimension (s1, s2, dim : string * string * dimension)]
    | Dimension (s1, s2) -> [%sexp Dimension (s1, s2 : string * string)]

  and sexp_of_declaration : Types.Declaration.t -> Sexp.t = function
    | { name; value; important; loc = _ } ->
      [%sexp
        { name : string with_loc
        ; value : component_value with_loc list with_loc
        ; important : bool with_loc
        }]

  and sexp_of_brace_block : Types.Brace_block.t -> Sexp.t = function
    | Empty -> [%sexp Empty]
    | Declaration_list d -> [%sexp Declaration_list (d : declaration_list)]
    | Stylesheet s -> [%sexp Stylesheet (s : stylesheet)]

  and sexp_of_at_rule : Types.At_rule.t -> Sexp.t = function
    | { name; prelude; block; loc = _ } ->
      [%sexp
        { name : string with_loc
        ; prelude : component_value with_loc list with_loc
        ; block : brace_block
        }]

  and sexp_of_declaration_list_kind : Types.Declaration_list.kind -> Sexp.t = function
    | Declaration d -> [%sexp Declaration (d : declaration)]
    | At_rule a -> [%sexp At_rule (a : at_rule)]

  and sexp_of_declaration_list : Types.Declaration_list.t -> Sexp.t = function
    | d -> [%sexp (d : declaration_list_kind list with_loc)]

  and sexp_of_style_rule : Types.Style_rule.t -> Sexp.t = function
    | { prelude; block; loc = _ } ->
      [%sexp
        { prelude : component_value with_loc list with_loc; block : declaration_list }]

  and sexp_of_rule : Types.Rule.t -> Sexp.t = function
    | Style_rule s -> [%sexp Style_rule (s : style_rule)]
    | At_rule s -> [%sexp At_rule (s : at_rule)]

  and sexp_of_stylesheet : Types.Stylesheet.t -> Sexp.t = function
    | s -> [%sexp (s : rule list with_loc)]
  ;;
end

module Dimension = struct
  type t = Types.dimension =
    | Length
    | Angle
    | Time
    | Frequency

  let sexp_of_t = Sexpers.sexp_of_dimension
end

module Component_value = struct
  type t = Types.Component_value.t =
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

  let sexp_of_t = Sexpers.sexp_of_component_value
end

module Declaration = struct
  type t = Types.Declaration.t =
    { name : string with_loc
    ; value : Component_value.t with_loc list with_loc
    ; important : bool with_loc
    ; loc : Location.t
    }

  let sexp_of_t = Sexpers.sexp_of_declaration
end

module Brace_block = struct
  type t = Types.Brace_block.t =
    | Empty
    | Declaration_list of Types.Declaration_list.t
    | Stylesheet of Types.Stylesheet.t

  let sexp_of_t = Sexpers.sexp_of_brace_block
end

module At_rule = struct
  type t = Types.At_rule.t =
    { name : string with_loc
    ; prelude : Component_value.t with_loc list with_loc
    ; block : Brace_block.t
    ; loc : Location.t
    }

  let sexp_of_t = Sexpers.sexp_of_at_rule
end

module Declaration_list = struct
  type kind = Types.Declaration_list.kind =
    | Declaration of Declaration.t
    | At_rule of At_rule.t

  and t = kind list with_loc

  let sexp_of_kind = Sexpers.sexp_of_declaration_list_kind
  let sexp_of_t = Sexpers.sexp_of_declaration_list
end

module Style_rule = struct
  type t = Types.Style_rule.t =
    { prelude : Component_value.t with_loc list with_loc
    ; block : Declaration_list.t
    ; loc : Location.t
    }

  let sexp_of_t = Sexpers.sexp_of_style_rule
end

module Rule = struct
  type t = Types.Rule.t =
    | Style_rule of Style_rule.t
    | At_rule of At_rule.t

  let sexp_of_t = Sexpers.sexp_of_rule
end

module Stylesheet = struct
  type t = Rule.t list with_loc

  let to_string which t =
    let buffer = Buffer.create 64 in
    let formatter = Format.formatter_of_buffer buffer in
    Css_printer.Print.(css which) formatter t;
    Buffer.contents buffer
  ;;

  let to_string_hum = to_string Css_printer.Print.pretty_printer
  let to_string_minified = to_string Css_printer.Print.minify_printer

  let of_string ?pos s =
    (* the parser produces different output depending on if there is 
       a leading space or not.  They're equivalent semantically, but 
       I can add this to remove ambiguity. *)
    let s = " " ^ s in
    Css_parser.Parser.parse_stylesheet ?pos s
  ;;

  let sexp_of_t = Sexpers.sexp_of_stylesheet
end
