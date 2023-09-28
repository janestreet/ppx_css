(* Copyright © 2021 <Sébastien Dailly>

   Permission is hereby granted, free of charge, to any person obtaining a copy of
   this software and associated documentation files (the “Software”), to deal in
   the Software without restriction, including without limitation the rights to
   use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is furnished to do
    so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.*)

open StdLabels
open Css_parser.Types

(* Types helpers *)

type ('a, 'b) printer = (Format.formatter -> 'a -> unit) -> 'a -> 'b
type ('a, 'b) format_printer = (('a, 'b) printer, Format.formatter, unit) format

type style_rule_format =
  ( Component_value.t with_loc list
  , (Declaration_list.kind list, unit) printer )
  format_printer

type declaration_format = (Declaration_list.kind list, unit) format_printer
type stylesheet_format = (Rule.t list, unit) format_printer

type at_rule_format =
  ( string -> (Component_value.t with_loc list, (Brace_block.t, unit) printer) printer
  , Format.formatter
  , unit )
  format

type important_format = (unit, Format.formatter, unit) format

type template =
  { main_css : stylesheet_format
  ; style_rule : style_rule_format
  ; declaration : declaration_format
  ; stylesheet : stylesheet_format
  ; at_rule : at_rule_format
  ; important : important_format
  }

let pp_sep_column formater () = Format.fprintf formater ";@,"

let pretty_printer =
  { main_css = Stdlib.format_of_string "@[<v>%a@]@."
  ; style_rule = Stdlib.format_of_string "%a {@;<1 1>@[<v>%a@]@,}@,"
  ; declaration = Stdlib.format_of_string "{@;<1 1>@[<v>%a@]@,}@,"
  ; stylesheet = Stdlib.format_of_string "{@;<1 1>@[<v>%a@]@,}@,"
  ; at_rule = Stdlib.format_of_string "@%s%a%a@,"
  ; important = Stdlib.format_of_string " !important"
  }
;;

let minify_printer =
  { main_css = Stdlib.format_of_string "@[<h>%a@]@."
  ; style_rule = Stdlib.format_of_string "%a{%a}"
  ; declaration = Stdlib.format_of_string " {%a}"
  ; stylesheet = Stdlib.format_of_string " {%a}"
  ; at_rule = Stdlib.format_of_string "@%s%a%a"
  ; important = Stdlib.format_of_string "!important"
  }
;;

type sep_printer = Format.formatter -> bool -> unit

let print_space : sep_printer =
  fun formatter -> function
  | true -> Format.fprintf formatter " "
  | false -> ()
;;

let print_coma : sep_printer =
  fun formatter -> function
  | true -> Format.fprintf formatter ","
  | false -> ()
;;

let rec print_block
  :  ?printer:sep_printer -> ?add_space:bool -> bool -> Format.formatter
  -> Component_value.t with_loc list -> unit
  =
  fun ?(printer = print_space) ?(add_space = false) is_selector formatter values ->
  let _ =
    List.fold_left values ~init:add_space ~f:(fun v ->
      print_component printer is_selector v formatter)
  in
  ()

(** Print a component list.

    [print_component sep is_selector]

    [sep] : The space is the common separator, but it may be a coma when the
    components are selectors.

    [is_selector] tell if we are in a selector or in declaration. Some
    operators are handled differently in selectors (hash represent id and
    should be prepend by space)

    [add_space] tell the printer that a space shall be added before the print.


*)
and print_component
  : sep_printer -> bool -> bool -> Format.formatter -> Component_value.t with_loc -> bool
  =
  fun sep_printer is_selector add_space formatter (value, _) ->
  let _ = sep_printer in
  match value with
  | Delim str ->
    (match is_selector, str with
     | true, "*" -> Format.fprintf formatter "%a*" print_space add_space
     | false, ("*" | "/" | "+" | "-") -> Format.fprintf formatter " %s " str
     | _ -> Format.fprintf formatter "%s" str);
    false
  | Uri str ->
    Format.fprintf formatter {|%aurl(%s)|} print_space add_space str;
    true
  | Ident str | Operator str | Number str ->
    Format.fprintf formatter {|%a%s|} print_space add_space str;
    true
  | String str | Unicode_range str ->
    Format.fprintf formatter {|"%a%s"|} print_space add_space str;
    true
  | Hash str ->
    (match is_selector with
     | true ->
       (* We are in a selector, the # Operator is alway attached to an
             element — * are explicited in the parser. We do not need to add
             space before *)
       Format.fprintf formatter "#%s" str
     | false -> Format.fprintf formatter "%a#%s" print_space add_space str);
    true
  | Percentage str ->
    Format.fprintf formatter "%a%s%%" print_space add_space str;
    true
  | Dimension (str, unit') ->
    Format.fprintf formatter "%a%s %s" print_space add_space str unit';
    true
  | Float_dimension (str, unit', _) ->
    Format.fprintf formatter "%a%s%s" print_space add_space str unit';
    true
  | Bracket_block elems ->
    Format.fprintf
      formatter
      "[%a]"
      (* There is no need to add a space here, as the bracket as attribute
           cannot appear detachedfrom any selector  *)
      (print_block is_selector)
      elems;
    true
  | Paren_block elems ->
    Format.fprintf
      formatter
      "%a(%a)"
      print_space
      add_space
      (print_block is_selector)
      elems;
    true
  | Function (name, elems) ->
    let printer = print_coma in
    Format.fprintf
      formatter
      "%a%s(%a)"
      print_space
      add_space
      (fst name)
      (print_block ~printer is_selector)
      (fst elems);
    true
;;

let print_important : template -> Format.formatter -> bool with_loc -> unit =
  fun template formatter (is_important, _) ->
  match is_important with
  | true -> Format.fprintf formatter template.important
  | false -> ()
;;

let rec print_brace_block : template -> Format.formatter -> Brace_block.t -> unit =
  fun template formatter -> function
  | Empty -> Format.fprintf formatter ";"
  | Declaration_list d ->
    Format.fprintf
      formatter
      template.declaration
      (Format.pp_print_list ~pp_sep:pp_sep_column (print_declaration_list template))
      (fst d)
  | Stylesheet css ->
    Format.fprintf
      formatter
      template.stylesheet
      (Format.pp_print_list (print_rule template))
      (fst css)

and print_at_rule : template -> Format.formatter -> At_rule.t -> unit =
  fun template formatter rule ->
  Format.fprintf
    formatter
    template.at_rule
    (fst rule.name)
    (print_block true ~add_space:true)
    (fst rule.prelude)
    (print_brace_block template)
    rule.block

and print_declaration
  : template -> Format.formatter -> Css_parser.Types.Declaration.t -> unit
  =
  fun template formatter { name; value; important; _ } ->
  Format.fprintf
    formatter
    "%s:%a%a"
    (fst name)
    (print_block false)
    (fst value)
    (print_important template)
    important

and print_declaration_list
  : template -> Format.formatter -> Css_parser.Types.Declaration_list.kind -> unit
  =
  fun template formatter -> function
  | Declaration decl -> print_declaration template formatter decl
  | At_rule rule -> print_at_rule template formatter rule
  | Style_rule style_rule -> print_style_rule template formatter style_rule

and print_style_rule
  : template -> Format.formatter -> Css_parser.Types.Style_rule.t -> unit
  =
  fun template formatter rule ->
  Format.fprintf
    formatter
    template.style_rule
    (print_block true)
    (fst rule.prelude)
    (Format.pp_print_list ~pp_sep:pp_sep_column (print_declaration_list template))
    (fst rule.block)

and print_rule : template -> Format.formatter -> Css_parser.Types.Rule.t -> unit =
  fun template formatter -> function
  | Rule.Style_rule style -> print_style_rule template formatter style
  | Rule.At_rule rule -> print_at_rule template formatter rule
;;

let css : template -> Format.formatter -> Css_parser.Types.Stylesheet.t -> unit =
  fun template formatter css ->
  Format.fprintf
    formatter
    template.main_css
    (Format.pp_print_list (print_rule template))
    (fst css)
;;
