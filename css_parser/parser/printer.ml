open! Core
module Types = Types
open Types

(* Component values can also be comments. Making sure that they print using the same
   function so that there are no discrepancies between the two *)
let comment_to_string comment = [%string {|/*%{comment}*/|}]

let print_comment ~buffer (comment, _) =
  Print_buffer.append buffer (comment_to_string comment)
;;

let prepend_space ~if_ ~buffer f =
  if if_ then Print_buffer.append buffer " ";
  f ~buffer
;;

let prepend_space_if_not_first_element ~buffer ~index f =
  prepend_space ~if_:(index <> 0) ~buffer f
;;

let print_brace_block_with_newline_separators
  (type a)
  ~(f : buffer:Print_buffer.t -> a -> unit)
  ~buffer
  (items : a list)
  : unit
  =
  Print_buffer.append buffer " {";
  Print_buffer.indent buffer;
  List.iter items ~f:(fun item ->
    Print_buffer.start_newline buffer;
    f ~buffer item);
  Print_buffer.dedent buffer;
  Print_buffer.start_newline buffer;
  Print_buffer.append buffer "}"
;;

let print_space_separated_comments
  ?(leading_space = true)
  ?(trailing_space = false)
  buffer
  = function
  | [] -> ()
  | comments ->
    let last_comment_index = List.length comments - 1 in
    List.iteri comments ~f:(fun index comment ->
      if leading_space || index <> 0 then Print_buffer.append buffer " ";
      print_comment ~buffer comment;
      if trailing_space && index = last_comment_index then Print_buffer.append buffer " ")
;;

let rec print_rule ~buffer : Rule.t -> unit = function
  | Qualified_rule rule -> print_qualified_rule ~buffer rule
  | At_rule rule -> print_at_rule ~buffer rule
  | Style_rule rule -> print_style_rule ~buffer rule
  | Comment comment -> print_comment ~buffer comment

and print_style_block_item ~buffer : Style_block.Block_element.t -> unit = function
  | Rule (r, _) -> print_rule ~buffer r
  | Declaration (d, _) -> print_declaration ~buffer d
  | Comment comment -> print_comment ~buffer comment

and print_style_rule ~buffer { selectors; block = block, _; _ } =
  print_selector_combinator_list ~buffer selectors;
  print_brace_block_with_newline_separators ~buffer ~f:print_style_block_item block

and print_selector_or_combinator
  ~buffer
  (selector_or_combinator : Complex_selector.selector_or_combinator)
  =
  match selector_or_combinator with
  | Selector s -> print_compound_selector ~buffer s
  | Combinator c -> print_combinator ~buffer c

(* Also prints relative selectors *)
and print_complex_selector ~buffer (complex_selector, _complex_selector_loc) =
  List.filter
    complex_selector
    ~f:(fun ((selector_or_combinator : Complex_selector.selector_or_combinator), _) ->
      (* We are printing spaces between elements of this list, so we have to filter out
         descendant combinators so that multiple spaces aren't printed when we only want
         one.
      *)
      let is_descendant_combinator =
        match selector_or_combinator with
        | Combinator (Descendant, _) -> true
        | _ -> false
      in
      not is_descendant_combinator)
  |> List.iteri ~f:(fun index (selector_or_combinator, comments) ->
    prepend_space_if_not_first_element
      ~index
      ~buffer
      print_selector_or_combinator
      selector_or_combinator;
    print_space_separated_comments buffer comments)

and print_selector_combinator_list ~buffer (complex_selector_list, _) =
  List.iteri
    complex_selector_list
    ~f:
      (fun
        index
        ( complex_selector
        , (* These comments are the comments that come after the commas *)
          comments_after_comma )
      ->
      (* The first selector in a list of selectors should not prepend a space. If it did,
         it would cause something like this to happen:

         "|" denotes start of line
         | .a .b .c {}

         OR 

         | .a:has( .b) {}
      *)
      prepend_space_if_not_first_element
        ~buffer
        ~index
        print_complex_selector
        complex_selector;
      let is_last_element_in_list = index = List.length complex_selector_list - 1 in
      if (not is_last_element_in_list)
         || (* The comments attached to the selector are the comments that come after a
               comma, so we do have to add the comma even if it's the last element.

               The parser should have thrown an error in that case, though.
            *)
         not (List.is_empty comments_after_comma)
      then (
        Print_buffer.append buffer ",";
        print_space_separated_comments buffer comments_after_comma))

and print_combinator ~buffer ((c, _) : Combinator.t with_loc) =
  let combinator =
    match c with
    | Descendant -> " "
    | Child -> ">"
    | Subsequent_sibling -> "~"
    | Next_sibling -> "+"
    | Column -> "||"
  in
  Print_buffer.append buffer combinator

(* A compound selector is an ordered sequence of selectors that are not separated by
   whitespace or combinators. They are allowed to be separated by comments, and the
   comments _must_be maintained in order to prevent the following:

   div/* */a

   from being output as

   diva

   which is a completely selector.

   There should not be any spaces between the selectors and the comments, or the semantic
   meaning of the selector changes.

   [div/* */a] is NOT equivalent to [div /* */ a] as the former selects for something
   which is both [div] and [a], while the latter selects for an [a] that is a child of a
   [div] due to the whitespaces
*)
and print_compound_selector ~buffer (selector_list, _) =
  List.iter selector_list ~f:(fun (selector, comments) ->
    print_selector ~buffer selector;
    List.iter ~f:(print_comment ~buffer) comments)

and print_selector ~buffer ((selector, _) : Selector.t with_loc) =
  match selector with
  | Id (id, _)
  (* The CSS spec mentions that: A <hash-token> with the "unrestricted" type flag may not
     need as much escaping as the same token with the "id" type flag.
     https://www.w3.org/TR/css-syntax-3/#serialization

     This doesn't apply to us since we aren't "parsing" the escape sequence into the
     character it represents, and are instead preserving the escape string verbatim. This
     means that we do not have to escape the value when serializing it
  *) -> Print_buffer.append buffer {%string|#%{id}|}
  | Class name -> Print_buffer.append buffer {%string|.%{name}|}
  | Attribute (c_list, _) ->
    Print_buffer.append buffer {%string|[%{component_value_list_to_string c_list}]|}
  | Pseudoclass p -> print_pseudoclass_like_selector ~buffer p
  | Pseudoelement p ->
    Print_buffer.append buffer ":";
    print_pseudoclass_like_selector ~buffer p
  | Type (t, ns) ->
    let type_selector =
      match ns with
      | Some ns -> [%string "%{ns}|%{t}"]
      | None -> t
    in
    Print_buffer.append buffer type_selector
  | Ampersand -> Print_buffer.append buffer "&"

and print_pseudoclass_like_selector ~buffer (p : Pseudoclass_element_selector.t) =
  Print_buffer.append buffer ":";
  match p with
  | Ident ident -> Print_buffer.append buffer ident
  | Function ((fn, _), c_list) ->
    Print_buffer.append
      buffer
      [%string "%{fn}(%{component_value_list_to_string  c_list})"]
  | Function_with_selectors (((fn, _), comments), sc_list) ->
    Print_buffer.append buffer [%string "%{fn}("];
    (* Prevents a leading space before the comment. Also appends a space after the last
       comment so that following combinator/selector is not directly attached to the last
       comment *)
    print_space_separated_comments
      ~leading_space:false
      ~trailing_space:true
      buffer
      comments;
    print_selector_combinator_list ~buffer sc_list;
    Print_buffer.append buffer ")"

and print_at_rule ~buffer { name; prelude; block; _ } =
  Print_buffer.append buffer {%string|@%{name}|};
  let at_rule_prelude =
    let%map.Option prelude, _ = prelude in
    component_value_list_to_string prelude
  in
  (match at_rule_prelude with
   | None | Some "" -> ()
   | Some str -> Print_buffer.append buffer [%string " %{str}"]);
  match block with
  | None -> Print_buffer.append buffer ";"
  | Some (Types.At_rule.Declaration_list (declaration_list, _)) ->
    print_brace_block_with_newline_separators
      ~f:print_declaration_list_item
      ~buffer
      declaration_list
  | Some (Types.At_rule.Style_block (block, _)) ->
    print_brace_block_with_newline_separators ~f:print_style_block_item ~buffer block

and print_declaration_list_item ~buffer (element : Declaration_list.Element.t) =
  match element with
  | At_rule (rule, _) -> print_at_rule ~buffer rule
  | Qualified_rule (rule, _) -> print_qualified_rule ~buffer rule
  | Declaration (decl, _) -> print_declaration ~buffer decl
  | Comment comment -> print_comment ~buffer comment

and print_declaration
  ~buffer
  { name = (name, _name_loc), name_comments
  ; value = value, _
  ; important = important, important_comments
  }
  =
  Print_buffer.append buffer name;
  print_space_separated_comments buffer name_comments;
  if List.length name_comments > 0 then Print_buffer.append buffer " ";
  Print_buffer.append buffer ": ";
  List.iter
    ~f:(fun component_value ->
      component_value_to_string component_value |> Print_buffer.append buffer)
    value;
  (match important with
   | true -> Print_buffer.append buffer " !important"
   | false -> ());
  print_space_separated_comments buffer important_comments;
  Print_buffer.append buffer ";"

and print_qualified_rule ~buffer { prelude = prelude, _; block = block, _; _ } =
  component_value_list_to_string prelude |> Print_buffer.append buffer;
  print_brace_block_with_newline_separators ~buffer ~f:print_declaration_list_item block

and component_value_to_string ?(whitespace_behavior = `Convert_to_single_space) (c, _) =
  match c with
  | Paren_block c_list ->
    let c_list = component_value_list_to_string ~whitespace_behavior c_list in
    [%string "(%{c_list})"]
  | Bracket_block c_list ->
    let c_list = component_value_list_to_string ~whitespace_behavior c_list in
    [%string "[%{c_list}]"]
  | Brace_block c_list ->
    let c_list = component_value_list_to_string ~whitespace_behavior c_list in
    [%string "{%{c_list}}"]
  | Percentage (base, _, exponent) ->
    let exponent =
      Option.value_map ~f:Css_parser_common.Exponent.to_string ~default:"" exponent
    in
    [%string "%{base}%{exponent}%"]
  | At_keyword at_name -> [%string "@%{at_name}"]
  | Ident (ident, _) -> ident
  | String str -> Css_parser_common.String_token.to_string str
  | Url url -> [%string "url(%{url})"]
  | Delim delim -> delim
  | Ampersand -> "&"
  | Comment (comment, _loc) -> comment_to_string comment
  | Ocaml_code ((code, sigil), _loc) ->
    let sigil = Interpolation_sigil.to_sigil_string sigil in
    let ocaml_code_leader = [%string "%{sigil}{"] in
    [%string {|%{ocaml_code_leader}%{code}}|}]
  | Function ((fn, _), c_list) ->
    let c_list = component_value_list_to_string c_list in
    [%string {|%{fn}(%{c_list})|}]
  | Hash (id, _) -> [%string "#%{id}"]
  | Number (base, _, exponent) ->
    let exponent =
      Option.value_map ~f:Css_parser_common.Exponent.to_string ~default:"" exponent
    in
    [%string "%{base}%{exponent}"]
  | Dimension (base, _, exponent, unit_) ->
    let exponent =
      Option.value_map ~f:Css_parser_common.Exponent.to_string ~default:"" exponent
    in
    [%string "%{base}%{exponent}%{unit_}"]
  | Whitespace ws ->
    (match whitespace_behavior with
     | `Convert_to_single_space -> " "
     | `Preserve_original -> ws)
  | Semicolon -> ";"
  | Colon -> ":"
  | Comma -> ","
  | Cdo -> "<!--"
  | Cdc -> "-->"
  | Bad_string bad_string -> [%string {|"%{bad_string}"|}]
  | Bad_url bad_url -> [%string "url(%{bad_url})"]
  | Right_brace -> "}"
  | Right_paren -> ")"
  | Right_bracket -> "]"

and component_value_list_to_string ?whitespace_behavior c_list =
  List.map c_list ~f:(component_value_to_string ?whitespace_behavior)
  |> String.concat ~sep:""
;;

let stylesheet_to_string (stylesheet, _) =
  let buffer = Print_buffer.create ~indent_size:2 16 in
  List.iter
    ~f:(fun rule ->
      print_rule ~buffer rule;
      Print_buffer.start_newline buffer)
    stylesheet;
  Print_buffer.contents buffer |> String.rstrip
;;

let style_block_contents_to_string (style_block_contents, _) =
  let buffer = Print_buffer.create ~indent_size:2 16 in
  List.iter
    ~f:(fun style_block_item ->
      print_style_block_item ~buffer style_block_item;
      Print_buffer.start_newline buffer)
    style_block_contents;
  Print_buffer.contents buffer |> String.rstrip
;;

let for_apply_style_to_string (for_apply_style, _) =
  let buffer = Print_buffer.create ~indent_size:2 16 in
  List.iter
    ~f:(fun component_value ->
      component_value_to_string ~whitespace_behavior:`Preserve_original component_value
      |> Print_buffer.append buffer)
    for_apply_style;
  Print_buffer.contents buffer |> String.rstrip
;;
