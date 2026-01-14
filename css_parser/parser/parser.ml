open! Core
open Css_parser_common
module Token = Css_parser_common.Token
open Types

let rec consume_list_of_rules tokens =
  let rules_with_loc =
    Parse_queue.with_loc
      ~context:(Some Rules)
      ~f:
        (Parse_queue.process_into_list ~while_:(Not (Is EOF)) ~f:(function
          | WHITESPACE _ | CDO | CDC | SEMICOLON -> None
          | COMMENT comment ->
            let loc = Parse_queue.dequeue_exn tokens |> Location.from_token in
            Some (Rule.Comment (comment, loc))
          | AT_KEYWORD _ -> Some (At_rule (consume_at_rule tokens))
          | _ -> Some (Style_rule (consume_style_rule tokens))))
      tokens
  in
  match rules_with_loc with
  | None ->
    (match Parse_queue.to_list tokens with
     | [] ->
       Parse_queue.raise_no_location
         "Tried to parse but received empty queue of tokens. This is a parse error, \
          please contact the maintainers of the CSS parser"
     | [ (Token.EOF, loc) ] -> [], loc
     | remaining_tokens ->
       let remaining_tokens =
         List.map
           ~f:(fun (token, loc) -> [%string "%{token#Token} (%{loc#Location})"])
           remaining_tokens
         |> String.concat ~sep:"\n"
       in
       Parse_queue.raise_no_location
         [%string
           "Didn't parse anything but token queue is not empty.\n\
           \ Remaining tokens: %{remaining_tokens}"])
  | Some (rules, loc) -> rules, loc

and consume_style_rule tokens =
  let selectors =
    Parse_queue.with_loc_exn
      ~f:(consume_complex_selector_list ~list_terminator:Token.is_left_brace)
      tokens
  in
  let style_block = Parse_queue.with_loc_exn ~f:consume_style_block tokens in
  let loc = Location.merge ~start:(snd selectors) ~end_:(snd style_block) in
  { Style_rule.selectors; block = style_block; loc }

and is_declaration_or_qualified_rule tokens =
  Parse_queue.fold_until
    ~init:`Neither
    ~finish:(fun _ -> `Declaration `Definite)
    ~f:(fun seen (token, _) ->
      match seen with
      (* Check if starts with identifier. If it does and the next non-whitespace and
         non-comment token is a colon, this could be either a qualified rule or a
         declaration *)
      | `Neither ->
        (match token with
         | WHITESPACE _ | COMMENT _ -> Continue `Neither
         | IDENT _ -> Continue `Ident
         | _ -> Stop `Qualified_rule)
      | `Ident ->
        (match token with
         | WHITESPACE _ | COMMENT _ -> Continue `Ident
         | COLON -> Continue (`Ident_and_colon `Just_seen)
         | _ -> Stop `Qualified_rule)
      | `Ident_and_colon processing_state ->
        let continue = Continue_or_stop.Continue (`Ident_and_colon `Stale) in
        (match token with
         (* If we see whitespace directly after the first ident-colon pair, we know it
            must be a declaration. *)
         | WHITESPACE _ | COMMENT _ ->
           (match processing_state with
            | `Just_seen -> Stop (`Declaration `Definite)
            | `Stale -> continue)
         | SEMICOLON -> Stop (`Declaration `Definite)
         (* Including right brace here so that we throw the proper error if the last
            declaration in a block does not end in a semicolon *)
         | RIGHT_BRACE -> Stop (`Declaration `Maybe_selector)
         (* We're making the distinction here that a brace block cannot exist as a
            component value inside a declaration value *)
         | LEFT_BRACE -> Stop `Qualified_rule
         (* Once we hit EOF, we cannot trivially descern between a (potentially) malformed
            declaration and a complex selector. *)
         | EOF -> Stop (`Declaration `Maybe_selector)
         | _ -> continue))
    tokens

and consume_style_block_contents ~while_ tokens =
  Parse_queue.with_context
    Block
    ~f:(fun tokens ->
      Parse_queue.process_into_list tokens ~while_ ~f:(function
        | SEMICOLON | WHITESPACE _ -> None
        | COMMENT comment ->
          let loc = Parse_queue.dequeue_exn tokens |> Location.from_token in
          Some (Style_block.Block_element.Comment (comment, loc))
        | AT_KEYWORD _ ->
          let at_rule =
            Parse_queue.with_loc_exn ~f:(fun tokens -> consume_at_rule tokens) tokens
            |> Tuple2.map_fst ~f:(fun rule -> Rule.At_rule rule)
          in
          Some (Rule at_rule)
        | _ ->
          (match is_declaration_or_qualified_rule tokens with
           | `Declaration ambiguity ->
             let declaration =
               Parse_queue.with_loc_exn
                 ~f:(fun tokens -> consume_declaration ~ambiguity tokens)
                 tokens
             in
             Some (Declaration declaration)
           | `Qualified_rule ->
             let rule =
               Parse_queue.with_loc_exn ~f:consume_style_rule tokens
               |> Tuple2.map_fst ~f:(fun rule -> Rule.Style_rule rule)
             in
             Some (Rule rule))))
    tokens

and consume_style_block tokens =
  Parse_queue.require_next_token_to_match_and_ignore
    tokens
    ~matches:(Is LEFT_BRACE)
    ~error_msg:(fun _ -> "Error while parsing style block. Expected opening left brace");
  let rules =
    consume_style_block_contents ~while_:(Not (Any [ Is RIGHT_BRACE; Is EOF ])) tokens
  in
  Parse_queue.require_next_token_to_match_and_ignore
    tokens
    ~matches:(Is RIGHT_BRACE)
    ~error_msg:(fun _ -> "Error while parsing style block. Expected closing right brace");
  rules

(*=This function checks to see if there are comments and/or whitespaces between a
   namespace and the namespace selector. It should only be called after a type selector
   or an asterisk, as those are the only two selectors that are allowed to have namespace
   separators

   Ex (true):
   a |div
   a/**/|div

   Ex (false):
   a|div
   a||div

   You can read about namespaces here: https://developer.mozilla.org/en-US/docs/Web/CSS/@namespace
*)
and matches_namespace_separator tokens =
  (*=Returns false for [selector] "||", as "||" is a valid selector. *)
  Parse_queue.fold_until
    tokens
    ~init:`Init
    ~f:(fun acc (token, _loc) ->
      match acc, token with
      | `Init, DELIM "|" -> Continue `Maybe_valid_namespace
      (* Two pipes consecutively gives us a column combinator, which is not a namespace *)
      | `Maybe_valid_namespace, DELIM "|" -> Stop `Not_namespace
      (* Single pipe followed by a whitespace or a comment is an invalid namespace *)
      | `Maybe_valid_namespace, (WHITESPACE _ | COMMENT _) -> Stop `Trailing_invalid
      | `Maybe_valid_namespace, (IDENT _ | DELIM "*") -> Stop `Valid_namespace
      (* We have to match at least one whitespace or comment before hitting the pipe
         delimiter in order for this to be invalid
      *)
      | (`Init | `Whitespace_or_comment), (WHITESPACE _ | COMMENT _) ->
        Continue `Whitespace_or_comment
      (* Detached pipe could still lead to a column combinator *)
      | `Whitespace_or_comment, DELIM "|" -> Continue `Detached_pipe
      | `Detached_pipe, DELIM "|" -> Stop `Not_namespace
      (* A singular pipe in a selector can only be a namespace separator, so it's safe to
         terminate here
      *)
      | `Detached_pipe, _ -> Stop `Leading_invalid
      (* If none of the cases are matched, we can assume this is not a namespace *)
      | _ -> Stop `Not_namespace)
    ~finish:(fun _ -> `Not_namespace)

and consume_type_selector tokens =
  let get_type_selector_part tokens =
    (*=Parses the namespace and the selector as they both have the same requirements

       The selector can be something along these lines:
       - *
       - *|*
       - *|<type>
       - <type>|*
       - <type>|<type>
       - <type>

       This function parses what comes before and after the "|"

       If there is no "|", there is no namespace, and it's just a type selector
    *)
    match Parse_queue.dequeue_exn tokens with
    | IDENT ident, _ -> Ident_like.to_string ident
    | DELIM "*", _ -> "*"
    | token, loc ->
      Parse_queue.raise
        ~loc
        [%string
          "Error while parsing type selector. Expected an identifier or an asterisk, but \
           found: %{token#Token}"]
  in
  let first_type_selector_part = get_type_selector_part tokens in
  match matches_namespace_separator tokens with
  | `Not_namespace -> Selector.Type (first_type_selector_part, None)
  | `Trailing_invalid ->
    Parse_queue.dequeue_exn tokens
    |> Parse_queue.throw_error_for_token ~f:(fun _ ->
      "Comments and/or whitespaces are not allowed directly after a namespace separator")
  | `Leading_invalid ->
    Parse_queue.dequeue_exn tokens
    |> Parse_queue.throw_error_for_token ~f:(fun _ ->
      "Comments and/or whitespaces are not allowed directly before a namespace separator")
  | `Valid_namespace ->
    (* This shouldn't throw an error if [matches_namespace_separator] logic is correct *)
    Parse_queue.require_next_token_to_match_and_ignore
      ~matches:(Equals (DELIM "|"))
      ~error_msg:(fun _ ->
        "Expected a namespace separator. This is an error within the logic of the CSS \
         parser, please contact the maintainers of the parser.")
      tokens;
    let type_selector = get_type_selector_part tokens in
    Selector.Type (type_selector, Some first_type_selector_part)

and parse_pseudo_class_like_selector ~kind tokens =
  (* Parses pseudoclasses, but the same logic can be used for pseudoelements so long as
     the leading ":" is dropped prior to running this function.

     [kind] is used to tailor the error messages to the selector type, as well as to parse
     the arguments of specific function as either selectors or component values
  *)
  let kind_string =
    match kind with
    | `Pseudoclass -> "pseudoclass"
    | `Pseudoelement -> "pseudoelement"
  in
  let error_msg = lazy [%string "Error while parsing %{kind_string} selector."] in
  Parse_queue.require_next_token_to_match_and_ignore
    ~matches:(Is COLON)
    ~error_msg:(fun _ ->
      match kind with
      | `Pseudoelement ->
        (* This error shouldn't raise unless the lookahead that directs the parser into
           parsing a pseudoelement has a bug in it *)
        [%string
          "%{force error_msg} Expected to parse a pseudoelement but missing second \
           colon. This is potentially an error in the CSS parser, please contact the \
           maintainers of the parser."]
      | `Pseudoclass ->
        [%string
          "%{force error_msg} %{String.capitalize kind_string} selector must start with \
           a colon"])
    tokens;
  match Parse_queue.dequeue_exn ~error_msg tokens with
  | (COMMENT _, _) as comment ->
    Parse_queue.throw_error_for_token comment ~f:(fun _ ->
      [%string
        "%{force error_msg} Comments should not occur between parts of a %{kind_string} \
         selector"])
  | IDENT ident, _ -> Pseudoclass_element_selector.Ident (Ident_like.to_string ident)
  | (FUNCTION fn, _) as fn_token ->
    let fn = Ident_like.to_string fn in
    (* There are a few more pseudoclass functions which accept selectors, but like
       [::slotted] below, they deal with shadow DOM and only accept a <compound-selector>.
       They should be audited to see if they should also be parsed as selectors.
    *)
    let pseudoclass_functions_with_selectors_matches_fn =
      List.mem
        ~equal:[%equal: string]
        [ "has"; "is"; "where"; "not" ]
        (String.lowercase fn)
    in
    let fn_name_with_loc = fn, Location.from_token fn_token in
    (match kind, `Match_pseudoclass pseudoclass_functions_with_selectors_matches_fn with
     (* Pseudoelements do technically have a function that take selectors as an argument,
        which is [::slotted]. However, it is a function that targets elements within the
        shadow DOM, and should probably be audited to see if we should allow the arguments
        to be parsed as a selector.

        [::slotted] also only accepts a <compound-selector>
     *)
     | `Pseudoclass, `Match_pseudoclass true ->
       (* Parse the arguments to the function as selectors *)
       (* These are the comments that occur directly after the '(' of the function *)
       let comments = Parse_queue.consume_comments_and_ignore_whitespaces tokens in
       let fn_name_with_comments = fn_name_with_loc, comments
       and fn_selectors_with_loc = consume_function_items_as_selectors tokens in
       Pseudoclass_element_selector.Function_with_selectors
         (fn_name_with_comments, fn_selectors_with_loc)
     | _, _ ->
       (* Parse the arguments to the function as component values *)
       let fn_items_with_loc = consume_function_items_as_component_values tokens in
       Function (fn_name_with_loc, fn_items_with_loc))
  | token ->
    Parse_queue.throw_error_for_token
      ~f:(fun token ->
        [%string
          "%{force error_msg} Expected an identifier or function but got %{token#Token}"])
      token

and consume_pseudoclass_selector tokens =
  let pseudoclass_selector = parse_pseudo_class_like_selector ~kind:`Pseudoclass tokens in
  Selector.Pseudoclass pseudoclass_selector

and consume_pseudoelement_selector tokens =
  Parse_queue.require_next_token_to_match_and_ignore
    ~matches:(Is COLON)
    ~error_msg:(fun _ -> "Pseudoelement selector must start with a colon")
    tokens;
  let pseudoelement_selector =
    parse_pseudo_class_like_selector ~kind:`Pseudoelement tokens
  in
  Selector.Pseudoelement pseudoelement_selector

and consume_subclass_selector tokens =
  (* Technically, pseudoclasses are included in the subclass selector designation.
     However, we're not parsing them here because they have a bit more complexity behind
     them, and their parsing logic is basically the same as pseudoelements, which are not
     subclass selectors
  *)
  match Parse_queue.peek_token_exn tokens with
  | HASH (id, flag) ->
    Parse_queue.dequeue_and_ignore_exn tokens;
    Selector.Id (Ident_like.to_string id, flag)
  | DELIM "." ->
    Parse_queue.dequeue_and_ignore_exn tokens;
    let ident, _ =
      (* Ocaml interpolation is currently only allowed in declaration values *)
      Parse_queue.maybe_throw_ocaml_code_error tokens;
      Parse_queue.require_next_token_to_match
        ~matches:(Is IDENT)
        ~error_msg:(fun token ->
          [%string
            "Error while parsing class. Expected a CSS identifier but got %{token#Token}"])
        tokens
    in
    Class (Ident_like.to_string ident)
  | LEFT_BRACKET ->
    let block_contents =
      Parse_queue.with_loc_exn
        ~f:(fun tokens ->
          consume_block
            ~remove_surrounding_whitespaces:true
            ~allow_ocaml_code:false
            ~kind:`Bracket
            tokens)
        tokens
    in
    Selector.Attribute block_contents
  | _ ->
    (* Ocaml interpolation is currently only allowed in declaration values *)
    Parse_queue.maybe_throw_ocaml_code_error tokens;
    Parse_queue.dequeue_exn tokens
    |> Parse_queue.throw_error_for_token ~f:(fun token ->
      [%string
        "Error while parsing class. Got unexpected token %{token#Token} at start of class"])

(* A compound selector is a group of selectors that is not separated by whitespaces or
   combinators *)
and consume_compound_selector tokens =
  Parse_queue.with_loc_exn ~f:(consume_compound_selector_part ~stage:`Type) tokens

(* Check to see if [compound_selector_part] is the last selector in the compound selector.
   This is to see if we should parse the comments that may not be separated from the selector 
   as part of the [Selector_combinator] instead of part of the [Selector].

   Without this check, these two selectors will parse into different ASTs:

   .a/* comment */ {}

   .a /* comment */ {}

   We want them to parse into the same AST which will eventually print to:
   .a /* comment */ {}
*)
and is_last_compound_selector_part_ignore_comments tokens =
  Parse_queue.fold_until
    tokens
    ~init:()
    ~f:(fun () (token, _) ->
      match token with
      | COMMENT _ -> Continue ()
      (* The rest of this list should be the same as in [consume_compound_selector_part] *)
      | DELIM ("~" | "+" | ">" | "|")
      | WHITESPACE _
      (* Right parens end a list of complex selectors for pseudoclass functions *)
      | RIGHT_PAREN
      (* Commas separate complex selectors in lists of complex selectors *)
      | COMMA
      (* Left braces end the list of selectors for style rules *)
      | LEFT_BRACE -> Stop true
      | _ -> Stop false)
    ~finish:(fun () -> true)

and consume_comments_if_not_last_compound_selector_part tokens =
  match is_last_compound_selector_part_ignore_comments tokens with
  | true -> []
  | false -> Parse_queue.consume_comments_only tokens

(** [consume_compound_selector_part] ensures that we're parsing the compound selectors in
    the proper order, as the selectors within a <compound-selector> have a schema that
    they must adhere to.

    If [strict] is false, we will __not__ check that the selectors follow a proper
    ordering *)
and consume_compound_selector_part ~stage tokens =
  let raise_error_from_token ~stage tokens =
    (* Ocaml interpolation is currently only allowed in declaration values *)
    Parse_queue.maybe_throw_ocaml_code_error tokens;
    let stage_name =
      match stage with
      | `Type -> Some "type"
      | `Subclass -> Some "subclass"
      | `Pseudoelement -> Some "pseudoelement"
      | `Unchecked -> None
    in
    Parse_queue.dequeue_exn tokens
    |> Parse_queue.throw_error_for_token ~f:(fun token ->
      (* Consume remaining tokens. This ensures any stop exceptions have priority over a
         parse error. *)
      Parse_queue.consume_and_ignore ~while_matches:(Not (Is EOF)) tokens;
      let stage_name = Option.value ~default:"a" stage_name in
      [%string
        "Error while parsing selector. Expected start of %{stage_name} selector but got \
         %{token#Token}"])
  in
  match Parse_queue.peek_token_exn tokens, stage with
  | ( (*=These delimiters as well as whitespace are considered combinators, which appear
         between compound selectors.

         "|" specifically is a namespace separator and is
         considered to be part of a type selector, but that should be parsed out by
         [consume_type_selector] so long as it's parsing properly.

         "|" is included as a lookahead for "||", which is the column combinator.
      *)
      ( DELIM ("~" | "+" | ">" | "|")
      | WHITESPACE _
      (* Right parens end a list of complex selectors for pseudoclass functions *)
      | RIGHT_PAREN
      (* Commas separate complex selectors in lists of complex selectors *)
      | COMMA
      (* Left braces end the list of selectors for style rules *)
      | LEFT_BRACE
      (* If we haven't parsed these comments out, it's because they're on the last part of
         a compound selector. See [is_last_compound_selector_part_ignore_comments] for an
         explanation as to why we're not parsing these out here
      *)
      | COMMENT _ )
    , _ ) -> []
  (* Ampersands can be placed anywhere and do not change the stage *)
  | DELIM "&", stage ->
    let loc = Parse_queue.dequeue_exn tokens |> Location.from_token in
    let comment = consume_comments_if_not_last_compound_selector_part tokens in
    ((Selector.Ampersand, loc), comment) :: consume_compound_selector_part ~stage tokens
  (* There can only be a single type selector as it's impossible for a single element to
     match against two type selectors. They also must always come first (excluding
     ampersands) *)
  | (IDENT _ | DELIM "*"), (`Type | `Unchecked) ->
    let type_selector = Parse_queue.with_loc_exn ~f:consume_type_selector tokens in
    let comment = consume_comments_if_not_last_compound_selector_part tokens in
    (type_selector, comment) :: consume_compound_selector_part ~stage:`Subclass tokens
  | (IDENT _ | DELIM "*"), _ ->
    Parse_queue.dequeue_exn tokens
    |> Parse_queue.throw_error_for_token ~f:(fun _ ->
      "Error while parsing selector. Type selectors are only allowed at the start of a \
       compound selector or directly after a leading ampersand")
  (* Subclass selectors are not allowed once we parse a pseudoelement selector *)
  | (DELIM "." | HASH _ | LEFT_BRACKET), (`Type | `Subclass | `Unchecked) ->
    let subclass_selector =
      Parse_queue.with_loc_exn ~f:consume_subclass_selector tokens
    in
    let comment = consume_comments_if_not_last_compound_selector_part tokens in
    (subclass_selector, comment) :: consume_compound_selector_part ~stage:`Subclass tokens
  | COLON, stage ->
    (* Checking the token after the colon we matched on to see if this is a pseudoelement
       or pseudoclass selector *)
    (match Parse_queue.get_nth_token_exn ~n:1 tokens with
     | COLON ->
       let stage =
         match stage with
         | `Unchecked -> `Unchecked
         | _ -> `Pseudoelement
       in
       let pseudoelement_selector =
         Parse_queue.with_loc_exn ~f:consume_pseudoelement_selector tokens
       in
       let comment = consume_comments_if_not_last_compound_selector_part tokens in
       (pseudoelement_selector, comment) :: consume_compound_selector_part ~stage tokens
     | _ ->
       let stage =
         match stage with
         | `Unchecked -> `Unchecked
         | `Pseudoelement -> `Pseudoelement
         | _ -> `Subclass
       in
       let pseudoclass_selector =
         Parse_queue.with_loc_exn ~f:consume_pseudoclass_selector tokens
       in
       let comment = consume_comments_if_not_last_compound_selector_part tokens in
       (pseudoclass_selector, comment) :: consume_compound_selector_part ~stage tokens)
  | _, stage -> raise_error_from_token ~stage tokens

and consume_combinator tokens =
  let combinator =
    match
      Parse_queue.dequeue_exn
        ~error_msg:(lazy "Hit unexpected EOF while parsing combinator")
        tokens
    with
    | DELIM "~", _ -> Combinator.Subsequent_sibling
    | DELIM "+", _ -> Next_sibling
    | DELIM ">", _ -> Child
    | DELIM "|", _ ->
      Parse_queue.require_next_token_to_match_and_ignore
        ~matches:(Equals (DELIM "|"))
        ~error_msg:(fun _ ->
          [%string "Namespace separators are only valid for type selectors"])
        tokens;
      Combinator.Column
    | WHITESPACE _, _ ->
      Parse_queue.consume_and_ignore_whitespaces tokens;
      (match is_start_of_combinator tokens with
       | true -> consume_combinator tokens
       | false -> Combinator.Descendant)
    | token ->
      Parse_queue.throw_error_for_token
        ~f:(fun token ->
          [%string
            "Error while parsing combinator. %{token#Token} is not a valid combinator"])
        token
  in
  Parse_queue.consume_and_ignore_whitespaces tokens;
  combinator

and is_start_of_combinator tokens =
  match Parse_queue.peek_token_exn tokens with
  | DELIM ("~" | "+" | ">" | "|") -> true
  | _ -> false

and is_potentially_start_of_selector = function
  | Token.DELIM ("&" | "*" | ".") | IDENT _ | COLON | HASH _ | LEFT_BRACKET -> true
  | _ -> false

and is_next_token_potentially_start_of_selector tokens =
  Parse_queue.peek_exn tokens |> fst |> is_potentially_start_of_selector

and consume_complex_selector_list
  ?(allow_relative = true)
  ~list_terminator
  (tokens : Parse_queue.t)
  : Complex_selector.t with_comments list
  =
  Parse_queue.process_into_list
    ~while_:(Not (Any [ Matches list_terminator; Is EOF ]))
    ~f:(fun _ ->
      Parse_queue.consume_and_ignore_whitespaces tokens;
      (* Process selectors up until comma or until [while_] is true *)
      let maybe_leading_combinator =
        (* If we allow for relative selectors, we may or may not have a leading
           combinator. *)
        match
          ( `Starts_combinator (is_start_of_combinator tokens)
          , `Allow_relative allow_relative )
        with
        | `Starts_combinator true, `Allow_relative true ->
          let combinator =
            Complex_selector.Combinator
              (Parse_queue.with_loc_exn ~f:consume_combinator tokens)
          in
          let comment = Parse_queue.consume_comments_and_ignore_whitespaces tokens in
          Some (combinator, comment)
        | `Starts_combinator true, `Allow_relative false ->
          Parse_queue.dequeue_exn tokens
          |> Parse_queue.throw_error_for_token ~f:(fun _ ->
            "Relative selectors are not allowed here.")
        | `Starts_combinator false, _ -> None
      in
      let potential_selectors_loc_for_error = Parse_queue.peek_exn tokens |> snd in
      let selectors_with_loc =
        Parse_queue.with_loc
          ~f:
            ((* Each iteration of this function returns a one or two item list so that we
                can ensure that combinators are always followed by a selector.

                The selector-combinator list is then flattened before returning the
                values.

                The one-or-two-item list can be one of:

                1. [ <selector> ]
                2. [ <selector> ; <combinator> ]

                Due to the way selectors are parsed, if we receive a one-item list, it
                means we have reached the end of the selector.

                If we receive a two-item list, we must also check if the next token in the
                queue starts a selector, as a selector-combinator list cannot end in a
                combinator
             *)
             Parse_queue.process_into_list
               ~while_:(Not (Any [ Matches list_terminator; Is EOF; Is COMMA ]))
               ~f:(fun _ ->
                 (* Ignore whitespace if it happens outside of a combinator *)
                 Parse_queue.consume_and_ignore_whitespaces tokens;
                 (* We have to consume at least one selector here *)
                 let current_selector = consume_compound_selector tokens in
                 (* Remove the whitespace tokens and comments if they exist. This will be
                    used to generate a descendant selector if needed.

                    If there are any comments produced by this function, we know they have
                    a leading whitespace, as [consume_compound_selector] calls
                    [consume_compound_selector_parts], which consumes all cosecutive
                    trailing comments.
                 *)
                 let comments_and_whitespaces, comment_loc =
                   Parse_queue.with_loc
                     ~f:Parse_queue.consume_comment_and_whitespace
                     tokens
                   |> Option.value ~default:([], current_selector |> snd)
                 in
                 let comments, seen_whitespace =
                   List.fold
                     ~init:(Reversed_list.[], false)
                     comments_and_whitespaces
                     ~f:(fun (comments, seen_whitespace) -> function
                       | `Whitespace _ -> comments, true
                       | `Comment comment -> comment :: comments, seen_whitespace)
                 in
                 (* Comments that occur after a selector but are separated from the
                    selector by whitespace *)
                 let comments = Reversed_list.rev comments in
                 let selector = Complex_selector.Selector current_selector, comments in
                 let combinator =
                   let is_descendant_combinator =
                     seen_whitespace && is_next_token_potentially_start_of_selector tokens
                   in
                   match
                     (* These should never be true at the same time *)
                     ( `Starts_combinator (is_start_of_combinator tokens)
                     , `Is_descendant_combinator is_descendant_combinator )
                   with
                   | `Starts_combinator true, `Is_descendant_combinator false ->
                     let combinator =
                       Parse_queue.with_loc_exn ~f:consume_combinator tokens
                     in
                     let comment =
                       Parse_queue.consume_comments_and_ignore_whitespaces tokens
                     in
                     Parse_queue.throw_error_if_next_token_matches
                       ~f:(Not (Matches is_potentially_start_of_selector))
                       ~error_msg:"Combinators must be followed by a selector"
                       tokens;
                     Some (Complex_selector.Combinator combinator, comment)
                   | `Starts_combinator false, `Is_descendant_combinator true ->
                     (* If the token after the whitespace could potentially start a
                        selector, we will assume this is a descendant selector and try to
                        parse a selector. Otherwise we will toss the whitespace
                     *)
                     let loc = comment_loc in
                     Some
                       ( Combinator (Descendant, loc)
                       , []
                         (* Descendant combinator will never have comments due to them
                            being consumed by the end of the compound selector *)
                       )
                   (* If neither are true, we're probably at the end of the list and the
                      next token is either a comma or the terminating character. *)
                   | `Starts_combinator false, `Is_descendant_combinator false -> None
                   | `Starts_combinator true, `Is_descendant_combinator true ->
                     Parse_queue.raise
                       ~loc:(Parse_queue.dequeue_exn tokens |> snd)
                       "Error parsing selector. Next token both starts a combinator and \
                        is a selector. This should never happen, please contact the \
                        maintainers of the CSS parser."
                 in
                 Some (List.filter_opt [ Some selector; combinator ])))
          tokens
      in
      let has_comma = Parse_queue.dequeue_if_matches ~f:(Is COMMA) tokens in
      (* This guards against not having parsed anything where selectors were expected *)
      match selectors_with_loc with
      | None | Some ([], _) ->
        Parse_queue.raise
          ~loc:potential_selectors_loc_for_error
          "Expected a selector but nothing was found"
      | Some (selectors, selector_loc) ->
        let comments = Parse_queue.consume_comments_and_ignore_whitespaces tokens in
        let selectors =
          let selectors = List.concat selectors in
          match maybe_leading_combinator with
          | Some leading_combinator -> leading_combinator :: selectors
          | None -> selectors
        in
        let is_next_token_end_of_list =
          Parse_queue.peek_token_exn tokens |> list_terminator
        in
        (match has_comma && is_next_token_end_of_list with
         | true ->
           (* We don't allow for dangling commas at the end of selector lists. *)
           Parse_queue.raise
             ~loc:selector_loc
             "The last selector in a list cannot have a dangling comma."
         | false -> Some ((selectors, selector_loc), comments)))
    tokens

and consume_qualified_rule tokens : Qualified_rule.t =
  let prelude, prelude_loc =
    (* This location is only held so that we can have a location for errors *)
    let potential_prelude_error_loc = Parse_queue.peek_exn tokens |> snd in
    let prelude_with_loc =
      Parse_queue.with_loc
        ~f:
          (consume_component_value_list
             ~allow_ocaml_code:false
             ~remove_surrounding_whitespaces:true
             ~while_:(Not (Any [ Is LEFT_BRACE; Is EOF ])))
        tokens
    in
    (* Checks if a prelude is empty. Preludes with solely whitespace and/or comments are
       considered to be empty *)
    let maybe_empty_prelude =
      match prelude_with_loc with
      | None -> None
      | Some (prelude, prelude_loc) ->
        let ignore_whitespace_and_comment = function
          | Component_value.Whitespace _, _ | Comment _, _ -> false
          | _ -> true
        in
        (match List.filter prelude ~f:ignore_whitespace_and_comment with
         | [] -> None
         | _ -> Some (prelude, prelude_loc))
    in
    match maybe_empty_prelude with
    | None ->
      Parse_queue.raise
        ~loc:potential_prelude_error_loc
        "Qualified rule prelude cannot be empty."
    | Some prelude_with_loc -> prelude_with_loc
  in
  let block, loc_2 = Parse_queue.with_loc_exn ~f:consume_declaration_list_block tokens in
  let loc = Location.merge ~start:prelude_loc ~end_:loc_2 in
  { Qualified_rule.prelude = prelude, loc; block = block, loc_2; loc }

(* Recursively consumes whitespaces until a non-whitespace component value is seen *)
and remove_whitespace_from_end_of_component_values_list = function
  | Reversed_list.((Component_value.Whitespace _, _) :: tl) ->
    remove_whitespace_from_end_of_component_values_list tl
  | component_values -> component_values

and process_important_from_declaration_value declaration_value =
  (* Recursively consumes whitespaces and comments from the head of the reversed list.
     Returns the list of comments and the remaining reversed list *)
  let rec eat_whitespaces_and_comments component_values =
    match remove_whitespace_from_end_of_component_values_list component_values with
    | (Component_value.Comment (comment, _comment_loc), comment_loc) :: tl ->
      let child_comments, tl = eat_whitespaces_and_comments tl in
      Reversed_list.((comment, comment_loc) :: child_comments), tl
    | tl -> Reversed_list.[], tl
  in
  (* Remove whitespaces from the end of the list of component values *)
  let component_values =
    Reversed_list.of_list_rev declaration_value
    |> remove_whitespace_from_end_of_component_values_list
  in
  let end_comments, tl = eat_whitespaces_and_comments component_values in
  match tl with
  | (Component_value.Ident (ident, _), _) :: tl
    when String.lowercase ident |> String.equal "important" ->
    (* After removing all whitespaces and comments from the end of the list, check to see
       if the first element in the reversed list (last element in the regular list after
       removing whitespaces and comments) is the case-insensitive string "important"
    *)
    let tl = remove_whitespace_from_end_of_component_values_list tl in
    (* Any number of whitespaces is allowed between [!] and [important] *)
    (match tl with
     | (Component_value.Delim "!", _) :: tl ->
       (* Remove all whitespaces that come before the [!] so that the declaration does not
          end in whitespace *)
       ( remove_whitespace_from_end_of_component_values_list tl |> Reversed_list.rev
       , (true, Reversed_list.rev end_comments) )
     (* If the next token is not a !, return the original list *)
     | _ -> Reversed_list.rev component_values, (false, []))
  (* Ignore the value from the match statement as that has both comments and whitespaces
     removed from the start of the reversed list *)
  | _ -> Reversed_list.rev component_values, (false, [])

and consume_declaration ~ambiguity tokens =
  let name, name_loc =
    Parse_queue.require_next_token_to_match
      ~matches:(Is IDENT)
      ~error_msg:(fun _ -> "Declaration must start with a valid CSS identifier.")
      tokens
  and
    (* Any number of whitespace is allowed around the colon that separates the declaration
       name and values *)
    name_comments
    =
    Parse_queue.consume_comments_and_ignore_whitespaces tokens
  in
  let name = Ident_like.to_string name in
  let is_custom_property =
    (* Custom properties are allowed to have empty declaration values
       https://www.w3.org/TR/css-values-4/#dashed-idents
    *)
    String.is_prefix ~prefix:"--" name && String.length name > 2
  in
  let context =
    let decl = Context.Declaration { property_name = name } in
    match ambiguity with
    | `Definite ->
      (* Definite, can use Declaration directly *)
      decl
    | `Maybe_selector ->
      (* Might be a selector, so include the [Rules] context which includes selectors *)
      Ambiguous [ decl; Rules ]
  in
  let (), colon_loc =
    Parse_queue.with_context
      context
      ~f:(fun tokens ->
        Parse_queue.require_next_token_to_match
          ~matches:(Is COLON)
          ~error_msg:(fun _ -> "Declaration name must be followed by a colon.")
          tokens)
      tokens
  in
  let check_maybe_missing_semicolon =
    let seen_so_far = ref `Nothing in
    let previous_token_loc = ref colon_loc in
    let non_whitespace_component_value_before_ident_loc = ref None in
    fun token (_component_value, component_value_loc) ->
      let new_value =
        match !seen_so_far, token with
        (* While it's technically not possible to have an ident come after another ident,
           should still match here in case
        *)
        | (`Nothing | `Ident), Token.IDENT _ ->
          (* We're using the token before the start of a declaration as the end of the
             last declaration. This is probably a whitespace token. We're going to use the
             start of that token's location as the end of the location of the error *)
          non_whitespace_component_value_before_ident_loc := Some !previous_token_loc;
          `Ident
        | `Ident, Token.(WHITESPACE _ | COMMENT _) -> `Ident
        (* If we see a colon after an ident, we're throwing an error as this means there's
           probably a missing semicolon
        *)
        | `Ident, Token.COLON ->
          let loc_end =
            Option.value
              ~default:!previous_token_loc
              !non_whitespace_component_value_before_ident_loc
          in
          (* Consume remaining tokens. This ensures any stop exceptions have priority over
             a parse error. *)
          Parse_queue.consume_and_ignore ~while_matches:(Not (Is EOF)) tokens;
          Parse_queue.raise
            ~loc:(Location.of_positions ~start:name_loc.loc_start ~end_:loc_end.loc_end)
            "Declaration is missing a semicolon"
        | _ ->
          non_whitespace_component_value_before_ident_loc := None;
          `Nothing
      in
      (*=Finding the last non-whitespace component value location to be the end of the 
         declaration that is missing the semicolon.

         Ex:

         ```css
         flex: 1 1 auto
         -------------^ This is the component value location we want to retrieve
         display: flex;
         ```
      *)
      (match token with
       | Token.WHITESPACE _ -> ()
       | _ -> previous_token_loc := component_value_loc);
      seen_so_far := new_value
  in
  let declaration_value_with_loc =
    let maybe_declaration_value_list =
      Parse_queue.with_loc
        ~context:(Some context)
        ~f:
          (consume_component_value_list
             ~maybe_throw_error:check_maybe_missing_semicolon
             ~remove_surrounding_whitespaces:true
             ~allow_ocaml_code:true
             ~while_:(Not (Any [ Is SEMICOLON; Is EOF; Is RIGHT_BRACE ])))
        tokens
    in
    match maybe_declaration_value_list with
    | None ->
      (* This branch allows us to have a location for empty declaration values, which are
         allowed for custom properties *)
      let loc_end = Parse_queue.peek_exn tokens |> snd in
      let loc =
        Location.of_positions ~start:colon_loc.loc_start ~end_:loc_end.loc_start
      in
      [], loc
    | Some value -> value
  in
  if (* We have to dequeue the semicolon if it exists even if we don't require the
        terminating semicolon. We've already implicitly checked for EOF and RIGHT_BRACE in
        [consume_component_value_list], so we don't need to check again
     *)
     Parse_queue.dequeue_and_check_if_next_token_matches
       ~matches:(Not (Is SEMICOLON))
       tokens
  then (
    (* Get the location of the last element in the declaration value list, which should be
       a non-whitespace component value. If that doesn't exist, return the location of the
       entire list
    *)
    let declaration_value_loc =
      fst declaration_value_with_loc
      |> List.last
      |> Option.value_map ~f:snd ~default:(snd declaration_value_with_loc)
    in
    let loc = Location.merge ~start:name_loc ~end_:declaration_value_loc in
    Parse_queue.handle_recoverable_error
      tokens
      (Missing_semicolon_at_end_of_declaration_list { loc }));
  match declaration_value_with_loc with
  | [], loc when not is_custom_property ->
    (* Consume the closing brace in the case that we are partial-parsing within an
       incomplete block *)
    ignore
    @@ Parse_queue.with_context
         ~f:(fun tokens -> Parse_queue.dequeue tokens)
         context
         tokens;
    Parse_queue.raise ~loc "Declaration value cannot be empty"
  | declaration_value, declaration_value_loc ->
    let component_values, is_important =
      process_important_from_declaration_value declaration_value
    in
    let is_empty_declaration =
      List.fold_until
        declaration_value
        ~init:()
        ~f:(fun () -> function
          | Component_value.Whitespace _, _ | Component_value.Comment _, _ -> Continue ()
          | _ -> Stop false)
        ~finish:(fun () -> true)
    in
    (match (not is_custom_property) && is_empty_declaration with
     | true ->
       Parse_queue.raise ~loc:declaration_value_loc "Declaration value cannot be empty"
     | false ->
       { Declaration.name = (name, name_loc), name_comments
       ; value = component_values, declaration_value_loc
       ; important = is_important
       })

and consume_at_rule tokens : At_rule.t =
  let at_rule_name, at_token_loc =
    Parse_queue.require_next_token_to_match
      ~matches:(Is AT_KEYWORD)
      ~error_msg:(fun token ->
        (* This shouldn't ever happen unless we've forgotten to check if an at-keyword
           exists before using this function
        *)
        [%string "Expected '@<identifier>' but got %{token#Token}"])
      tokens
  in
  let at_rule_name = Ident_like.to_string at_rule_name in
  (*=Make sure to remove leading whitespace as we always add a whitespace after the at
     rule name. 

     This is required because this 
     ```css
     @media(max-height: 500px) 
     ```

     would round-trip into

     ```css
     @media (max-height: 500px) 
     ```

     Which is a different AST but carries the same semantic meaning
  *)
  let prelude_with_loc =
    (* Unlike qualified rules, some at-rules are allowed to have empty preludes. These
       empty preludes should parse as [None] and not an empty list *)
    let%bind.Option prelude, loc =
      Parse_queue.with_loc
        ~f:
          (consume_component_value_list
             ~allow_ocaml_code:false
             ~remove_surrounding_whitespaces:true
             ~while_:(Not (Any [ Is EOF; Is LEFT_BRACE; Is SEMICOLON ])))
        tokens
    in
    match prelude with
    | [] -> None
    | prelude -> Some (prelude, loc)
  in
  let block, block_loc =
    match Parse_queue.peek tokens with
    | Some (Token.SEMICOLON, _) ->
      let semicolon_token = Parse_queue.dequeue_exn tokens in
      None, Location.from_token semicolon_token
    | Some (Token.LEFT_BRACE, _) ->
      (match String.lowercase at_rule_name with
       | "layer" | "media" | "supports" | "when" | "else" | "container" ->
         (*=Special-case some at-rules so that we can parse their contents as style
            blocks. 

            All [conditional group rules] can be special-cased as they can contain
            any rule that is normally allowed at the top-level of a stylesheet

            https://www.w3.org/TR/css-conditional-3/
            https://www.w3.org/TR/css-conditional-4/
            https://www.w3.org/TR/css-conditional-5/
         *)
         let block, block_loc = Parse_queue.with_loc_exn ~f:consume_style_block tokens in
         Some (At_rule.Style_block (block, block_loc)), block_loc
       | _ ->
         let block, loc =
           Parse_queue.with_loc_exn ~f:consume_declaration_list_block tokens
         in
         Some (At_rule.Declaration_list (block, loc)), loc)
    | None ->
      Parse_queue.raise
        ~loc:at_token_loc
        "Unexpected end of file while parsing at-rule. Please contact the maintainers of \
         the CSS parser."
    | Some token ->
      Parse_queue.throw_error_for_token token ~f:(fun token ->
        [%string
          "Error while parsing at-rule. Expected a semicolon or start of block but got \
           %{token#Token}"])
  in
  let loc = Location.merge ~start:at_token_loc ~end_:block_loc in
  { At_rule.name = at_rule_name; prelude = prelude_with_loc; block; loc }

and consume_declaration_list tokens ~while_ : Declaration_list.Element.t list =
  Parse_queue.with_context
    Block
    ~f:(fun tokens ->
      Parse_queue.process_into_list tokens ~while_ ~f:(function
        | Token.WHITESPACE _ | SEMICOLON -> None
        | COMMENT comment ->
          Some
            (Declaration_list.Element.Comment
               (comment, Parse_queue.dequeue_exn tokens |> Location.from_token))
        | AT_KEYWORD _ ->
          let at_rule = (Parse_queue.with_loc_exn ~f:consume_at_rule) tokens in
          Some (At_rule at_rule)
        | _ ->
          (match is_declaration_or_qualified_rule tokens with
           | `Declaration ambiguity ->
             let decl =
               Parse_queue.with_loc_exn ~f:(consume_declaration ~ambiguity) tokens
             in
             Some (Declaration decl)
           | `Qualified_rule ->
             let rule = Parse_queue.with_loc_exn ~f:consume_qualified_rule tokens in
             Some (Qualified_rule rule))))
    tokens

and consume_declaration_list_block tokens : Declaration_list.Element.t list =
  Parse_queue.require_next_token_to_match_and_ignore
    ~matches:(Is LEFT_BRACE)
    ~error_msg:(fun _ -> "Declaration list block must start with an opening left brace")
    tokens;
  let declarations =
    consume_declaration_list ~while_:(Not (Any [ Is RIGHT_BRACE; Is EOF ])) tokens
  in
  Parse_queue.require_next_token_to_match_and_ignore
    ~matches:(Is RIGHT_BRACE)
    ~error_msg:(fun _ -> "Missing closing right brace for declaration list block.")
    tokens;
  declarations

and consume_block
  (* We don't always want to remove the surrounding whitespace because we're not sure that
     the whitespace is always significant. This may cause some roundtrippability issues,
     but if the white-space is significant then that's better than potentially modifying
     the user's CSS in ways that result in unexpected behavior
  *)
  ~remove_surrounding_whitespaces
  ~allow_ocaml_code
  ~kind
  tokens
  =
  let match_start_of_block, match_end_of_block, kind_string =
    match kind with
    | `Brace -> Token.is_left_brace, Token.is_right_brace, "brace"
    | `Paren -> Token.is_left_paren, Token.is_right_paren, "paren"
    | `Bracket -> Token.is_left_bracket, Token.is_right_bracket, "bracket"
  in
  Parse_queue.require_next_token_to_match_and_ignore
    ~matches:(Matches match_start_of_block)
    ~error_msg:(fun _ -> [%string "Block must start with an opening left %{kind_string}"])
    tokens;
  let block_items =
    consume_component_value_list
      tokens
      ~remove_surrounding_whitespaces
      ~allow_ocaml_code
      ~while_:(Not (Any [ Is EOF; Matches match_end_of_block ]))
  in
  Parse_queue.require_next_token_to_match_and_ignore
    ~matches:(Matches match_end_of_block)
    ~error_msg:(fun _ -> [%string "Block must end with a closing right %{kind_string}"])
    tokens;
  block_items

and consume_component_value_list
  :  remove_surrounding_whitespaces:bool -> allow_ocaml_code:bool
  -> (* [maybe_throw_token_error] is passed the token that started the parsing, as well as
        the component value that is parsed from this iteration of the loop.
     *)
     ?maybe_throw_error:(Token.t -> Component_value.t with_loc -> unit)
  -> while_:Parse_queue.Match.t -> Parse_queue.t -> Component_value.t with_loc list
  =
  fun ~remove_surrounding_whitespaces
    ~allow_ocaml_code
    ?(maybe_throw_error = fun _ _ -> ())
    ~(while_ : Parse_queue.Match.t)
    tokens ->
  if remove_surrounding_whitespaces then Parse_queue.consume_and_ignore_whitespaces tokens;
  let items = ref Reversed_list.[] in
  Parse_queue.process
    ~while_
    ~f:(fun token ->
      let component_value =
        consume_component_value ~remove_surrounding_whitespaces ~allow_ocaml_code tokens
      in
      items := component_value :: !items;
      maybe_throw_error token component_value)
    tokens;
  let items =
    match remove_surrounding_whitespaces with
    | true -> remove_whitespace_from_end_of_component_values_list !items
    | false -> !items
  in
  Reversed_list.rev items

and consume_block_as_component_value
  ~remove_surrounding_whitespaces
  ~allow_ocaml_code
  ~kind
  tokens
  =
  let block_items, loc =
    Parse_queue.with_loc_exn
      ~f:(consume_block ~remove_surrounding_whitespaces ~allow_ocaml_code ~kind)
      tokens
  in
  match kind with
  | `Brace -> Component_value.Brace_block block_items, loc
  | `Paren -> Paren_block block_items, loc
  | `Bracket -> Bracket_block block_items, loc

(* The location returned is the location of the arguments to the function *)
and consume_function_items_as_component_values ?(allow_ocaml_code = false) tokens =
  let items =
    consume_component_value_list
      ~remove_surrounding_whitespaces:true
      ~allow_ocaml_code
      ~while_:(Not (Any [ Is EOF; Is RIGHT_PAREN ]))
      tokens
  in
  Parse_queue.require_next_token_to_match_and_ignore
    ~matches:(Is RIGHT_PAREN)
    ~error_msg:(fun token ->
      [%string
        "Error while parsing function. Expected closing paren but got %{token#Token}"])
    tokens;
  items

(* The location returned is the location of the arguments to the function *)
and consume_function_items_as_selectors tokens =
  let function_items, function_items_loc =
    Parse_queue.with_loc_exn
      ~f:
        (consume_complex_selector_list
           ~list_terminator:Token.is_right_paren
           ~allow_relative:true)
      tokens
  in
  Parse_queue.require_next_token_to_match_and_ignore
    ~matches:(Is RIGHT_PAREN)
    ~error_msg:(fun token ->
      [%string
        "Error while parsing function. Expected closing paren but got %{token#Token}"])
    tokens;
  function_items, function_items_loc

and consume_component_value
  ~remove_surrounding_whitespaces
  ?(allow_ocaml_code = false)
  tokens
  =
  let potentially_function_or_block =
    (* These functions parse the leading character within the function body itself, so we
       cannot advance the queue for these *)
    match Parse_queue.peek_token_exn tokens with
    | FUNCTION fn_name ->
      let loc = Parse_queue.dequeue_exn tokens |> Location.from_token in
      let function_items, function_items_loc =
        Parse_queue.with_loc_exn
          ~f:(consume_function_items_as_component_values ~allow_ocaml_code)
          tokens
      in
      let fn_name = Ident_like.to_string fn_name in
      ( Component_value.Function ((fn_name, loc), function_items)
      , Location.merge ~start:loc ~end_:function_items_loc )
      |> Option.return
    | LEFT_BRACE ->
      consume_block_as_component_value
        ~remove_surrounding_whitespaces
        ~allow_ocaml_code
        ~kind:`Brace
        tokens
      |> Option.return
    | LEFT_PAREN ->
      consume_block_as_component_value
        ~remove_surrounding_whitespaces
        ~allow_ocaml_code
        ~kind:`Paren
        tokens
      |> Option.return
    | LEFT_BRACKET ->
      consume_block_as_component_value
        ~remove_surrounding_whitespaces
        ~allow_ocaml_code
        ~kind:`Bracket
        tokens
      |> Option.return
    | _ -> None
  in
  match potentially_function_or_block with
  | Some fn_or_block -> fn_or_block
  | None ->
    (* We only need to check if the next token is Ocaml code here because the other branch
       should be checking its contents within the [potentially_function_or_block] branches
    *)
    if not allow_ocaml_code then Parse_queue.maybe_throw_ocaml_code_error tokens;
    let token, token_loc =
      (* Peek first so we can give the interpolation token the proper context *)
      let token, _ = Parse_queue.peek_exn tokens in
      match token with
      | OCAML_CODE _ ->
        Parse_queue.with_context
          Interpolation
          ~f:(fun tokens -> Parse_queue.dequeue_exn tokens)
          tokens
      | _ -> Parse_queue.dequeue_exn tokens
    in
    let component_value =
      match token with
      | Token.WHITESPACE value -> Component_value.Whitespace value
      | STRING str -> String str
      | IDENT ident -> Ident (Ident_like.to_string ident, token_loc)
      | PERCENTAGE p -> Percentage p
      | URL url -> Url url
      | DELIM d -> Delim d
      | COMMENT c -> Comment (c, token_loc)
      | OCAML_CODE c -> Ocaml_code (c, token_loc)
      | HASH (h, flag) -> Hash (Ident_like.to_string h, flag)
      | NUMBER n -> Number n
      | DIMENSION (number, num_type, exponent, dimension) ->
        Dimension (number, num_type, exponent, Ident_like.to_string dimension)
      | COLON -> Colon
      | CDO -> Cdo
      | CDC -> Cdc
      | COMMA -> Comma
      | SEMICOLON -> Semicolon
      | RIGHT_BRACKET -> Right_bracket
      | RIGHT_BRACE -> Right_brace
      | RIGHT_PAREN -> Right_paren
      | BAD_STRING str -> Bad_string str
      | BAD_URL url -> Bad_string url
      | AT_KEYWORD name -> At_keyword (Ident_like.to_string name)
      | token ->
        let loc = Parse_queue.dequeue_exn tokens |> Location.from_token in
        Parse_queue.throw_error_for_token (token, loc) ~f:(fun token ->
          [%string
            "Encountered unexpected token %{token#Token} when parsing component value"])
    in
    component_value, token_loc
;;

let stylesheet ~(parsing_config : Parsing_config.t) tokens =
  let tokens = Parse_queue.of_list ~parsing_config tokens in
  consume_list_of_rules tokens
;;

let style_block_contents ~(parsing_config : Parsing_config.t) tokens =
  let tokens = Parse_queue.of_list ~parsing_config tokens in
  match
    Parse_queue.with_loc
      ~f:(fun tokens -> consume_style_block_contents ~while_:(Not (Is EOF)) tokens)
      tokens
  with
  | None ->
    let _, loc =
      Parse_queue.require_next_token_to_match
        ~error_msg:(fun _ ->
          "No EOF token found when consuming empty style block declaration. This is a \
           parser error, please contact the maintainers of the CSS parser")
        ~matches:(Is EOF)
        tokens
    in
    [], loc
  | Some style_block_contents -> style_block_contents
;;

let for_apply_style tokens =
  let tokens =
    Parse_queue.of_list ~parsing_config:Parsing_config.ignore_recoverable_errors tokens
  in
  match
    Parse_queue.with_loc
      ~f:
        (consume_component_value_list
           ~remove_surrounding_whitespaces:false
           ~allow_ocaml_code:true
           ~while_:(Not (Is EOF)))
      tokens
  with
  | None ->
    let _, loc =
      Parse_queue.require_next_token_to_match
        ~error_msg:(fun _ ->
          "No EOF token found empty CSS string. This is a parser error, please contact \
           the maintainers of the CSS parser")
        ~matches:(Is EOF)
        tokens
    in
    [], loc
  | Some apply_style_sheet -> apply_style_sheet
;;
