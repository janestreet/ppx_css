open! Core
open Css_parser_common

type token_with_loc = Token.t * Location.t [@@deriving sexp_of]

let dummy_loc =
  { Location.loc_start = Lexing.dummy_pos; loc_end = Lexing.dummy_pos; loc_ghost = true }
;;

let throw_error_for_token ~f (token, { Location.loc_start; loc_end; _ }) =
  raise
    (Errors.Parse_error { start_pos = loc_start; end_pos = loc_end; message = f token })
;;

let raise ~loc:{ Location.loc_start; loc_end; _ } msg =
  raise (Errors.Parse_error { start_pos = loc_start; end_pos = loc_end; message = msg })
;;

let raise_no_location msg = raise ~loc:dummy_loc msg

module Match = struct
  type t =
    | Not of t
    | Any of t list
    | Is : 'a Token.Typed_variant.t -> t
    | Matches of (Token.t -> bool)
    | Equals of Token.t

  let rec f to_ (token, loc) =
    match to_ with
    | Not t -> not (f t (token, loc))
    | Any t_list -> List.exists ~f:(fun t -> f t (token, loc)) t_list
    | Matches matches -> matches token
    | Is variant ->
      [%equal: Token.Typed_variant.Packed.t]
        { f = T variant }
        (Token.Typed_variant.which token)
    | Equals other_token -> Token.equal token other_token
  ;;
end

module Typed_match = struct
  type 'a t = Is : 'a Token.Typed_variant.t -> 'a t

  type 'a matches =
    | Yes of 'a
    | No

  let f : type a. a t -> token_with_loc -> a matches =
    fun match_ (token, _loc) ->
    match match_ with
    | Is v ->
      (match Token.Typed_variant.get v token with
       | None -> No
       | Some value -> Yes value)
  ;;
end

type t =
  { queue : token_with_loc Queue.t
  ; parsing_config : Parsing_config.t
  ; context : Context.t Stack.t
  }

let of_list ~parsing_config items =
  { queue = Queue.of_list items; parsing_config; context = Stack.create () }
;;

let to_list { queue; _ } = Queue.to_list queue
let peek t = Core.Queue.peek t.queue
let peek_exn t = Core.Queue.peek_exn t.queue
let peek_token tokens = peek tokens |> Option.map ~f:Tuple2.get1

let peek_token_exn tokens =
  match peek tokens with
  | None -> raise_no_location "Expected a token but none were found"
  | Some token -> Tuple2.get1 token
;;

let get_nth_token_exn ~n tokens = Queue.get tokens.queue n |> Tuple2.get1

let stop_if_past_end ~(token : token_with_loc) t =
  match t.parsing_config.partial_parsing_behavior with
  | No_partial_parsing -> ()
  | Stop_parsing_after_reaching { pos_cnum; pos_bol; pos_lnum = stop_line; _ } ->
    let cur_pos = (snd token).loc_end in
    let context = Stack.top t.context in
    let cur_char = cur_pos.pos_cnum - cur_pos.pos_bol in
    let stop_char = pos_cnum - pos_bol in
    let next_token_is_eof =
      (* Also stop if we are about to reach the EOF token to prevent parse errors from
         peeking ahead. *)
      match peek t with
      | Some (EOF, _loc) -> true
      | None | Some _ -> false
    in
    let passed_line = cur_pos.pos_lnum > stop_line in
    let passed_column_on_current_line =
      cur_pos.pos_lnum = stop_line && cur_char > stop_char
    in
    if next_token_is_eof || passed_line || passed_column_on_current_line
    then Core.raise (Partial_parsing_behavior.Reached_stop_position_with_context context)
    else ()
;;

let dequeue t =
  match Core.Queue.dequeue t.queue with
  | Some token ->
    stop_if_past_end ~token t;
    Some token
  | None -> None
;;

let dequeue_exn ?error_msg t =
  let item =
    let token_before_dequeue = peek t in
    match error_msg with
    | None ->
      let token = Core.Queue.dequeue_exn t.queue in
      stop_if_past_end ~token t;
      token
    | Some error_msg ->
      (match dequeue t with
       | None ->
         (match token_before_dequeue with
          | Some token -> throw_error_for_token token ~f:(fun _ -> force error_msg)
          | None -> raise_no_location (force error_msg))
       | Some item -> item)
  in
  item
;;

let dequeue_and_ignore_exn t = dequeue_exn t |> (ignore : token_with_loc -> unit)

let dequeue_if_matches ~f tokens =
  match peek tokens with
  | Some token when Match.f f token ->
    dequeue_and_ignore_exn tokens;
    true
  | Some _ | None -> false
;;

let dequeue_and_ignore_if_matches ~f tokens =
  let (_ : bool) = dequeue_if_matches ~f tokens in
  ()
;;

(* If the queue is empty we will also return false *)
let dequeue_and_check_if_next_token_matches ~matches tokens =
  match peek tokens with
  | None -> false
  | Some token -> Match.f matches token
;;

(* Convenience method for throwing errors and also retrieving values. *)
let require_next_token_to_match ~matches ~error_msg tokens =
  let token =
    (* This should always have a next token, but not using the [exn] version here so that
       we can throw the proper error message
    *)
    let token_before_dequeue = peek tokens in
    match dequeue tokens with
    | None ->
      (* This should be called after already visiting at least one token, so the previous
         items should never be empty *)
      (match token_before_dequeue with
       | Some token -> throw_error_for_token token ~f:error_msg
       | None -> throw_error_for_token ~f:error_msg (Token.EOF, dummy_loc))
    | Some token -> token
  in
  match Match.f matches token with
  | false -> throw_error_for_token ~f:error_msg token
  | true -> token
;;

let require_next_token_to_match_and_ignore ~matches ~error_msg tokens =
  let (_ : Token.t * Location.t) =
    require_next_token_to_match ~matches ~error_msg tokens
  in
  ()
;;

(* Convenience method for throwing errors and also retrieving values. *)
let require_next_token_to_match ~matches ~error_msg tokens =
  let token =
    match dequeue tokens with
    | None ->
      (* This should be called after already visiting at least one token, so the previous
         items should never be empty *)
      raise_no_location
        "Error while checking if next token matches. No tokens left in queue"
    | Some token -> token
  in
  match Typed_match.f matches token with
  | No -> throw_error_for_token ~f:error_msg token
  | Yes value -> value, snd token
;;

let token v ~error_message tokens =
  require_next_token_to_match ~matches:(Is v) ~error_msg:error_message tokens
;;

let drain ~while_ ~f t =
  let f token =
    stop_if_past_end ~token t;
    f token
  in
  Queue.drain ~while_ ~f t.queue
;;

let process ~while_ ~f t =
  let prev_head = ref None in
  let should_continue_loop t =
    match peek t with
    | None ->
      raise_no_location
        "Unexpected end of file while processing queue. This is a parser error, please \
         contact the maintainers of the CSS parser."
    | Some token ->
      (match
         Option.is_none !prev_head || not (phys_equal (Option.value_exn !prev_head) token)
       with
       | true ->
         prev_head := Some token;
         Match.f while_ token
       | false ->
         raise
           ~loc:(snd token)
           "Potential infinite loop detected while processing CSS. This is a parser \
            error, please contact the maintainers of the CSS parser.")
  in
  while should_continue_loop t do
    let token, _ = peek_exn t in
    f token
  done
;;

let fold_until { queue; _ } = Core.Queue.fold_until queue

let throw_error_if_next_token_matches ~error_msg ~f tokens =
  match peek_exn tokens |> Match.f f with
  | true ->
    let loc = dequeue_exn tokens |> Tuple2.get2 in
    raise ~loc error_msg
  | false -> ()
;;

let maybe_throw_ocaml_code_error tokens =
  match peek_exn tokens with
  | Token.OCAML_CODE _, loc ->
    raise ~loc "OCaml interpolation is only allowed within a declaration value"
  | _ -> ()
;;

let process_into_list ?(allow_ocaml_code = false) ~while_ ~f tokens =
  let items = ref Reversed_list.[] in
  process
    ~while_
    ~f:(fun token ->
      (match allow_ocaml_code with
       | true -> ()
       | false -> maybe_throw_ocaml_code_error tokens);
      match f token with
      | Some list_item -> items := list_item :: !items
      | None -> dequeue_and_ignore_exn tokens)
    tokens;
  Reversed_list.rev !items
;;

let consume_and_ignore ~while_matches tokens =
  drain ~f:(fun _ -> ()) ~while_:(Match.f while_matches) tokens
;;

let consume_and_ignore_whitespaces = consume_and_ignore ~while_matches:(Is WHITESPACE)

let consume_comments_only tokens =
  process_into_list
    ~while_:(Is COMMENT)
    ~f:(function
      | Token.COMMENT comment ->
        let loc = dequeue_exn tokens |> Location.from_token in
        Some (comment, loc)
      (* This should stop before we hit anything other than comments *)
      | _ ->
        let loc = dequeue_exn tokens |> Location.from_token in
        raise
          ~loc
          "Tried to consume invalid token while consuming comments. Please contact the \
           maintainers of the CSS parser.")
    tokens
;;

let consume_comment_and_whitespace tokens =
  process_into_list
    ~while_:(Any [ Is COMMENT; Is WHITESPACE ])
    ~f:(fun token ->
      let loc = dequeue_exn tokens |> Location.from_token in
      match token with
      | Token.WHITESPACE _ -> Some (`Whitespace loc)
      | Token.COMMENT comment -> Some (`Comment (comment, loc))
      | _ ->
        (* This should stop before we hit anything other than a comment or whitespace *)
        raise
          ~loc
          "Tried to consume invalid token while consuming whitespaces and comments. \
           Please contact the maintainers of the CSS parser.")
    tokens
;;

let filter_whitespaces =
  List.filter_map ~f:(function
    | `Whitespace _ -> None
    | `Comment comment -> Some comment)
;;

let consume_comments_and_ignore_whitespaces tokens =
  consume_comment_and_whitespace tokens |> filter_whitespaces
;;

let with_context new_ctx ~f tokens =
  Stack.push tokens.context new_ctx;
  let result = f tokens in
  let _ : Context.t option = Stack.pop tokens.context in
  result
;;

(** This function should _never_ mutate [tokens] itself, only [f] should mutate [tokens]. *)
let with_loc ?(context = None) ~f tokens =
  let first_token =
    match peek tokens with
    | None -> raise_no_location "Unable to retrieve location due to unexpected EOF."
    | Some token -> token
  in
  let val_ =
    match context with
    | Some context -> with_context context ~f tokens
    | None -> f tokens
  in
  let loc_1 = Location.from_token first_token in
  let loc_next = peek_exn tokens |> Location.from_token in
  (* Check to see if the next location is the same as the very first location we checked.
     If so, we have not parsed anything, so the location should be None
  *)
  match Location.equal loc_1 loc_next with
  | true -> None
  | false ->
    (* If the two locations are not equal, we can assume we parsed something *)
    let start = loc_1.loc_start
    and
      (* The start of the next token in the queue should be the same as the end of the
         previous token in the queue, which was the last token parsed by [f] *)
      end_
      =
      loc_next.loc_start
    in
    let loc = Location.of_positions ~start ~end_ in
    Some (val_, loc)
;;

(** Just like [with_loc], this function should __never__ mutate [tokens] *)
let with_loc_exn ?(context = None) ?(here = Stdlib.Lexing.dummy_pos) ~f tokens =
  let token_before_dequeue = peek tokens in
  match with_loc ~context ~f tokens with
  | None ->
    (* This acts as a default error message if we didn't parse anything when something was
       expected. Ideally, [f] will have its own error message
    *)
    let token_list =
      to_list tokens
      |> List.map ~f:(fun (token, { loc_start; loc_end; _ }) ->
        [%string "%{token#Token} (location: %{loc_start#Position}, %{loc_end#Position})"])
      |> String.concat ~sep:", "
    in
    let error_msg =
      [%string
        {|
Expected to parse something but got nothing.
Call pos: %{here#Position} 
If there are any remaining tokens, this is definitely an error in the CSS 
parser. Please contact the maintainers of the parser. 
Remaining tokens: %{token_list}
|}]
    in
    (match token_before_dequeue with
     | Some token -> throw_error_for_token token ~f:(fun _ -> error_msg)
     | None -> raise_no_location error_msg)
  | Some f_with_loc -> f_with_loc
;;

let handle_recoverable_error t recoverable_error =
  t.parsing_config.handle_recoverable_error recoverable_error
;;
