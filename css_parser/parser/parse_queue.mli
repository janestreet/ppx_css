open! Core
open Css_parser_common

type token_with_loc = Token.t * Location.t [@@deriving sexp_of]

(* These methods return ['a] so that they can be used within match arms. They should
   always raise exceptions when called *)
val throw_error_for_token : f:(Token.t -> string) -> token_with_loc -> 'a
val raise : loc:Location.t -> string -> 'a
val raise_no_location : string -> 'a

module Match : sig
  (* Convenience module for matching against specific things. 

     - [Is] will match on the [Token.Typed_variant.t], which means that it will
     match on the type of the token, disregarding the value
     - [Matches] allows using a custom function which takes a [Token.t] and returns
     a boolean
     - [Equals] is an exact match function

     [Not] and [Any] are convenience methods. [Not] is just like [not], and [Any] 
     will return [true] if any of the elements in [t list] return true
  *)
  type t =
    | Not of t
    | Any of t list
    | Is : 'a Token.Typed_variant.t -> t
    | Matches of (Token.t -> bool)
    | Equals of Token.t

  val f : t -> token_with_loc -> bool
end

module Typed_match : sig
  type 'a t = Is : 'a Token.Typed_variant.t -> 'a t

  type 'a matches =
    | Yes of 'a
    | No

  val f : 'a t -> token_with_loc -> 'a matches
end

type t

(* These methods are based on [Core.Queue] *)
val of_list : parsing_config:Parsing_config.t -> token_with_loc list -> t
val to_list : t -> token_with_loc list
val dequeue : t -> token_with_loc option
val drain : while_:(token_with_loc -> bool) -> f:(token_with_loc -> unit) -> t -> unit

(* Unlike [Core.dequeue_exn], this function allows you to raise a custom error message *)
val dequeue_exn : ?error_msg:string Lazy.t -> t -> token_with_loc
val dequeue_if_matches : f:Match.t -> t -> bool
val dequeue_and_ignore_exn : t -> unit
val dequeue_and_ignore_if_matches : f:Match.t -> t -> unit

(* Throws an error if the next token doesn't match [matches]. Returns the token otherwise *)
val require_next_token_to_match
  :  matches:'a Typed_match.t
  -> error_msg:(Token.t -> string)
  -> t
  -> 'a * Location.t

val token
  :  'a Token.Typed_variant.t
  -> error_message:(Token.t -> string)
  -> t
  -> 'a * Location.t

val dequeue_and_check_if_next_token_matches : matches:Match.t -> t -> bool

(* As this does not return a value, it can be a bit more permissive in how it matches
   the next token. Accepts any variant of [Match.t]
*)
val require_next_token_to_match_and_ignore
  :  matches:Match.t
  -> error_msg:(Token.t -> string)
  -> t
  -> unit

val peek : t -> token_with_loc option
val peek_exn : t -> token_with_loc

(* Convenience methods for when location is not really necessary, like for lookaheads *)
val peek_token : t -> Token.t option

(* Wrapper around [peek_token] that throws an error if [None] is returned. Does not use
   [Queue.peek_exn]
*)
val peek_token_exn : t -> Token.t

(* Used for lookaheads. Is done in constant time due to Queues using arrays for their 
   implementations.

   [n] is 0-indexed

   This is a separate function mainly due to the fact that the others expose [Queue.peek]
   and this one exposes [Queue.get], which throws an error on out of bounds.
*)
val get_nth_token_exn : n:int -> t -> Token.t

(** Unlike drain, process does not dequeue the token. Preventing infinite loops by making
    sure that the first element in the queue is not the same as in the last loop using
    [phys_equal]. This is __very__ rudimentary, but good enough for the CSS parsers needs *)
val process : while_:Match.t -> f:(Token.t -> unit) -> t -> unit

(* This is just [Queue.fold_until], but is very useful as a tool to do an unbounded
   lookahead without mutating the parse queue and the [prev] stack *)
val fold_until
  :  t
  -> init:'a
  -> f:local_ ('a -> token_with_loc -> ('a, 'b) Base.Continue_or_stop.t)
  -> finish:local_ ('a -> 'b)
  -> 'b

val throw_error_if_next_token_matches : error_msg:string -> f:Match.t -> t -> unit
(* *)

val maybe_throw_ocaml_code_error : t -> unit

(* Takes the value inside of [Some] and adds it to the list. If [None] is returned, we
    will dequeue the token and not do anything with it. 

    Note: As this utilizes [process] under the hood, The user must dequeue the token(s)
    that are utilized whenever [Some] is returned, or the function will error out in the
    same fashion as [process]. 

    This function is mainly useful to act as a one-token lookahead, which then passes 
    the token queue to a function which will process the queue.
*)
val process_into_list
  :  ?allow_ocaml_code:bool
  -> while_:Match.t
  -> f:(Token.t -> 'a option)
  -> t
  -> 'a list

val consume_and_ignore : while_matches:Match.t -> t -> unit
val consume_and_ignore_whitespaces : t -> unit
val consume_comments_only : t -> (string * Location.t) list
val handle_recoverable_error : t -> Recoverable_error.t -> unit

val consume_comment_and_whitespace
  :  t
  -> [ `Comment of string * Location.t | `Whitespace of Location.t ] list

val consume_comments_and_ignore_whitespaces : t -> (string * Location.t) list

(** Wrap [f] by pushing then popping a [Context.t] from the context stack *)
val with_context : Context.t -> f:(t -> 'a) -> t -> 'a

(** [with_loc] retrieves the location of the next token before [f] is run, and then the
    location of the last token that was processed after [f] is run. Optionally, wrap the
    inner call to f with [with_context] if a [Context.t] is provided.

    This function should _never_ mutate [tokens] itself, only [f] should mutate [tokens] *)
val with_loc : ?context:Context.t option -> f:(t -> 'a) -> t -> ('a * Location.t) option

val with_loc_exn
  :  ?context:Context.t option
  -> here:[%call_pos]
  -> f:(t -> 'a)
  -> t
  -> 'a * Location.t
