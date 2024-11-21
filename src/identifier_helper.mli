open! Core
open Ppxlib

(* Helper that makes it easier to consistently generate the same string but in different formats *)
type 'a t

val create
  :  to_string:('a -> string)
  -> prefix:string
  -> hash:string
  -> loc:location
  -> 'a t

val pattern : 'a t -> index:'a -> pattern
val expression : 'a t -> index:'a -> expression
val string : 'a t -> index:'a -> Identifier_kind.t -> string

(** [assert_expression_and_pattern_symmetry] asserts that 
    both [expression] and [pattern] have been called for
    all identifiers. *)
val assert_expression_and_pattern_symmetry : ?here:Stdlib.Lexing.position -> 'a t -> unit
