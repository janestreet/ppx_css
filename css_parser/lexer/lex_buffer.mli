(** This module exists as a thin wrapper around the default Sedlexing. It mainly just
    re-exposes specific methods from the [Sedlexing] module. It also adds a few
    convenience methods, mainly [of_utf8_string], [utf8], and [utf8_between]. *)
open! Core

type t

(* Creates a [t] from a string and sets filename and position if provided. If no [pos] is
   provided, will set linenumber to 1, which tells [Sedlexing] to track the current
   position in the lexbuffer. If that is not set, [Sedlexing] will not track the position
*)
val of_utf8_string : ?filename:string -> ?pos:Lexing.position -> string -> t

(* Internal functions to Sedlexing *)
val __private__next_int : t -> int
val mark : t -> int -> unit
val next : t -> Uchar.t option
val backtrack : t -> int
val start : t -> unit

(* Convenience methods *)
val utf8 : ?start:int -> ?end_:int -> t -> string
val utf8_between : start:int -> end_:int -> t -> string

(* Exposed functions from Sedlexing *)
val rollback : t -> unit
val lexing_positions : t -> Lexing.position * Lexing.position

(* Takes two positions and turns them into a location *)
val make_loc : ?loc_ghost:bool -> Lexing.position -> Lexing.position -> Location.t

(* Used to get the location at the start and end of a function. Useful within match arms
   of percent-sedlex if intending to match on more lexemes within said function before
   returning a token

   Note that you have to call this after matching on the FIRST lexeme whose position you
   want. This will set [Location.loc_start] to the start of the last lexeme matched, which
   should be the FIRST lexeme you wish to be included in location calculation
*)
val with_loc : t -> f:(unit -> 'a) -> 'a * Location.t

(* Like [with_loc], but also gives you the [start_pos] so that it can be used within the
   function itself to do things like reporting in exceptions
*)
val with_loc' : t -> f:(start_pos:Lexing.position -> 'a) -> 'a * Location.t
