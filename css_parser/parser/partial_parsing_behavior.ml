open! Core
open Css_parser_common

exception Reached_stop_position_with_context of Context.t option
[@@deriving sexp_of ~nonportable__magic_unsafe_in_parallel_programs]

type t =
  | No_partial_parsing
  | Stop_parsing_after_reaching of Position.t
