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

open Css_parser.Types
open StdLabels

(** The module Comparator helps to compare two stylesheet together. *)

(** Compare two list in a safe way *)
let compare_list : ('a -> 'a -> int) -> 'a list -> 'a list -> int =
  fun cmp l1 l2 ->
  let length = List.compare_lengths l1 l2 in
  if length <> 0
  then length
  else
    List.fold_left2
      ~f:(fun res v1 v2 -> if res <> 0 then res else cmp v1 v2)
      ~init:0
      l1
      l2
;;

(** Compare each component without the loccation information *)
let rec component_value : Component_value.t with_loc -> Component_value.t with_loc -> int =
  fun v1 v2 ->
  let open Component_value in
  match fst v1, fst v2 with
  | Paren_block b1, Paren_block b2 | Bracket_block b1, Bracket_block b2 ->
    compare_list component_value b1 b2
  | Percentage v1, Percentage v2
  | Ident v1, Ident v2
  | String v1, String v2
  | Uri v1, Uri v2
  | Operator v1, Operator v2
  | Delim v1, Delim v2
  | Hash v1, Hash v2
  | Number v1, Number v2
  | Unicode_range v1, Unicode_range v2 -> String.compare v1 v2
  | Float_dimension v1, Float_dimension v2 -> Stdlib.compare v1 v2
  | Dimension v1, Dimension v2 -> Stdlib.compare v1 v2
  | Function (n1, v1), Function (n2, v2) ->
    let name1 = fst n1
    and name2 = fst n2 in
    let cmp = String.compare name1 name2 in
    if cmp <> 0 then cmp else compare_list component_value (fst v1) (fst v2)
  | v1, v2 -> Stdlib.compare v1 v2
;;

let rec brace_block : Brace_block.t -> Brace_block.t -> int =
  fun v1 v2 ->
  match v1, v2 with
  | Declaration_list l1, Declaration_list l2 -> declaration_list l1 l2
  | Stylesheet s1, Stylesheet s2 -> style_sheet s1 s2
  | _, _ -> Stdlib.compare v1 v2

and at_rule : At_rule.t -> At_rule.t -> int =
  fun v1 v2 ->
  let cmp = String.compare (fst v1.name) (fst v2.name) in
  if cmp <> 0
  then cmp
  else (
    let cmp = compare_list component_value (fst v1.prelude) (fst v2.prelude) in
    if cmp <> 0 then cmp else brace_block v1.block v2.block)

and declaration : Declaration.t -> Declaration.t -> int =
  fun v1 v2 ->
  let cmp = String.compare (fst v1.name) (fst v2.name) in
  if cmp <> 0
  then cmp
  else (
    let cmp = Stdlib.compare (fst v1.important) (fst v2.important) in
    if cmp <> 0 then cmp else compare_list component_value (fst v1.value) (fst v2.value))

and declaration_kind : Declaration_list.kind -> Declaration_list.kind -> int =
  fun v1 v2 ->
  match v1, v2 with
  | Declaration v1, Declaration v2 -> declaration v1 v2
  | At_rule v1, At_rule v2 -> at_rule v1 v2
  | _, _ -> Stdlib.compare v1 v2

and declaration_list : Declaration_list.t -> Declaration_list.t -> int =
  fun v1 v2 -> compare_list declaration_kind (fst v1) (fst v2)

and style_rule : Style_rule.t -> Style_rule.t -> int =
  fun v1 v2 ->
  let cmp = declaration_list v1.block v2.block in
  if cmp <> 0 then cmp else compare_list component_value (fst v1.prelude) (fst v2.prelude)

and rule : Rule.t -> Rule.t -> int =
  fun v1 v2 ->
  match v1, v2 with
  | Style_rule v1, Style_rule v2 -> style_rule v1 v2
  | At_rule v1, At_rule v2 -> at_rule v1 v2
  | _, _ -> Stdlib.compare v1 v2

and style_sheet : Stylesheet.t -> Stylesheet.t -> int =
  fun v1 v2 -> compare_list rule (fst v1) (fst v2)
;;

(** Compare two rules by name only *)
let at_rule' : At_rule.t -> At_rule.t -> int =
  fun v1 v2 ->
  let cmp = String.compare (fst v1.name) (fst v2.name) in
  if cmp <> 0 then cmp else compare_list component_value (fst v1.prelude) (fst v2.prelude)
;;

(** Compare two declarations by name only *)
let declaration' : Declaration.t -> Declaration.t -> int =
  fun v1 v2 -> String.compare (fst v1.name) (fst v2.name)
;;

(** Compare two declaration_kind by name only *)
let declaration_kind' : Declaration_list.kind -> Declaration_list.kind -> int =
  fun v1 v2 ->
  match v1, v2 with
  | Declaration v1, Declaration v2 -> declaration' v1 v2
  | At_rule v1, At_rule v2 -> at_rule' v1 v2
  | _, _ -> Stdlib.compare v1 v2
;;
