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

module AtRule = Map.Make (struct
  type t = string * Component_value.t list

  let compare at1 at2 =
    let cmp = String.compare (fst at1) (fst at2) in
    if cmp <> 0
    then cmp
    else
      Comparator.compare_list
        (fun l1 l2 ->
          Comparator.component_value (l1, Common.location_none) (l2, Common.location_none))
        (snd at1)
        (snd at2)
  ;;
end)

type at_type =
  | Empty
  | Declaration of (Declaration_list.t * Location.t) list
  | Stylesheet of (Merge_style.t * ats)

and at_map_content = Location.t * at_type
and ats = at_map_content AtRule.t

type t = Merge_style.t * ats

let rec add_brace_block
  : Brace_block.t -> Location.t -> at_map_content option -> at_map_content option
  =
  fun block loc value ->
  match block, value with
  (* Empty element, update the existing one if any *)
  | Brace_block.Empty, _ -> Some (loc, Empty)
  (* New declarationList, juste add it *)
  | Brace_block.Declaration_list decls, None -> Some (loc, Declaration [ decls, loc ])
  | Brace_block.Declaration_list decls, Some (loc, Declaration decl2) ->
    Some
      (loc, Declaration (Common.update_declarations (decls, Common.location_none) decl2))
  | Brace_block.Stylesheet s, None ->
    let eval = add_css (Merge_style.empty, AtRule.empty) s in
    Some (loc, Stylesheet eval)
  | Brace_block.Stylesheet s, Some (loc, Stylesheet css) ->
    let eval = add_css css s in
    Some (loc, Stylesheet eval)
  (* Othe cases are not handled *)
  | _ -> None

(** Add a new @ definition *)
and add_at : Css_parser.Types.At_rule.t -> ats -> ats =
  fun { name; prelude; block; loc } map ->
  let prelude = List.map (fst prelude) ~f:fst in
  let key = fst name, prelude in
  AtRule.update key (add_brace_block block loc) map

and add_css : t -> Stylesheet.t -> t =
  fun (styles, atrules) css ->
  List.fold_left (fst css) ~init:(styles, atrules) ~f:(fun (styles, ats) -> function
    | Rule.At_rule r -> styles, add_at r ats
    | Rule.Style_rule r -> Merge_style.add_style r styles, ats)
;;

(** Helper function for retrieving the location *)
let get_loc : Rule.t -> Location.t = function
  | Rule.Style_rule t -> t.Style_rule.loc
  | Rule.At_rule t -> t.At_rule.loc
;;

let rec extract_at : ats -> Css_parser.Types.Rule.t Seq.t =
  fun map ->
  AtRule.to_seq map
  |> Seq.map (fun ((name, prelude), (loc, value)) ->
       let name = name, loc
       and prelude = List.map ~f:(fun x -> x, loc) prelude, loc in
       match value with
       | Stylesheet css ->
         let stylesheet = extract_css css in
         let block = Brace_block.Stylesheet stylesheet in
         Rule.At_rule At_rule.{ name; prelude; block; loc }
       | Empty ->
         let block = Brace_block.Empty in
         Rule.At_rule At_rule.{ name; prelude; block; loc }
       | Declaration decls ->
         let declarations =
           List.fold_left decls ~init:[] ~f:(fun acc (decl, _) ->
             let elems = fst decl in
             List.append elems acc)
         in
         let block = Brace_block.Declaration_list (declarations, loc) in
         Rule.At_rule At_rule.{ name; prelude; block; loc })

and extract_css : t -> Stylesheet.t =
  fun (styles, ats) ->
  let arr =
    Seq.append (extract_at ats) (Merge_style.extract_style styles) |> Array.of_seq
  in
  (* Sort the declaration in initial ordering (using the location attribute) *)
  Array.fast_sort ~cmp:(fun v1 v2 -> Stdlib.compare (get_loc v1) (get_loc v2)) arr;
  Array.to_list arr, Common.location_none
;;

let empty : t = Merge_style.empty, AtRule.empty
