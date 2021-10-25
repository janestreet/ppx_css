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

let delim_coma = ( Component_value.Delim ","
                 , Common.location_none )

module MapRule = Map.Make(struct
    type t  = Component_value.t list
    let compare =
      Comparator.compare_list (fun l1 l2 ->
          Comparator.component_value
            (l1, Common.location_none)
            (l2, Common.location_none)
        )
  end)

(** The type of the map contains both :

    - The declaration inside the selector
    - The selector Location

*)
type t = (Declaration_list.t * Location.t) list MapRule.t

type acc = Component_value.t list * Component_value.t list list

(** Group all the selectors together, using a given delimiter *)
let group_selector
  : string -> Component_value.t with_loc list with_loc -> Component_value.t list list
  = fun delim elems ->

    let add_element
      : acc -> Component_value.t with_loc -> acc
      = fun (acc, prev) elem ->
        match (fst elem) with
        | Delim s when String.equal s delim -> [], (List.rev acc)::prev
        | other -> other::acc, prev
    in
    let last, prev = List.fold_left
        (fst elems)
        ~init:([], [])
        ~f:add_element in
    (List.rev last)::prev

(** Add a new style in the map. *)
let add_style
  : Style_rule.t -> t -> t
  = fun {prelude; block; loc} map ->
    List.fold_left (group_selector "," prelude)
      ~init:map
      ~f:(fun map group ->
          MapRule.update group
            (function
              | None ->
                (* There is no declaration yet, just add this one *)
                Some [(block, loc)]
              | Some tl ->

                (* The declaration is already present.

                   For each of them, we check if the declaration is overriden
                   by the new one, and update the list.

                   The news declarations are added in a new block (a second
                   pass may be necessary to join all the remaining elements
                   together.
                *)
                Some (Common.update_declarations (block, loc) tl))
            map)

module ReversedMapRule = Map.Make(struct
    type t  = Declaration_list.t * Location.t

    (* Use a custom comparaison without the location *)
    let compare l1 l2 =
      Comparator.declaration_list
        (fst l1)
        (fst l2)
  end)
type splitted_rules' = (Component_value.t list list) ReversedMapRule.t

(** Extract all the styles, and return them as a Rule.t sequence *)
let extract_style
  : t -> Rule.t Seq.t
  = fun map ->
    (* First, iterate all the values and match the identical one together *)

    let table:splitted_rules' =
      MapRule.fold
        (fun k values map' ->

           (* Each element may be present multiple times in the declaration. We
              have te extract each of them *)
           List.fold_left values
             ~init:map'
             ~f:(fun map' (v, loc) ->

                 ReversedMapRule.update (v, loc)
                   (function
                     | None -> Some [k]
                     | Some tl -> Some (k::tl))
                   map' ))
        map
        ReversedMapRule.empty in

    (* The rebuild the rules *)
    ReversedMapRule.to_seq table
    |> Seq.map (fun ((block, loc), k) ->

        let selectors =
          List.fold_left k
            ~init:[]
            ~f:(fun acc v ->
                let selectors = List.map
                    v
                    ~f:(fun x -> x , Common.location_none) in
                let tail = List.append selectors acc in
                delim_coma::tail) in

        let prelude =
          match selectors with
          | (Component_value.Delim ",", _)::tl ->
            (* Remove the first delimiter element *)
            ( tl
            , Common.location_none)
          | _->
            ( selectors
            , Common.location_none )
        in
        Rule.Style_rule (Style_rule.{prelude; block; loc}))

let empty = MapRule.empty
