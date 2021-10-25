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

let location_none = Location.none

(** Update the declaration list with this new property if the same property
    name is already present.

    If not return [None] *)
let merge_declations
  : Declaration_list.t -> Declaration_list.kind -> Declaration_list.t option
  = fun decls1 decl2 ->
    let declarations, loc = decls1 in
    let updated, list' =
      List.fold_left_map declarations
        ~init:false
        ~f:(fun res kind ->
            if Comparator.declaration_kind' kind decl2 = 0 then
              true, decl2
            else
              res, kind)
    in
    match updated with
    | false -> None
    | _ -> Some
             ( list'
             , loc )

(** Add all the declarations from [decl2] into the list [decl1]
    and return the list, and all the new declarations to add *)
let add_all_declarations
  : Declaration_list.t -> Declaration_list.t -> Declaration_list.t * Declaration_list.t
  = fun decls1 (decls2, loc2) ->

    let decls1, remain' = List.fold_left decls2
        ~init:(decls1, [])
        ~f:(fun (decls1, remain) new_declaration ->
            match merge_declations decls1 new_declaration with
            (* TODO : Handle empty property as None *)
            | None -> decls1, (Some new_declaration::remain)
            | Some decls1 -> decls1, remain
          ) in
    (* Remove all the unused properties *)
    let remain' = List.filter_map ~f:(fun x -> x) remain' in
    ( decls1
    , (remain', loc2) )


let update_declarations
  : (Declaration_list.t * Location.t) -> (Declaration_list.t * Location.t) list -> (Declaration_list.t * Location.t) list
  = fun (block, loc) existing ->
    let remain, tl = List.fold_left
        existing
        ~init:(block, [])
        ~f:(fun (block, prev) (declarations, location) ->

            let update, remain = add_all_declarations declarations block in
            remain, (update, location)::prev) in
    match fst remain with
    | [] -> tl
    | other -> (((other, loc), loc)::tl)
