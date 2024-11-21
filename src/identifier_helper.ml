open! Core
open Ppxlib

type 'a t =
  { string : 'a -> Identifier_kind.t -> string
  ; pattern : 'a -> pattern
  ; expression : 'a -> expression
  ; raise_if_not_all_utilized : here:Source_code_position.t -> unit
  ; remove_expr_from_all : unit -> unit
  }

let create ~to_string ~prefix ~hash ~loc =
  let open (val Ast_builder.make loc) in
  (* Keeps track of all the strings and which values they've been accessed by. *)
  let unused_identifier_table = String.Table.create () in
  let string index kind =
    let identifier = {%string|%{prefix}%{hash}__%{to_string index}|} in
    Hashtbl.update unused_identifier_table identifier ~f:(function
      | None -> Identifier_kind.Set.singleton kind
      | Some existing -> Set.add existing kind);
    identifier
  in
  let pattern index =
    let pattern_string = string index Pattern in
    ppat_var (Located.mk pattern_string)
  in
  let expression index =
    let expr_string = string index Expression in
    pexp_ident (Located.mk (Lident expr_string))
  in
  let raise_if_not_all_utilized ~(here : Source_code_position.t) =
    let unutilized_items =
      Hashtbl.filter unused_identifier_table ~f:(fun data ->
        not (Set.equal data Identifier_kind.all))
      |> Hashtbl.map ~f:(Set.diff Identifier_kind.all)
      |> Hashtbl.to_alist
    in
    match unutilized_items with
    | [] -> ()
    | _ ->
      let error_message =
        Sexp.to_string_hum
          [%message
            "BUG while creating hoisted module. Please report this bug!"
              (here : Source_code_position.t)
              "following identifiers have not been called symmetrically. Missing the \
               associated call"
              (unutilized_items : (string * Identifier_kind.Set.t) list)]
      in
      Location.raise_errorf
        ~loc:{ loc_start = here; loc_end = here; loc_ghost = true }
        "%s"
        error_message
  in
  (* Allows removing a specific value from each keys set. This allows you to just remove
     the expressions for a value that may need to call each pattern more than once *)
  let remove_pattern_or_expr_from_all t =
    Hashtbl.map_inplace unused_identifier_table ~f:(fun data -> Set.remove data t)
  in
  let remove_expr_from_all () = remove_pattern_or_expr_from_all Expression in
  { string; pattern; expression; raise_if_not_all_utilized; remove_expr_from_all }
;;

let pattern t ~index = t.pattern index
let expression t ~index = t.expression index
let string t ~index kind = t.string index kind

let assert_expression_and_pattern_symmetry ?(here = Stdlib.Lexing.dummy_pos) t =
  t.raise_if_not_all_utilized ~here
;;
