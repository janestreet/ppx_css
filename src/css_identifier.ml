open! Core

module T = struct
  type t =
    | Id of string
    | Class of string
    | Variable of string
  [@@deriving compare, variants, sexp, hash, equal]
end

include Comparable.Make (T)
include Hashable.Make (T)
include T

let to_css_selector = function
  | Id id -> [%string {|#%{id}|}]
  | Class classname -> [%string {|.%{classname}|}]
  | Variable var_ -> [%string {|--%{var_}|}]
;;

let extract_css_identifier = function
  | Id id | Class id | Variable id -> id
;;

let extract_ocaml_identifier = function
  | Id id | Class id | Variable id -> Helper_utils.css_identifier_to_ocaml_identifier id
;;

let set_includes_class_or_id identifiers =
  let includes_id =
    Core.Set.exists identifiers ~f:(function
      | Id _ -> true
      | _ -> false)
  in
  let includes_class =
    Core.Set.exists identifiers ~f:(function
      | Class _ -> true
      | _ -> false)
  in
  match includes_id, includes_class with
  | true, true -> `Both
  | true, false -> `Only_id
  | false, true -> `Only_class
  | false, false -> `None
;;

let group_by_ocaml_identifier =
  Core.List.fold ~init:String.Map.empty ~f:(fun acc identifier ->
    let ocaml_identifier = extract_ocaml_identifier identifier in
    Core.Map.update acc ocaml_identifier ~f:(function
      | Some existing_identifiers -> Core.Set.add existing_identifiers identifier
      | None -> Set.singleton identifier))
;;
