open! Core
open! Ppxlib
open Css_jane

type t =
  { css_string : string
  ; mapping : string String.Table.t
  }

let map_loc (v, loc) ~f = f v, loc

let fold_c_value ~f prev_is_dot = function
  | (Component_value.Delim "." as d), loc -> true, (d, loc)
  | Ident s, loc when prev_is_dot -> false, (Ident (f (`Class s) loc), loc)
  | Hash s, loc -> false, (Hash (f (`Id s) loc), loc)
  | other -> false, other
;;

let rec map_at_rule ~f (at_rule : Css_jane.At_rule.t) : Css_jane.At_rule.t =
  let prelude =
    map_loc at_rule.prelude ~f:(List.folding_map ~init:false ~f:(fold_c_value ~f))
  in
  let block =
    match at_rule.block with
    | Empty -> Css_jane.Brace_block.Empty
    | Declaration_list decl_list -> Declaration_list (map_decl_list decl_list ~f)
    | Stylesheet stylesheet -> Stylesheet (map_stylesheet ~f stylesheet)
  in
  { at_rule with prelude; block }

and map_decl_list list ~f =
  let open Css_jane.Declaration_list in
  let map_block = function
    | Declaration d -> Declaration d
    | At_rule a -> At_rule (map_at_rule ~f a)
  in
  map_loc list ~f:(List.map ~f:map_block)

and map_style_rule ~f (style_rule : Css_jane.Style_rule.t) =
  let prelude =
    style_rule.prelude |> map_loc ~f:(List.folding_map ~init:false ~f:(fold_c_value ~f))
  in
  let block = map_decl_list ~f style_rule.block in
  { style_rule with prelude; block }

and map_stylesheet (parsed : Css_jane.Stylesheet.t) ~f : Css_jane.Stylesheet.t =
  let open Css_jane.Rule in
  map_loc
    parsed
    ~f:
      (List.map ~f:(function
         | Style_rule style_rule -> Style_rule (map_style_rule ~f style_rule)
         | At_rule at_rule -> At_rule (map_at_rule ~f at_rule)))
;;

let fix_identifier =
  String.map ~f:(function
    | '-' -> '_'
    | x -> x)
;;

let raise_due_to_collision_with_existing_ident ~loc ~original_identifier ~fixed_identifier
  =
  Location.raise_errorf
    ~loc
    "Unsafe collision of names. Cannot rename '%s' to '%s' because '%s' already exists"
    original_identifier
    fixed_identifier
    fixed_identifier
;;

let raise_due_to_collision_with_newly_minted_identifier
      ~loc
      ~previously_computed_ocaml_identifier
      ~original_identifier
      ~fixed_identifier
  =
  Location.raise_errorf
    ~loc
    "Unsafe collisions of names. Two different unsafe names map to the same fixed name \
     which might lead to unintended results. Both '%s' and '%s' map to '%s'"
    previously_computed_ocaml_identifier
    original_identifier
    fixed_identifier
;;

let get_ocaml_identifier original_identifier ~loc ~original_identifiers ~fixed_to_original
  =
  match String.exists original_identifier ~f:(Char.equal '-') with
  | false -> original_identifier
  | true ->
    let fixed_identifier = fix_identifier original_identifier in
    (match Set.mem original_identifiers fixed_identifier with
     | true ->
       raise_due_to_collision_with_existing_ident
         ~loc
         ~original_identifier
         ~fixed_identifier
     | false ->
       let previously_computed_ocaml_identifier =
         String.Table.find fixed_to_original fixed_identifier
       in
       (match previously_computed_ocaml_identifier with
        | None ->
          String.Table.set fixed_to_original ~key:fixed_identifier ~data:original_identifier;
          fixed_identifier
        | Some previously_computed_ocaml_identifier ->
          (match String.equal previously_computed_ocaml_identifier original_identifier with
           | true -> fixed_identifier
           | false ->
             raise_due_to_collision_with_newly_minted_identifier
               ~loc
               ~previously_computed_ocaml_identifier
               ~original_identifier
               ~fixed_identifier)))
;;

let transform ~pos ~dont_hash_these s =
  let parsed = Stylesheet.of_string ~pos s in
  let hash =
    let filename = Ppx_here_expander.expand_filename pos.pos_fname in
    let hash_prefix = 10 in
    parsed
    |> Stylesheet.sexp_of_t
    |> Sexp.to_string_mach
    |> sprintf "%s:%s" filename
    |> Md5.digest_string
    |> Md5.to_hex
    |> Fn.flip String.prefix hash_prefix
  in
  let mapping = String.Table.create () in
  let original_identifiers = String.Hash_set.create () in
  map_stylesheet parsed ~f:(fun (`Class identifier | `Id identifier) _loc ->
    Hash_set.add original_identifiers identifier;
    identifier)
  |> (ignore : Stylesheet.t -> unit);
  let original_identifiers = Set.of_hash_set (module String) original_identifiers in
  let fixed_to_original = String.Table.create () in
  let sheet =
    map_stylesheet parsed ~f:(fun (`Class identifier | `Id identifier) loc ->
      let ocaml_identifier =
        get_ocaml_identifier identifier ~loc ~original_identifiers ~fixed_to_original
      in
      let ret =
        match Set.mem dont_hash_these identifier with
        | true -> identifier
        | false -> sprintf "%s_hash_%s" identifier hash
      in
      String.Table.add mapping ~key:(sprintf "%s" ocaml_identifier) ~data:ret
      |> (ignore : [ `Duplicate | `Ok ] -> unit);
      ret)
  in
  let css_string = Stylesheet.to_string_hum sheet in
  let css_string =
    sprintf
      "\n/* %s */\n\n%s"
      (Ppx_here_expander.expand_filename pos.pos_fname)
      (String.strip css_string)
  in
  { css_string; mapping }
;;
