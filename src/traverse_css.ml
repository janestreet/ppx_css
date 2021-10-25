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
  | Ident s, loc when prev_is_dot -> false, (Ident (f (`Class s)), loc)
  | Hash s, loc -> false, (Hash (f (`Id s)), loc)
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

let transform ~pos s =
  let hash =
    let filename = Ppx_here_expander.expand_filename pos.pos_fname in
    let hash_prefix = 10 in
    Md5.digest_string (filename ^ s) |> Md5.to_hex |> Fn.flip String.prefix hash_prefix
  in
  let parsed = Stylesheet.of_string ~pos s in
  let mapping = String.Table.create () in
  let sheet =
    map_stylesheet parsed ~f:(function `Class identifier | `Id identifier ->
      let ret = sprintf "%s_hash_%s" identifier hash in
      String.Table.add mapping ~key:(sprintf "%s" identifier) ~data:ret
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
