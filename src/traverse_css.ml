open! Core
open! Ppxlib
open Css_jane

let map_loc (v, loc) ~f = f v, loc

module Identifier_kind = struct
  module T = struct
    type t =
      | Class
      | Id
      | Variable
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

module Prev_delimeter = struct
  type t =
    | Other
    | Dot
    | Colon
end

open Prev_delimeter

(* Hashes ".a" within :not(.a). We are taking a "not hash by default" approach rather than
   immediately hashing every identifier in the AST to not break existing apps. *)
let hash_the_contents_of_these_selector_functions =
  String.Set.of_list [ "not"; "has"; "where"; "is" ]
;;

let rec fold_c_value ~rewrite ~dont_hash_prefixes ~f prev v =
  match prev, v with
  | _, ((Component_value.Delim "." as d), loc) -> Dot, (d, loc)
  | _, ((Delim ":" as d), loc) -> Colon, (d, loc)
  | Dot, (Ident s, loc) -> Other, (Ident (f (`Class s) loc), loc)
  | _, (Hash s, loc) -> Other, (Hash (f (`Id s) loc), loc)
  | Colon, (Function (((fn_name, _) as first), second), loc)
    when Set.mem hash_the_contents_of_these_selector_functions fn_name ->
    let component_value =
      let second =
        Tuple2.map_fst
          second
          ~f:(map_component_value_list ~rewrite ~f ~dont_hash_prefixes)
      in
      Component_value.Function (first, second)
    in
    Other, (component_value, loc)
  | _, other -> Other, other

and map_component_value_list ~f ~rewrite ~dont_hash_prefixes =
  List.folding_map ~init:Other ~f:(fold_c_value ~rewrite ~dont_hash_prefixes ~f)
;;

let map_stylesheet ~rewrite ~dont_hash_prefixes stylesheet ~f =
  let mapper =
    object
      inherit Css_jane.Traverse.map as super

      method! style_rule (style_rule : Style_rule.t) =
        let prelude =
          map_loc
            style_rule.prelude
            ~f:
              (List.folding_map
                 ~init:Other
                 ~f:(fold_c_value ~rewrite ~f ~dont_hash_prefixes))
        in
        super#style_rule { style_rule with prelude }

      method! declaration (declaration : Declaration.t) =
        let name, loc = declaration.name in
        let name =
          match String.is_prefix name ~prefix:"--" with
          | true -> f (`Variable name) loc
          | false -> name
        in
        let name = name, loc in
        let declaration = { declaration with name } in
        super#declaration declaration

      method! component_value (component_value : Component_value.t) =
        let component_value =
          match component_value with
          | Function ((("var", _) as first), (((Ident s, loc) :: remaining, _) as second))
            when String.is_prefix s ~prefix:"--" ->
            let second =
              Tuple2.map_fst second ~f:(fun _ ->
                (Component_value.Ident (f (`Variable s) loc), loc) :: remaining)
            in
            Component_value.Function (first, second)
          | _ -> component_value
        in
        super#component_value component_value
    end
  in
  mapper#stylesheet stylesheet
;;

(* Iterates over class, id, and variables in the file *)
let iter_identifiers ~rewrite ~dont_hash_prefixes stylesheet ~f =
  let f ((`Class identifier | `Id identifier | `Variable identifier) as case) _loc =
    f case;
    identifier
  in
  (ignore : Stylesheet.t -> unit)
    (map_stylesheet stylesheet ~rewrite ~f ~dont_hash_prefixes)
;;

let fix_identifier =
  let swap_kebab_case =
    String.map ~f:(function
      | '-' -> '_'
      | x -> x)
  in
  Fn.compose swap_kebab_case (String.chop_prefix_if_exists ~prefix:"--")
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
         Hashtbl.find fixed_to_original fixed_identifier
       in
       (match previously_computed_ocaml_identifier with
        | None ->
          Hashtbl.set fixed_to_original ~key:fixed_identifier ~data:original_identifier;
          fixed_identifier
        | Some previously_computed_ocaml_identifier ->
          (match
             String.equal previously_computed_ocaml_identifier original_identifier
           with
           | true -> fixed_identifier
           | false ->
             raise_due_to_collision_with_newly_minted_identifier
               ~loc
               ~previously_computed_ocaml_identifier
               ~original_identifier
               ~fixed_identifier)))
;;

let string_constant ~loc l =
  let open (val Ast_builder.make loc) in
  pexp_constant (Pconst_string (l, loc, Some ""))
;;

let raise_if_unused_rewrite_identifiers ~loc ~unused_rewrite_identifiers ~unused_allow_set
  =
  let unused_rewrite_identifiers =
    Set.of_hash_set (module String) unused_rewrite_identifiers
  in
  let identifier_allow_list =
    let { Preprocess_arguments.dont_hash; rewrite; dont_hash_prefixes = _ } =
      Preprocess_arguments.get ()
    in
    Set.union_list (module String) [ dont_hash; Map.key_set rewrite; unused_allow_set ]
  in
  match Set.is_subset unused_rewrite_identifiers ~of_:identifier_allow_list with
  | true -> ()
  | false ->
    Location.raise_errorf
      ~loc
      "Unused keys: %s"
      (Sexp.to_string_hum ([%sexp_of: String.Set.t] unused_rewrite_identifiers))
;;

let raise_if_unused_prefixes ~loc ~used_prefixes ~dont_hash_prefixes =
  let unused_prefixes =
    Set.diff
      (String.Set.of_list dont_hash_prefixes)
      (String.Set.of_hash_set used_prefixes)
  in
  let prefix_allow_list =
    let { Preprocess_arguments.dont_hash = _; rewrite = _; dont_hash_prefixes } =
      Preprocess_arguments.get ()
    in
    dont_hash_prefixes
  in
  match Set.is_subset unused_prefixes ~of_:prefix_allow_list with
  | true -> ()
  | false ->
    Location.raise_errorf
      ~loc
      "Unused prefixes: %s"
      (Sexp.to_string_hum ([%sexp_of: String.Set.t] unused_prefixes))
;;

module Transform = struct
  type result =
    { css_string : string
    ; identifier_mapping : (Identifier_kind.Set.t * expression) String.Table.t
    ; reference_order : expression list
    }

  let f
        ~loc
        ~pos
        ~rewrite
        ~css_string:s
        ~dont_hash_prefixes
        ~unused_allow_set
        ~always_hash
    =
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
    let identifier_mapping = String.Table.create () in
    let original_identifiers = String.Hash_set.create () in
    let reference_order = ref Reversed_list.[] in
    let unused_rewrite_identifiers = String.Hash_set.of_list (Map.keys rewrite) in
    iter_identifiers
      ~rewrite
      ~dont_hash_prefixes
      parsed
      ~f:(fun (`Class identifier | `Id identifier | `Variable identifier) ->
        Hash_set.add original_identifiers identifier;
        match Set.mem always_hash identifier with
        | true -> ()
        | false -> Hash_set.remove unused_rewrite_identifiers identifier);
    raise_if_unused_rewrite_identifiers ~loc ~unused_rewrite_identifiers ~unused_allow_set;
    let original_identifiers = Set.of_hash_set (module String) original_identifiers in
    let fixed_to_original = String.Table.create () in
    let used_prefixes = String.Hash_set.create () in
    let is_matched_by_a_prefix =
      let dont_hash_prefixes =
        (* Sorted from most general to least general (i.e. shorter prefix to longest
           prefix)*)
        List.dedup_and_sort
          ~compare:(fun a_1 b_1 ->
            Comparable.lexicographic
              [ (fun a b -> Comparable.lift Int.ascending ~f:String.length a b)
              ; String.compare
              ]
              a_1
              b_1)
          dont_hash_prefixes
      in
      fun identifier ->
        List.exists dont_hash_prefixes ~f:(fun prefix ->
          match String.is_prefix identifier ~prefix with
          | true ->
            Hash_set.add used_prefixes prefix;
            true
          | false -> false)
    in
    let sheet =
      map_stylesheet
        ~rewrite
        ~dont_hash_prefixes
        parsed
        ~f:
          (fun
            ((`Class identifier | `Id identifier | `Variable identifier) as token) loc ->
            let ocaml_identifier =
              get_ocaml_identifier identifier ~loc ~original_identifiers ~fixed_to_original
            in
            let hashed =
              lazy
                (let ret = sprintf "%s_hash_%s" identifier hash in
                 ret, string_constant ~loc ret)
            in
            let ret, expression =
              match
                `Always_hash (Set.mem always_hash identifier), Map.find rewrite identifier
              with
              | `Always_hash true, _ -> force hashed
              | `Always_hash false, None ->
                (match is_matched_by_a_prefix identifier with
                 | false -> force hashed
                 | true -> identifier, string_constant ~loc identifier)
              | ( `Always_hash false
                , Some
                    { pexp_desc = Pexp_constant (Pconst_string (identifier, _, _))
                    ; pexp_loc = loc
                    ; _
                    } ) -> identifier, string_constant ~loc identifier
              | `Always_hash false, Some expression_to_use ->
                (reference_order := Reversed_list.(expression_to_use :: !reference_order));
                "%s", expression_to_use
            in
            let identifier_kind =
              match token with
              | `Class _ -> Identifier_kind.Class
              | `Id _ -> Id
              | `Variable _ -> Variable
            in
            Hashtbl.update identifier_mapping ocaml_identifier ~f:(fun prev ->
              match prev with
              | None -> Identifier_kind.Set.singleton identifier_kind, expression
              | Some (prev, expression) -> Set.add prev identifier_kind, expression);
            ret)
    in
    raise_if_unused_prefixes ~loc ~used_prefixes ~dont_hash_prefixes;
    let css_string = Stylesheet.to_string_hum sheet in
    let css_string =
      sprintf
        "\n/* %s */\n\n%s"
        (Ppx_here_expander.expand_filename pos.pos_fname)
        (String.strip css_string)
    in
    { css_string
    ; identifier_mapping
    ; reference_order = Reversed_list.rev !reference_order
    }
  ;;
end

module Get_all_identifiers = struct
  type result =
    { variables : string list
    ; identifiers : (string * [ `Both | `Only_class | `Only_id ]) list
    }
  [@@deriving sexp_of]

  let css_variables stylesheet =
    let out = String.Hash_set.create () in
    iter_identifiers
      ~rewrite:String.Map.empty
      ~dont_hash_prefixes:[]
      stylesheet
      ~f:(function
        | `Class _ | `Id _ -> ()
        | `Variable identifier -> Hash_set.add out identifier);
    String.Set.of_hash_set out
  ;;

  let css_identifiers stylesheet =
    let out = String.Hash_set.create () in
    iter_identifiers
      ~rewrite:String.Map.empty
      ~dont_hash_prefixes:[]
      stylesheet
      ~f:(fun (`Class identifier | `Id identifier | `Variable identifier) ->
        Hash_set.add out identifier);
    String.Set.of_hash_set out
  ;;

  let ocaml_identifiers stylesheet =
    let identifiers = String.Table.create () in
    let variables = String.Hash_set.create () in
    let fixed_to_original = String.Table.create () in
    let original_identifiers = css_identifiers stylesheet in
    iter_identifiers
      ~rewrite:String.Map.empty
      ~dont_hash_prefixes:[]
      stylesheet
      ~f:(fun current_item ->
        let (`Class identifier | `Id identifier | `Variable identifier) = current_item in
        let fixed_identifier =
          get_ocaml_identifier
            identifier
            ~loc:Location.none
            ~original_identifiers
            ~fixed_to_original
        in
        match current_item with
        | `Variable _ -> Hash_set.add variables fixed_identifier
        | `Class _ ->
          Hashtbl.update identifiers fixed_identifier ~f:(function
            | None | Some `Only_class -> `Only_class
            | Some `Only_id | Some `Both -> `Both)
        | `Id _ ->
          Hashtbl.update identifiers fixed_identifier ~f:(function
            | None | Some `Only_id -> `Only_id
            | Some `Only_class | Some `Both -> `Both));
    { identifiers = Hashtbl.to_alist identifiers; variables = Hash_set.to_list variables }
  ;;
end

module For_testing = struct
  let map_style_sheet s ~rewrite ~dont_hash_prefixes ~f =
    map_stylesheet s ~f ~rewrite ~dont_hash_prefixes
  ;;
end
