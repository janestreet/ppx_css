open! Core
open! Ppxlib
open Css_parser
module Preprocess_arguments = Ppx_css_syntax.Preprocess_arguments
module Graph_lib = Graph

let mapper ~(f : Css_identifier.t -> Location.t -> string) =
  object
    inherit Css_parser.Traverse.map as super

    method! selector_with_loc (selector, selector_loc) =
      let selector =
        match selector with
        | Id (ident, flag) -> Selector.Id (f (Id ident) selector_loc, flag)
        | Class ident -> Class (f (Class ident) selector_loc)
        | other -> other
      in
      (selector, selector_loc) |> super#selector_with_loc

    (* Fetching custom css property declarations. This assumes that all custom property 
       declarations are css variables, which is not always the case.

       We may want to consider parsing declarations with variants for custom and 
       non-custom sometime in the future *)
    method! declaration (declaration : Declaration.t) =
      let (name, name_loc), comments = declaration.name in
      let name =
        match String.is_prefix name ~prefix:"--" with
        | true -> f (Variable name) name_loc
        | false -> name
      in
      let name = name, name_loc in
      let declaration = { declaration with name = name, comments } in
      super#declaration declaration

    (* Fetching all variables that refer to a custom css property. We may want to
       consider parsing css vars as their own thing in the future *)
    method! component_value (component_value : Component_value.t) =
      let component_value =
        match component_value with
        | Function
            ((("var", _) as fn_name_with_loc), (Ident (s, ident_loc), loc) :: remaining)
          when String.is_prefix s ~prefix:"--" ->
          let args : Component_value.t with_loc list =
            (Ident (f (Variable s) loc, ident_loc), loc) :: remaining
          in
          Component_value.Function (fn_name_with_loc, args)
        | _ -> component_value
      in
      super#component_value component_value
  end
;;

let map_stylesheet stylesheet ~f =
  let mapper = mapper ~f in
  Tuple2.map_fst stylesheet ~f:(Map.map ~f:mapper#rule)
;;

let map_rule rule ~f =
  let mapper = mapper ~f in
  mapper#rule rule
;;

module Components = Graph_lib.Components.Make (Stylesheet_graph)

(* We only process the selector lists of specific selector functions. This selector list
   has been thought through and tested so that we can ensure that they are safe to lazily
   load and include in the strongly connected components calculation.

   I think :has, :where, and :is are relatively safe to use, as none of those will
   actually apply unless the selectors inside them are available in the DOM. However, if
   the selector is something like :has(div, .class1) or :where(.unhashed-class, .class2-hashed),
   then it must immediately be forced as the function can be applied whenever ANY of the
   selectors in the list are valid. :not must always be forced

   I think there may be other unconsidered edge cases, so functional selectors should
   be eagerly evaluated.
*)

let retrieve_the_contents_of_these_selector_functions = String.Set.of_list [ "has" ]

(* A set of all [At_rule]s that will be split into individual sheets. This is so that the
   constituent rules inside each at rule can be lazily instantiated.

   Only the [At_rule]s at the top level of the stylesheet will be split into multiple
   declarations, as it becomes increasingly more difficult to maintain ordering
   (and my sanity) if we allow for recursively breaking down the rules into stylesheets

   Note that other at-rules such as @media and @supports could potentially be split up,
   but those aren't used as frequently and can probably remain as eagerly applied.

   Example:
   ```css
   @layer test {
    .a {}
    .b {}
    .a + .b {}
   }
   ```

   becomes

   ```css
   @layer test {
    .a {}
   }

   @layer test {
    .b {}
   }

   @layer test {
    .a + .b {}
   }
   ```
*)
let splittable_at_rules = String.Set.of_list [ "layer" ]

let rec split_layers ((stylesheet, loc) : Stylesheet.t) : Stylesheet.t =
  ( List.fold
      stylesheet
      ~init:Reversed_list.[]
      ~f:(fun acc rule ->
        match rule with
        (* [At_rule]s within the [splittable_at_rules] list will be split into multiple
         rules that each contain one declaration within them *)
        | Rule.At_rule ({ name; prelude = _; block; _ } as at_rule)
          when Set.mem splittable_at_rules name ->
          (match block with
           (* If the block is empty, there's nothing to process *)
           | None -> rule :: acc
           (* [Declaration_list]s can contain key-value pairs of CSS declarations. I don't
              think `@layer` can contain a [Declaration_list], so for now I think it is okay
              to eagerly force this and not process [At_rule]s with [Declaration_list]s *)
           | Some (Declaration_list (_, loc)) ->
             Location.raise_errorf
               ~loc
               {|PPX_CSS Bug: Layer [At_rules]s should not contain [Declaration_list]s. Please report this bug.|}
           | Some (Style_block (block, block_loc)) ->
             (* Split each individual rule within the at-rule into a rule wrapped by 
                the at-rule and add it to the overall list of rules *)
             List.fold block ~init:acc ~f:(fun acc rule ->
               match rule with
               | Comment (comment, comment_loc) ->
                 let rule : Rule.t =
                   At_rule
                     { at_rule with
                       block =
                         Some
                           (Style_block ([ Comment (comment, comment_loc) ], comment_loc))
                     }
                 in
                 rule :: acc
               | Rule (rule, rule_loc) ->
                 let processed_rule, _processed_loc =
                   split_layers ([ rule ], block_loc)
                 in
                 List.fold processed_rule ~init:acc ~f:(fun acc sheet ->
                   let rule : Rule.t =
                     At_rule
                       { at_rule with
                         block = Some (Style_block ([ Rule (sheet, rule_loc) ], rule_loc))
                       }
                   in
                   rule :: acc)
               | Declaration (decl, decl_loc) ->
                 let rule : Rule.t =
                   At_rule
                     { at_rule with
                       block =
                         Some
                           (Declaration_list ([ Declaration (decl, decl_loc) ], decl_loc))
                     }
                 in
                 rule :: acc))
        (* All other [At_rule]s will be skipped over *)
        (* [Style_rule]s, [Qualified_rule]s, [Comment]s will not be processed *)
        | At_rule _ | Style_rule _ | Qualified_rule _ | Comment _ -> rule :: acc)
    |> Reversed_list.rev
  , loc )
;;

let is_hashed_identifier ~should_hash_identifier (identifier : Css_identifier.t) =
  match should_hash_identifier identifier with
  | `Hash -> true
  | `Dont_hash | `Dont_hash_prefixes _ -> false
;;

let remove_unhashed_identifiers
  ~(is_hashed_identifier : Css_identifier.t -> bool)
  identifiers
  =
  Set.filter identifiers ~f:is_hashed_identifier
;;

module Graph_node_identifiers = struct
  (* This is a 2d list because we need to represent a selector list. A selector list is
     something like

     ```css
     .div #some-id > .a,
     .b + .c,
     :has(.q + .r)  {
     }
     ```

     (index of t) = [x] and (index of t[x]) = [y]

     Each t[x] in the prelude is independently affected by the declarations in the rule.
     This means that the above example is saying the rule applies to

     [.div #some-id > .a] OR [.b + .c] OR [:has(.q + .r)]

     t[x][y] is an identifier that makes up one of the selectors that is required in order
     for the rule to apply

     Ex: t[0] in the example above would be [.div ; #some-id ; .a ]
  *)
  type t = selector_func_or_identifier list list

  and selector_func =
    { name : string
    ; selectors : t
    }

  and selector_func_or_identifier =
    | Selector_func of selector_func
    | Identifier of Css_identifier.t
    | Ampersand

  (* This function recursively iterates through the nested lists of selectors and computes
     whether or not the rule needs to be autoforced.

     (index of t) = [x] and (index of t[x]) = [y]

     We need to recursively check to make sure that at least one [x] returns true.
     If so, we must autoforce the rule.
  *)
  let rec should_be_autoforced ~is_top_level ~is_unhashed_identifier t =
    List.exists t ~f:(should_selector_be_autoforced ~is_top_level ~is_unhashed_identifier)

  (* Every identifier in t[x] must be an autoforced identifier for t[x] to be considered
     autoforceable

     We need to know which identifiers are unhashed because those should be excluded
     from the calculation
  *)
  and should_selector_be_autoforced ~is_top_level ~is_unhashed_identifier = function
    | [] -> true
    | selectors ->
      List.for_all
        selectors
        ~f:(should_identifier_be_autoforced ~is_top_level ~is_unhashed_identifier)

  and should_identifier_be_autoforced ~is_top_level ~is_unhashed_identifier = function
    | Selector_func { selectors; name } ->
      let is_an_unknown_function =
        not (Set.mem retrieve_the_contents_of_these_selector_functions name)
      in
      is_an_unknown_function
      || should_be_autoforced ~is_top_level ~is_unhashed_identifier selectors
    | Identifier identifier -> is_unhashed_identifier identifier
    (* If this is being called for a top-level selector, we would need to autoforce this 
       as it then refers to the scoping root. This means that it could refer to [:root]
       or something similiar to [:root] which ppx_css has no context about, so we 
       need to autoforce the ampersand

       https://www.w3.org/TR/css-nesting-1/#nest-selector
    *)
    | Ampersand -> is_top_level
  ;;

  (* If any nested rule inside of a top-level rule has a selector function that 
     uses an explicit ampersand, we will autoforce the top-level rule.
  *)
  let rec contains_ampersand t =
    List.exists t ~f:(function
      | Ampersand -> true
      | Selector_func { selectors; _ } -> List.exists selectors ~f:contains_ampersand
      | Identifier _ -> false)
  ;;

  let rec of_selectors ((selector_list, _loc) : Selector_list.t) =
    (* Process the [complex_selectors] *)
    List.fold
      selector_list
      ~init:[]
      ~f:(fun acc ((selectors, _selectors_loc), _selector_comments) ->
        let selector_list_item =
          (* Process the [compound_selector]s and [combinator]s *)
          List.fold selectors ~init:[] ~f:(fun acc (selector_combinator, _comments) ->
            match selector_combinator with
            | Combinator _ -> acc
            | Selector (compound_selector, _loc) ->
              (* Process the individual selectors *)
              List.fold
                ~init:acc
                compound_selector
                ~f:(fun acc ((selector, _comments), _loc) ->
                  match selector with
                  | Selector.Class ident -> Identifier (Class ident) :: acc
                  | Id (ident, _hash_flag) -> Identifier (Id ident) :: acc
                  | Ampersand -> Ampersand :: acc
                  (* We're processing both pseudoclass and pseudoelement selector 
                     functions that contain selectors here. We'll be checking later on to 
                     see if they should be included within the graph calculation *)
                  | Pseudoclass pseudo | Pseudoelement pseudo ->
                    (match pseudo with
                     | Function_with_selectors
                         (((fn_name, _fn_name_loc), _comments), selectors) ->
                       Selector_func
                         { name = fn_name; selectors = of_selectors selectors }
                       :: acc
                     | Ident _ | Function _ -> acc)
                  | _ -> acc))
        in
        selector_list_item :: acc)
  ;;

  (* Retrieves all utilized identifiers within [t] as a list *)
  let rec to_set ~read_contents_of_functions ~init t =
    List.fold t ~init ~f:(fun init ->
      List.fold ~init ~f:(fun init -> add_identifier ~init ~read_contents_of_functions))

  and add_identifier ~init ~read_contents_of_functions = function
    | Selector_func { selectors; name } ->
      let should_traverse_pseudoselector =
        read_contents_of_functions
        || Set.mem retrieve_the_contents_of_these_selector_functions name
      in
      if should_traverse_pseudoselector
      then to_set ~init ~read_contents_of_functions selectors
      else init
    | Identifier identifier -> Set.add init identifier
    (* The ampersand is only useful for seeing if this rule should be autoforced. It
       doesn't really affect the total amount of selectors, so we ignore it.*)
    | Ampersand -> init
  ;;

  let to_set ~read_contents_of_functions t =
    to_set ~init:Css_identifier.Set.empty ~read_contents_of_functions t
  ;;
end

(* The identifiers retrieved will be CSS identifiers converted to ocaml identifiers. It
   will only collect identifiers from classes and IDs

   ------

   When [read_contents_of_functions] is [false], this function takes into account the
   selector functions that it should retrieve identifiers from, and does discern
   between the selector lists that are present.

   Ex:

   ```css
   div, .a, .c + .b {}
   ```

   Will not produce any selectors as one of the selectors in the list needs to be
   auto-forced, so the entire rule will be auto-forced.


   ```css
   .a, .c + .b {}
   ```

   However, will produce the list [.a ; .c ; .b]

   If a selector function is processable, then the same rules apply to its selector list.

   ------

   When [read_contents_of_functions] is [true], the function will retrieve all identifiers
   regardless of if they are in a selector function or in a selector list.
*)
let rec get_identifiers_for_style_block ~read_contents_of_functions = function
  | `Declaration_list (declarations, _loc) ->
    List.fold declarations ~init:Css_identifier.Set.empty ~f:(fun acc -> function
      | Declaration_list.Element.At_rule (rule, _loc) ->
        Set.union
          acc
          (get_identifiers_for_style_rule ~read_contents_of_functions (Rule.At_rule rule))
      | Declaration _ | Qualified_rule _ | Comment _ -> acc)
  | `Style_block (block_elements, _loc) ->
    List.fold ~init:Css_identifier.Set.empty block_elements ~f:(fun acc -> function
      | Style_block.Block_element.Rule (rule, _rule_loc) ->
        Set.union acc (get_identifiers_for_style_rule ~read_contents_of_functions rule)
      | Declaration (decl, decl_loc) ->
        Set.union
          acc
          (get_identifiers_for_style_block
             ~read_contents_of_functions
             (`Declaration_list
               ([ Declaration_list.Element.Declaration (decl, decl_loc) ], decl_loc)))
      | Comment _ -> acc)

and get_identifiers_for_style_rule ~read_contents_of_functions = function
  | Qualified_rule _ | Comment _ -> Css_identifier.Set.empty
  | Style_rule { selectors; block; _ } ->
    let prelude_identifiers =
      Graph_node_identifiers.of_selectors selectors
      |> Graph_node_identifiers.to_set ~read_contents_of_functions
    in
    Set.union
      prelude_identifiers
      (get_identifiers_for_style_block ~read_contents_of_functions (`Style_block block))
  | At_rule { prelude = _; block; _ } ->
    (match block with
     | None -> Css_identifier.Set.empty
     | Some (At_rule.Declaration_list decl_list) ->
       get_identifiers_for_style_block
         ~read_contents_of_functions
         (`Declaration_list decl_list)
     | Some (Style_block block) ->
       get_identifiers_for_style_block ~read_contents_of_functions (`Style_block block))
;;

(* Checks to make sure all selectors in child rules do not have to be autoforced. This
   mainly deals with nested selector functions like [:not(&)] which should cause the parent
   rule to be autoforced
*)
let rec check_child_style_rules_for_autoforcing
  ~is_unhashed_identifier
  ({ Style_rule.selectors; _ } as rule)
  =
  let identifiers = Graph_node_identifiers.of_selectors selectors in
  let should_prelude_be_autoforced =
    List.exists
      ~f:(fun selector ->
        match Graph_node_identifiers.contains_ampersand selector with
        (* If this selector does not contain an ampersand, it has an implicit
           ampersand + descendant selector, which makes it so that it will not
           be autoforced so long as the parent isn't
        *)
        | false -> false
        | true ->
          Graph_node_identifiers.should_selector_be_autoforced
            ~is_top_level:false
            ~is_unhashed_identifier
            selector)
      identifiers
  in
  should_prelude_be_autoforced
  || check_should_style_rule_children_be_autoforced ~is_unhashed_identifier rule

and check_should_style_rule_children_be_autoforced
  ~is_unhashed_identifier
  { Style_rule.block = block, _; _ }
  =
  List.exists block ~f:(function
    | Rule (rule, _rule_loc) ->
      (match rule with
       | Style_rule rule ->
         check_child_style_rules_for_autoforcing ~is_unhashed_identifier rule
       (* If there's an at-rule nested within this style rule, we're going to eagerly force
       the parent rule. This is because there are several different ways that nested
       at-rules can interact with the parent style rule, and codifying all of those 
       interactions does not seem to be worth it. I doubt at-rules nested inside style
       rules will occur very frequently, either
       *)
       | At_rule _ -> true
       | Qualified_rule _ | Comment _ -> false)
    | Declaration _ | Comment _ -> false)
;;

let rec get_inner_rule (rule : Rule.t) =
  match rule with
  (* Processed [At_rule]s should only have a single element within their
     [Style_block]. It __must__ be a [Rule]. The rule will act as the top-level rule
     for this [At_rule]. Recursively extract the inner rule for use as
     the top-level identifiers *)
  | At_rule
      { name
      ; block = Some (Style_block (Rule (inner_rule, _inner_rule_loc) :: [], _))
      ; _
      }
    when Set.mem splittable_at_rules name -> get_inner_rule inner_rule
  (* All other [At_rule]s, including ones that were supposed to be processed but
     were seemingly processed incorrectly, will do the default *)
  | At_rule _ | Style_rule _ | Comment _ | Qualified_rule _ -> rule
;;

let retrieve_all_utilized_identifiers ~is_hashed_identifier rule =
  let rule = get_inner_rule rule in
  match rule with
  (* [At_rule]s that were not processed will have no utilized identifiers as
     they are currently eagerly forced *)
  | At_rule _ | Comment _ | Qualified_rule _ -> Css_identifier.Set.empty
  | Style_rule ({ selectors; _ } as style_rule) as rule ->
    let is_unhashed_identifier identifier = not (is_hashed_identifier identifier) in
    (* These are the identifiers from the top-level declaration/rule before
       traversing child rules. These are necessary because we need to know if
       the rule needs to be eagerly forced. If no identifiers are utilized
       at the top level, the entire rule needs to be eagerly forced *)
    let should_autoforce =
      Graph_node_identifiers.of_selectors selectors
      |> Graph_node_identifiers.should_be_autoforced
           ~is_top_level:true
           ~is_unhashed_identifier
    in
    let should_children_autoforce_parent =
      check_should_style_rule_children_be_autoforced ~is_unhashed_identifier style_rule
    in
    (match should_autoforce || should_children_autoforce_parent with
     | true -> Css_identifier.Set.empty
     | false ->
       get_identifiers_for_style_rule ~read_contents_of_functions:false rule
       (* Utilized identifiers have to have the unhashed identifiers removed *)
       |> remove_unhashed_identifiers ~is_hashed_identifier)
;;

module Graph = struct
  module Group_type = struct
    module T = struct
      type t =
        | Autoforced
        | Group of int [@nested "group_"]
      [@@deriving sexp, string, compare, hash]
    end

    include Comparable.Make (T)
    include Hashable.Make (T)
    include T
  end

  type result =
    { identifiers : Css_identifier.Set.t
    ; get_group_for_identifier : Css_identifier.t -> Group_type.t
    ; group_to_rule_indices :
        (Rule_id.t, Rule_id.comparator_witness) Nonempty_set.t Group_type.Map.t
    ; group_to_rules : Rule.t list Group_type.Map.t
    }

  let map_stylesheet_for_graph
    ~lazy_loading_optimization
    ~should_hash_identifier
    ((stylesheet : Rule.t Rule_id.Map.t), _loc)
    =
    match lazy_loading_optimization with
    | Preprocess_arguments.Lazy_graph ->
      let is_hashed_identifier = is_hashed_identifier ~should_hash_identifier in
      (* [all_identifiers] is the complete list of identifiers for the stylesheet,
       regardless of where they're from. It is used to see which identifiers are not being 
       utilized within a group, which gives us the list of identifiers that are being 
       autoforced *)
      let all_identifiers =
        Map.fold
          ~init:Css_identifier.Set.empty
          stylesheet
          ~f:(fun ~key:_ ~data:rule acc ->
            get_identifiers_for_style_rule ~read_contents_of_functions:true rule
            |> Set.union acc)
      in
      let utilized_identifiers_by_rule =
        Map.map stylesheet ~f:(fun rule ->
          retrieve_all_utilized_identifiers ~is_hashed_identifier rule)
      in
      let autoforced_rule_indices =
        Map.fold
          utilized_identifiers_by_rule
          ~init:Rule_id.Set.empty
          ~f:(fun ~key:rule_index ~data:identifiers acc ->
            match Set.is_empty identifiers with
            | true -> Set.add acc rule_index
            | _ -> acc)
      in
      (* A map of identifier -> top-level rule index that the identifier appears in *)
      let identifier_to_rule_indices =
        Map.fold
          utilized_identifiers_by_rule
          ~init:Css_identifier.Map.empty
          ~f:(fun ~key:rule_index ~data:utilized_identifiers_for_rule acc ->
            Set.fold utilized_identifiers_for_rule ~init:acc ~f:(fun acc identifier ->
              Map.update acc identifier ~f:(function
                | None -> Rule_id.Set.singleton rule_index
                | Some existing -> Set.add existing rule_index)))
      in
      (* [all_utilized_identifiers] ignores identifiers if they will not be considered for
       dependency generation. Non-utilized identifiers are treated similarly to top-level 
       tags such as [div]
      *)
      let all_utilized_identifiers =
        Map.fold
          utilized_identifiers_by_rule
          ~init:Css_identifier.Set.empty
          ~f:(fun ~key:_ ~data acc -> Set.union data acc)
      in
      let sheet_graph =
        let sheet_graph = Stylesheet_graph.create () in
        (* If two identifiers show up within the same rule, add an edge between them *)
        Map.iter utilized_identifiers_by_rule ~f:(fun deduped_identifiers ->
          (* Only using fold as a way to create edges between two vertices, 
           the resulting value doesn't actually matter and should always
           be an empty set.

           For position x in starting set [n(x), n(x + 1), n(x + 2), n(x + 3), .... n(x + k)]:
             Add vertex from n(x) -> [n(x + 1), n(x + 2), n(x + 3), ... n(x + k)]
          *)
          Set.fold deduped_identifiers ~init:deduped_identifiers ~f:(fun remainder hd ->
            let remainder = Set.remove remainder hd in
            Stylesheet_graph.add_vertex_if_needed sheet_graph hd;
            Set.iter remainder ~f:(Stylesheet_graph.add_edge sheet_graph hd);
            remainder)
          |> (ignore : Css_identifier.Set.t -> unit));
        sheet_graph
      in
      let _num_groups, get_group_num_from_identifier = Components.scc sheet_graph in
      (* Map of [Group_type.t] to a set of all rule indices that contain an identifier that
       is in the [Group_type.t] *)
      let group_to_rule_indices =
        (* This is a list of lists where list[x][y] is an identifier and x = the group
         number that the identifier belongs to *)
        let group_identifiers = Components.scc_list sheet_graph in
        List.foldi
          group_identifiers
          ~init:(Group_type.Map.singleton Group_type.Autoforced autoforced_rule_indices)
          ~f:(fun group_index acc -> function
          | [] ->
            Core.raise_s
              [%message
                "BUG "
                  [%here]
                  "unable to find identifiers for group. This means that strongly \
                   connected components were created without any vertices. This should \
                   not happen"]
          | identifiers ->
            let rule_indices =
              List.fold identifiers ~init:Rule_id.Set.empty ~f:(fun acc identifier ->
                match Map.find identifier_to_rule_indices identifier with
                | None ->
                  Core.raise_s
                    [%message
                      "BUG "
                        [%here]
                        "unable to find rule index for identifier. There was an issue \
                         aggregating the identifiers. This should not happen"]
                | Some rule_indices -> Set.union acc rule_indices)
            in
            Map.set
              acc
              ~key:(Group_type.Group group_index)
              ~data:(Set.diff rule_indices autoforced_rule_indices))
        |> Map.filter_map ~f:Nonempty_set.of_set
      in
      let autoforced_identifiers =
        (* Subtract the list of all utilized identifiers from the list of all identifiers
         so we have the list of all identifiers that are in rules that are autoforced
        *)
        Set.diff all_identifiers all_utilized_identifiers
      in
      (* All group numbers will be wrapped in a variant type so that we can discern if 
       a group is autoforced or not.
      *)
      let get_group_for_identifier identifier =
        match Set.mem autoforced_identifiers identifier with
        | true -> Group_type.Autoforced
        | false ->
          (match
             Or_error.try_with (fun () -> get_group_num_from_identifier identifier)
           with
           | Error _err ->
             Core.raise_s
               [%message
                 "BUG "
                   [%here]
                   "Unable to find group number for identifier"
                   (identifier : Css_identifier.t)
                   "this should not happen. Check to make sure CSS identifiers are being \
                    converted properly"]
           | Ok group_number ->
             (* We are filtering out the indices that are empty after we filter out
              autoforced rule indices. This check is to ensure that we aren't 
              erroneously filtering out rules that shouldn't have been *)
             (match Map.find group_to_rule_indices (Group_type.Group group_number) with
              | None ->
                Core.raise_s
                  [%message
                    "BUG in ppx_css"
                      [%here]
                      "autoforced identifier was not found in the set of autoforced \
                       identifiers. This should not happen. Please report this to the \
                       maintainers of ppx_css."]
              | Some _ -> ());
             Group group_number)
      in
      let group_to_rules =
        Map.map
          group_to_rule_indices
          ~f:
            (Nonempty_set.fold ~init:[] ~f:(fun acc rule_index ->
               (* This should always exist as it's what we've used to calculate which index 
                belongs to which rule.
               *)
               Map.find_exn stylesheet rule_index :: acc))
      in
      { get_group_for_identifier
      ; identifiers = all_utilized_identifiers
      ; group_to_rule_indices
      ; group_to_rules
      }
    | Eager | Default ->
      { get_group_for_identifier = (fun _ -> Group_type.Autoforced)
      ; identifiers = Css_identifier.Set.empty
      ; group_to_rule_indices =
          Group_type.Map.singleton
            Group_type.Autoforced
            (Map.keys stylesheet |> Nonempty_set.of_list_exn (module Rule_id))
      ; group_to_rules =
          Group_type.Map.singleton Group_type.Autoforced (Map.data stylesheet)
      }
  ;;
end

(* Iterates over class, id, and variables in the file *)
let iter_identifiers stylesheet ~f =
  let f
    ((Css_identifier.Class identifier | Id identifier | Variable identifier) as case)
    _loc
    =
    f case;
    identifier
  in
  (ignore : Rule.t Rule_id.Map.t * location -> unit) (map_stylesheet stylesheet ~f)
;;

let raise_due_to_collision_with_existing_ident
  ~loc
  ~css_identifier
  ~ocaml_identifier
  ~other_matching_ocaml_identifier
  =
  Location.raise_errorf
    ~loc
    "Unsafe collision of names. Cannot rename css identifier '%s' to ocaml identifier \
     '%s' because an existing css identifier '%s' also maps to '%s'"
    css_identifier
    ocaml_identifier
    other_matching_ocaml_identifier
    ocaml_identifier
;;

let raise_due_to_collision_with_newly_minted_identifier
  ~loc
  ~other_matching_css_identifier
  ~css_identifier
  ~ocaml_identifier
  =
  Location.raise_errorf
    ~loc
    "Unsafe collisions of names. Two different css identifiers map to the same ocaml \
     identifier which might lead to unintended results. Both '%s' and '%s' map to '%s'"
    other_matching_css_identifier
    css_identifier
    ocaml_identifier
;;

let raise_if_identifier_collides_with_existing
  identifier
  ~loc
  ~css_identifier_to_ocaml_identifier
  ~ocaml_identifier_to_css_identifier
  =
  let ocaml_identifier = Css_identifier.extract_ocaml_identifier identifier in
  let css_identifier = Css_identifier.extract_css_identifier identifier in
  let other_matching_css_identifier =
    Hashtbl.find ocaml_identifier_to_css_identifier ocaml_identifier
  in
  let other_matching_ocaml_identifier =
    Hashtbl.find css_identifier_to_ocaml_identifier css_identifier
  in
  match
    ( `Check_ocaml_identifier other_matching_ocaml_identifier
    , `Check_css_identifier other_matching_css_identifier )
  with
  | `Check_ocaml_identifier (Some other_matching_ocaml_identifier), _
    when not (String.equal other_matching_ocaml_identifier ocaml_identifier) ->
    raise_due_to_collision_with_existing_ident
      ~loc
      ~css_identifier
      ~ocaml_identifier
      ~other_matching_ocaml_identifier
  | `Check_ocaml_identifier (Some _), _ -> ()
  | `Check_ocaml_identifier None, `Check_css_identifier None ->
    (* If there's no collision and there's no other matching css identifier, we need to 
       add the current [css_identifier] to the hashmap to keep track of it
    *)
    Hashtbl.set
      ocaml_identifier_to_css_identifier
      ~key:ocaml_identifier
      ~data:css_identifier;
    Hashtbl.set
      css_identifier_to_ocaml_identifier
      ~key:css_identifier
      ~data:ocaml_identifier
  | `Check_ocaml_identifier _, `Check_css_identifier (Some other_matching_css_identifier)
    when not (String.equal other_matching_css_identifier css_identifier) ->
    (* If previously a different [css_identifier] mapped to this [ocaml_identifier], we 
       will raise
    *)
    raise_due_to_collision_with_newly_minted_identifier
      ~loc
      ~other_matching_css_identifier
      ~css_identifier
      ~ocaml_identifier
  (* Otherwise, the same [css_identifier] is mapping to this [ocaml_identifier] so we
     don't need to raise *)
  | `Check_ocaml_identifier _, `Check_css_identifier _ -> ()
;;

let string_constant ~loc l =
  let open (val Ast_builder.make loc) in
  pexp_constant (Pconst_string (l, loc, Some ""))
;;

let raise_if_unused_dont_hash_identifiers
  ~loc
  ~unused_dont_hash_identifiers
  ~unused_allow_set
  =
  let unused_dont_hash_identifiers =
    Set.of_hash_set (module String) unused_dont_hash_identifiers
  in
  let identifier_allow_list =
    let { Preprocess_arguments.dont_hash
        ; dont_hash_prefixes = _
        ; lazy_loading_optimization = _
        }
      =
      Preprocess_arguments.get ()
    in
    Set.union_list (module String) [ dont_hash; unused_allow_set ]
  in
  match Set.is_subset unused_dont_hash_identifiers ~of_:identifier_allow_list with
  | true -> ()
  | false ->
    Location.raise_errorf
      ~loc
      "Unused keys: %s"
      (Sexp.to_string_hum ([%sexp_of: String.Set.t] unused_dont_hash_identifiers))
;;

let raise_if_unused_prefixes ~loc ~unused_prefixes =
  let prefix_allow_list =
    let { Preprocess_arguments.dont_hash = _
        ; dont_hash_prefixes
        ; lazy_loading_optimization = _
        }
      =
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

let raise_if_unused_dont_hash_or_prefixes_or_collisions
  ~loc
  ~unused_allow_set
  ~unused_dont_hash
  ~unused_dont_hash_prefixes
  ~should_hash_identifier
  stylesheet
  =
  let css_identifier_to_ocaml_identifier = String.Table.create () in
  let ocaml_identifier_to_css_identifier = String.Table.create () in
  let unused_dont_hash = Set.to_list unused_dont_hash |> String.Hash_set.of_list in
  let unused_dont_hash_prefixes =
    Set.to_list unused_dont_hash_prefixes |> String.Hash_set.of_list
  in
  iter_identifiers stylesheet ~f:(fun identifier ->
    raise_if_identifier_collides_with_existing
      identifier
      ~loc
      ~css_identifier_to_ocaml_identifier
      ~ocaml_identifier_to_css_identifier;
    let css_identifier = Css_identifier.extract_css_identifier identifier in
    match should_hash_identifier identifier with
    | `Hash -> ()
    | `Dont_hash -> Hash_set.remove unused_dont_hash css_identifier
    | `Dont_hash_prefixes prefix -> Hash_set.remove unused_dont_hash_prefixes prefix);
  let unused_prefixes = String.Set.of_hash_set unused_dont_hash_prefixes in
  raise_if_unused_prefixes ~loc ~unused_prefixes;
  raise_if_unused_dont_hash_identifiers
    ~loc
    ~unused_dont_hash_identifiers:unused_dont_hash
    ~unused_allow_set
;;

module Original_and_post_processed = struct
  type 'a t =
    { original : 'a
    ; post_processed : 'a
    }
end

module Transform = struct
  type result =
    { stylesheet : Rule.t Original_and_post_processed.t Rule_id.Map.t * location
    ; identifier_mapping : expression Css_identifier.Table.t
    ; hash : string
    }

  let generate_hash ~disable_hashing ~pos original_css_string =
    let filename = Ppx_here_expander.expand_filename pos.pos_fname in
    let hash_prefix = 10 in
    let hash =
      if disable_hashing
      then "HASHELIDED"
      else
        sprintf
          "%s:%d:%d:%s"
          filename
          pos.pos_lnum
          (pos.pos_cnum - pos.pos_bol)
          original_css_string
        |> Md5.digest_string
        |> Md5.to_hex
    in
    String.prefix hash hash_prefix
  ;;

  let f
    ~pos_for_hashing
    ~original_css_string
    ~should_hash_identifier
    ~disable_hashing
    parsed
    =
    let hash = generate_hash ~disable_hashing original_css_string ~pos:pos_for_hashing in
    let identifier_mapping = Css_identifier.Table.create () in
    let stylesheet =
      Tuple2.map_fst parsed ~f:(fun original_stylesheet ->
        Map.map original_stylesheet ~f:(fun original_rule ->
          let post_processed_rule =
            map_rule original_rule ~f:(fun identifier loc ->
              let css_identifier = Css_identifier.extract_css_identifier identifier in
              let ret =
                match should_hash_identifier identifier with
                | `Hash -> sprintf "%s_hash_%s" css_identifier hash
                | `Dont_hash | `Dont_hash_prefixes _ -> css_identifier
              in
              Hashtbl.update identifier_mapping identifier ~f:(fun prev ->
                match prev with
                | None -> string_constant ~loc ret
                | Some prev -> prev);
              ret)
          in
          { Original_and_post_processed.original = original_rule
          ; post_processed = post_processed_rule
          }))
    in
    { stylesheet; identifier_mapping; hash }
  ;;
end

let get_all_identifiers stylesheet =
  let out = Css_identifier.Hash_set.create () in
  iter_identifiers stylesheet ~f:(fun identifier -> Hash_set.add out identifier);
  Css_identifier.Set.of_hash_set out
;;

module For_testing = struct
  let map_style_sheet s ~f = map_stylesheet s ~f
end
