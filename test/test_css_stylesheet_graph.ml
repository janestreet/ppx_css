open! Core
module Stable_stylesheet = Ppx_css.Stable_stylesheet
module Css_identifier = Ppx_css.Css_identifier

(* This file contains unit tests for the graph traversal module inside ppx_css. *)

module%test Traverse_graph = struct
  open Ppx_css.For_testing.Traverse_css

  let gen_result
    ?(dont_hash_prefixes = [])
    ?(dont_hash = String.Set.empty)
    ?(always_hash = String.Set.empty)
    (s : string)
    =
    let should_hash_identifier =
      Ppx_css.For_testing.create_should_hash_identifier
        ~dont_hash
        ~dont_hash_prefixes
        ~always_hash
      |> Staged.unstage
    in
    let stylesheet =
      Css_parser.(
        parse_stylesheet ~parsing_config:Parsing_config.raise_on_recoverable_errors s)
      |> Stable_stylesheet.of_stylesheet
    in
    Graph.map_stylesheet_for_graph
      ~lazy_loading_optimization:Ppx_css_syntax.Preprocess_arguments.Lazy_graph
      ~should_hash_identifier
      stylesheet
  ;;

  let check_autoforced ~here ~expected group_to_rule_indices =
    let autoforced_output =
      match Map.find group_to_rule_indices Graph.Group_type.Autoforced with
      | Some value -> Nonempty_set.to_set value |> Int.Set.map ~f:Ppx_css.Rule_id.to_int
      | None -> Int.Set.empty
    in
    let autoforced = Int.Set.of_list expected in
    Expect_test_helpers_base.require
      ~here
      ~hide_positions:true
      ~if_false_then_print_s:
        (lazy
          [%message
            "Autoforced rule indices mismatch between expected and output"
              ~expected:(autoforced : Int.Set.t)
              ~output:(autoforced_output : Int.Set.t)])
      (Int.Set.equal autoforced autoforced_output)
  ;;

  let check_if_input_groups_are_empty ~input ~here =
    match input with
    | [] ->
      (* It's okay for input groups to be an empty list, the autoforce validator will
         raise if they have not utilized everything required
      *)
      ()
    | input ->
      Expect_test_helpers_base.require
        ~here
        ~hide_positions:true
        ~if_false_then_print_s:
          (lazy
            [%message
              "One or more groups passed to the expected ~groups argument is empty. \
               Please fill in the expected groups"])
        (List.for_all input ~f:(fun identifiers -> not (List.is_empty identifiers)))
  ;;

  (** Removes an identifier from the set of total identifiers. If the identifier did not
      exist in the hash set, we assume that it has either already been removed or that the
      identifier was not utilized at all. *)
  let remove_identifier_from_hash_set_and_check
    ~here
    ~utilized_identifiers
    ~hash_set
    identifier
    =
    match Hash_set.strict_remove hash_set identifier with
    | Ok -> ()
    | Absent ->
      let is_utilized_identifier = Set.mem utilized_identifiers identifier in
      if is_utilized_identifier
      then
        Expect_test_helpers_base.print_cr
          ~here
          ~hide_positions:true
          [%message "Identifier shows up in more than one group"]
      else
        Expect_test_helpers_base.print_cr
          ~here
          ~hide_positions:true
          [%message
            "Identifier was not utilized. Please remove identifier"
              (identifier : Css_identifier.t)]
  ;;

  (** Strictly adds to the list of seen groups and prints a CR if we've already added this
      group. This function should not be called within a list of identifiers and should
      only be called once per list of identifiers after all identifiers in the expected
      group are processed *)
  let add_to_seen_groups_and_check ~seen_groups ~here group =
    let have_not_seen =
      Hash_set.strict_add seen_groups group |> Hash_set.Ok_or_duplicate.is_ok
    in
    Expect_test_helpers_base.require
      ~here
      ~hide_positions:true
      ~if_false_then_print_s:
        (lazy
          [%message
            "Identifiers from the same group expected to be in different groups. Please \
             check the expected output in ~group and fix any problems"])
      have_not_seen
  ;;

  let check_previous_group_equals_current_group ~here ~identifier ~previous_group output =
    Expect_test_helpers_base.require
      ~here
      ~hide_positions:true
      ~if_false_then_print_s:
        (lazy
          [%message
            "Group type mismatch between expected"
              (previous_group : Graph.Group_type.t)
              "and"
              (output : Graph.Group_type.t)
              "for"
              (identifier : Css_identifier.t)])
      (Graph.Group_type.equal previous_group output)
  ;;

  let check_for_missing_identifiers ~here missing_identifiers =
    let missing_identifiers = Css_identifier.Set.of_hash_set missing_identifiers in
    Expect_test_helpers_base.require
      ~here
      ~hide_positions:true
      ~if_false_then_print_s:
        (lazy
          [%message
            "Identifiers are missing from the expected output. Please add the following \
             identifiers to ~groups"
              (missing_identifiers : Css_identifier.Set.t)])
      (Set.is_empty missing_identifiers)
  ;;

  let assert_test
    ~(here : [%call_pos])
    ~(autoforced : int list)
    ~(groups : Css_identifier.t list list)
    ({ group_to_rule_indices; identifiers; get_group_for_identifier; _ } : Graph.result)
    =
    check_if_input_groups_are_empty ~here ~input:groups;
    check_autoforced ~expected:autoforced ~here group_to_rule_indices;
    let missing_identifiers = Css_identifier.Hash_set.of_list (Set.to_list identifiers) in
    let original_identifiers = identifiers in
    let seen_groups = Graph.Group_type.Hash_set.create () in
    (* Checks to make sure all identifiers grouped together in the input are in the same
       group when passed to the [get_group_for_identifier] function. The specific group
       number doesn't really matter.
    *)
    List.iter groups ~f:(fun identifiers ->
      let group =
        List.fold identifiers ~init:None ~f:(fun group identifier ->
          remove_identifier_from_hash_set_and_check
            ~here
            ~utilized_identifiers:original_identifiers
            ~hash_set:missing_identifiers
            identifier;
          let output = get_group_for_identifier identifier in
          match group with
          | None -> Some output
          | Some group ->
            check_previous_group_equals_current_group
              ~here
              ~previous_group:group
              ~identifier
              output;
            Some output)
      in
      match group with
      (* Already printing CR for empty groups, can ignore *)
      | None -> ()
      | Some group -> add_to_seen_groups_and_check ~seen_groups ~here group);
    check_for_missing_identifiers ~here missing_identifiers
  ;;

  let expect_test
    ({ group_to_rules; get_group_for_identifier; identifiers; _ } : Graph.result)
    =
    let identifiers_by_group =
      Set.fold
        identifiers
        ~init:
          (Graph.Group_type.Map.singleton
             Graph.Group_type.Autoforced
             Ppx_css.Css_identifier.Set.empty)
        ~f:(fun acc identifier ->
          let group_number = get_group_for_identifier identifier in
          Map.update acc group_number ~f:(function
            | None -> Ppx_css.Css_identifier.Set.singleton identifier
            | Some existing_values -> Set.add existing_values identifier))
    in
    Map.iteri group_to_rules ~f:(fun ~key:group ~data:rules ->
      let loc = Ppxlib.Location.none in
      let sheet_as_string = Css_parser.stylesheet_to_string (rules, loc) in
      let identifiers_for_group = Map.find_exn identifiers_by_group group in
      print_endline "\n";
      print_s
        [%message
          (Ppx_css.For_testing.Traverse_css.Graph.Group_type.to_string group)
            ~identifiers:(identifiers_for_group : Ppx_css.Css_identifier.Set.t)];
      print_endline "\n";
      print_endline sheet_as_string;
      print_endline "==================================================")
  ;;

  let test
    ?(dont_hash_prefixes = [])
    ?(dont_hash = String.Set.empty)
    ?(always_hash = String.Set.empty)
    ~(here : [%call_pos])
    ~(autoforced : int list)
    ~(groups : Css_identifier.t list list)
    (s : string)
    =
    let%tydi result = gen_result ~dont_hash ~dont_hash_prefixes ~always_hash s in
    assert_test ~autoforced ~here ~groups result;
    expect_test result
  ;;

  let%expect_test "Testing identifiers" =
    test
      {|
        @layer test-layer {
          .inside-layer {

          }
        }

        .class {}

        #an_id {}

        .with-kebab-case {}

        .a-b_c {}

        .both {}

        #both {}

        div {
        }

        .class #id {
        }

        .class .class2 {
        }

        .class .class {
          .class2 {
            #id-inside {}
          }
        }

        #id #id {}
        div > .class {}
        div .class {}
        div.class {}
    |}
      ~autoforced:[ 7 ]
      ~groups:
        [ Css_identifier.[ Class "class"; Class "class2"; Id "id"; Id "id-inside" ]
        ; Css_identifier.[ Class "inside-layer" ]
        ; Css_identifier.[ Class "a-b_c" ]
        ; Css_identifier.[ Class "with-kebab-case" ]
        ; Css_identifier.[ Class "both" ]
        ; Css_identifier.[ Id "both" ]
        ; Css_identifier.[ Id "an_id" ]
        ];
    [%expect
      {|
      (Autoforced (identifiers ()))


      div {
      }
      ==================================================


      (group_0 (identifiers ((Class with-kebab-case))))


      .with-kebab-case {
      }
      ==================================================


      (group_1 (identifiers ((Id an_id))))


      #an_id {
      }
      ==================================================


      (group_2 (identifiers ((Id id) (Id id-inside) (Class class) (Class class2))))


      div.class {
      }
      div .class {
      }
      div > .class {
      }
      #id #id {
      }
      .class .class {
        .class2 {
          #id-inside {
          }
        }
      }
      .class .class2 {
      }
      .class #id {
      }
      .class {
      }
      ==================================================


      (group_3 (identifiers ((Class a-b_c))))


      .a-b_c {
      }
      ==================================================


      (group_4 (identifiers ((Class inside-layer))))


      @layer test-layer {
        .inside-layer {
        }
      }
      ==================================================


      (group_5 (identifiers ((Id both))))


      #both {
      }
      ==================================================


      (group_6 (identifiers ((Class both))))


      .both {
      }
      ==================================================
      |}]
  ;;

  let%expect_test "Testing variables" =
    test
      ~autoforced:[ 0; 1 ]
      ~groups:
        [ Css_identifier.[ Class "c1"; Class "c2"; Class "c3" ]
        ; Css_identifier.[ Class "c4"; Class "c5"; Class "c6" ]
        ; Css_identifier.[ Class "c7" ]
        ]
      {|
        :root {
           --with-kebab: 100px;
           --with_snakecase: 100px;
        }

        html {
           color: var(--inside-a-var);
        }

        .c1 {
        }

        .c2 {
        }

        .c1 + .c2 {
        }

        .c3 {
          .c1 {}
        }

        .c4 {
          .c5 {}
        }

        .c6:hover + .c5 {}

        .c7 {}
    |};
    [%expect
      {|
      (Autoforced (identifiers ()))


      html {
        color: var(--inside-a-var);
      }
      :root {
        --with-kebab: 100px;
        --with_snakecase: 100px;
      }
      ==================================================


      (group_0 (identifiers ((Class c1) (Class c2) (Class c3))))


      .c3 {
        .c1 {
        }
      }
      .c1 + .c2 {
      }
      .c2 {
      }
      .c1 {
      }
      ==================================================


      (group_1 (identifiers ((Class c4) (Class c5) (Class c6))))


      .c6:hover + .c5 {
      }
      .c4 {
        .c5 {
        }
      }
      ==================================================


      (group_2 (identifiers ((Class c7))))


      .c7 {
      }
      ==================================================
      |}]
  ;;

  let%expect_test "Top-level rules should automatically force children if no identifiers \
                   present. Layers should be considered top-level if they contain \
                   top-level declarations"
    =
    test
      ~autoforced:[ 0; 1 ]
      ~groups:[ Css_identifier.[ Class "a" ]; Css_identifier.[ Class "b" ] ]
      {|
        html {
           color: var(--inside-a-var);

          .a {}
          .b {}
        }

        @layer top-level {
          div {}
        }

        @layer not-top-level {
          .a {}
        }

        .a {}
        .b {}
    |};
    [%expect
      {|
      (Autoforced (identifiers ()))


      @layer top-level {
        div {
        }
      }
      html {
        color: var(--inside-a-var);
        .a {
        }
        .b {
        }
      }
      ==================================================


      (group_0 (identifiers ((Class a))))


      .a {
      }
      @layer not-top-level {
        .a {
        }
      }
      ==================================================


      (group_1 (identifiers ((Class b))))


      .b {
      }
      ==================================================
      |}]
  ;;

  let%expect_test "Properly processed at_rules should be treated like top-level rules  \
                   for grouping"
    =
    test
      ~autoforced:[]
      ~groups:
        [ Css_identifier.[ Class "a"; Class "b"; Class "c"; Class "d"; Class "e"; Id "f" ]
        ; Css_identifier.[ Class "g"; Id "h" ]
        ]
      {|
        @layer layer-1 {
          .c + .a {
            .b {}
          }
        }

        @layer layer-2 {
          .d {
            .e {}
          }
        }

        .e + #f {}

        .b + .d {}

        @layer layer-3 {
          .g + #h {}
        }

        #h {}
    |};
    [%expect
      {|
      (group_0
       (identifiers ((Id f) (Class a) (Class b) (Class c) (Class d) (Class e))))


      .b + .d {
      }
      .e + #f {
      }
      @layer layer-2 {
        .d {
          .e {
          }
        }
      }
      @layer layer-1 {
        .c + .a {
          .b {
          }
        }
      }
      ==================================================


      (group_1 (identifiers ((Id h) (Class g))))


      #h {
      }
      @layer layer-3 {
        .g + #h {
        }
      }
      ==================================================
      |}]
  ;;

  let%expect_test "Layers and other at_rules that are not properly processed should be \
                   automatically forced and not considered in grouping"
    =
    test
      ~autoforced:[ 0; 1 ]
      ~groups:[ Css_identifier.[ Class "a" ]; Css_identifier.[ Class "b" ] ]
      {|
        @layer not-properly-processed {
          .a {}
          .b {}
          .c {}
        }

        @media print {
          .a {}
        }

        .a {}
        .b {}
    |};
    [%expect
      {|
      (Autoforced (identifiers ()))


      @media print {
        .a {
        }
      }
      @layer not-properly-processed {
        .a {
        }
        .b {
        }
        .c {
        }
      }
      ==================================================


      (group_0 (identifiers ((Class a))))


      .a {
      }
      ==================================================


      (group_1 (identifiers ((Class b))))


      .b {
      }
      ==================================================
      |}]
  ;;

  let%expect_test "Selector functions are processed properly into groups" =
    test
      ~autoforced:[ 2; 8 ]
      ~groups:
        [ Css_identifier.[ Class "a"; Class "b"; Class "c" ]
        ; Css_identifier.[ Class "e"; Class "f"; Class "g"; Class "h" ]
        ; Css_identifier.[ Class "z"; Class "q" ]
        ; Css_identifier.[ Class "i" ]
        ]
      {|
        :has(.a, .b + .c) { }

        .e, .f, .g { }

        :has(div, .x, .y) {}

        div:has(.i) {}

        .z {}
        .q {}

        :has(div, .z) :has(.q) {}

        .g + .h {}

        :not(:has(.j)) {}
    |};
    [%expect
      {|
      (Autoforced (identifiers ()))


      :not(:has(.j)) {
      }
      :has(div, .x, .y) {
      }
      ==================================================


      (group_0 (identifiers ((Class e) (Class f) (Class g) (Class h))))


      .g + .h {
      }
      .e, .f, .g {
      }
      ==================================================


      (group_1 (identifiers ((Class q) (Class z))))


      :has(div, .z) :has(.q) {
      }
      .q {
      }
      .z {
      }
      ==================================================


      (group_2 (identifiers ((Class a) (Class b) (Class c))))


      :has(.a, .b + .c) {
      }
      ==================================================


      (group_3 (identifiers ((Class i))))


      div:has(.i) {
      }
      ==================================================
      |}]
  ;;

  let%expect_test "Selector lists are processed properly into groups" =
    test
      ~autoforced:[ 0 ]
      ~groups:[ Css_identifier.[ Class "e"; Class "f"; Class "g"; Class "h" ] ]
      {|
        div, .a, .b + .c {
        }

        .e, .f, .g { }

        .g + .h {}
    |};
    [%expect
      {|
      (Autoforced (identifiers ()))


      div, .a, .b + .c {
      }
      ==================================================


      (group_0 (identifiers ((Class e) (Class f) (Class g) (Class h))))


      .g + .h {
      }
      .e, .f, .g {
      }
      ==================================================
      |}]
  ;;

  let%expect_test "Testing dont hash" =
    test
      ~autoforced:[ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
      ~groups:[]
      ~dont_hash_prefixes:[ "c" ]
      {|
        :root {
           --with-kebab: 100px;
           --with_snakecase: 100px;
        }

        html {
           color: var(--inside-a-var);
        }

        .c1 {
        }

        .c2 {
        }

        .c1 + .c2 {
        }

        .c3 {
          .c1 {}
        }

        .c4 {
          .c5 {}
        }

        .c40 {
        }

        .c6 + .c5 {}

        .c7 {}

    |};
    [%expect
      {|
      (Autoforced (identifiers ()))


      .c7 {
      }
      .c6 + .c5 {
      }
      .c40 {
      }
      .c4 {
        .c5 {
        }
      }
      .c3 {
        .c1 {
        }
      }
      .c1 + .c2 {
      }
      .c2 {
      }
      .c1 {
      }
      html {
        color: var(--inside-a-var);
      }
      :root {
        --with-kebab: 100px;
        --with_snakecase: 100px;
      }
      ==================================================
      |}];
    test
      ~autoforced:[ 0; 1; 2; 3; 4; 5; 6; 7 ]
      ~groups:[ Css_identifier.[ Class "c6"; Class "c5" ]; Css_identifier.[ Class "c7" ] ]
      ~dont_hash:(String.Set.of_list [ "c1"; "c2"; "c3" ])
      ~dont_hash_prefixes:[ "c4" ]
      {|
        :root {
           --with-kebab: 100px;
           --with_snakecase: 100px;
        }

        html {
           color: var(--inside-a-var);
        }

        .c1 {
        }

        .c2 {
        }

        .c1 + .c2 {
        }

        .c3 {
          .c1 {}
        }

        .c4 {
          .c5 {}
        }

        .c40 {
        }

        .c6 + .c5 {}


        .c7 {}
    |};
    [%expect
      {|
      (Autoforced (identifiers ()))


      .c40 {
      }
      .c4 {
        .c5 {
        }
      }
      .c3 {
        .c1 {
        }
      }
      .c1 + .c2 {
      }
      .c2 {
      }
      .c1 {
      }
      html {
        color: var(--inside-a-var);
      }
      :root {
        --with-kebab: 100px;
        --with_snakecase: 100px;
      }
      ==================================================


      (group_0 (identifiers ((Class c7))))


      .c7 {
      }
      ==================================================


      (group_1 (identifiers ((Class c5) (Class c6))))


      .c6 + .c5 {
      }
      ==================================================
      |}]
  ;;

  let%expect_test "attrs with values that look like identifiers should not be considered \
                   identifiers"
    =
    test
      ~autoforced:[ 0; 1; 2; 3; 4; 5 ]
      ~groups:[]
      {|
        a[asdf=".a"] { }
        a[asdf='.a'] { }
        html[foo='#d'] { }
        html[foo="#d"] { }
        *[bar="--something"] { }
        *[bar='--something'] { }
    |};
    [%expect
      {|
      (Autoforced (identifiers ()))


      *[bar='--something'] {
      }
      *[bar="--something"] {
      }
      html[foo="#d"] {
      }
      html[foo='#d'] {
      }
      a[asdf='.a'] {
      }
      a[asdf=".a"] {
      }
      ==================================================
      |}]
  ;;

  let%expect_test "SHOULD pull identifiers from supported selector functions" =
    test
      ~autoforced:[]
      ~groups:[ Css_identifier.[ Class "a"; Class "b"; Class "c" ] ]
      {|
        div:has(.a, .b, .c) { }
        |};
    [%expect
      {|
      (group_0 (identifiers ((Class a) (Class b) (Class c))))


      div:has(.a, .b, .c) {
      }
      ==================================================
      |}]
  ;;

  let%expect_test "Should not be pulling identifiers from unsupported CSS functions or \
                   CSS functions that accept strings"
    =
    test
    (* The comment within the CSS now counts as a "rule" so we need to include it in these
       indices *)
      ~autoforced:[ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
      ~groups:[]
      {|
        div:where(.a, .b, .c) { }
        div:where(.a) { }
        div:where(.a + .b) { }
        div:is(.a, .b, .c) { }
        div:is(.a) { }
        div:is(.a + .b) { }
        div:test(.a + .b) { }
        div:unsupported(.a + .b) { }

        /* This one specifically is a known selector that should parse the identifiers.
           Checking to make sure that if we pass a string that it does not actually
           parse the identifiers */
        div:string(".a + .b") { }

        |};
    [%expect
      {|
      (Autoforced (identifiers ()))


      div:string(".a + .b") {
      }
      /* This one specifically is a known selector that should parse the identifiers.
                 Checking to make sure that if we pass a string that it does not actually
                 parse the identifiers */
      div:unsupported(.a + .b) {
      }
      div:test(.a + .b) {
      }
      div:is(.a + .b) {
      }
      div:is(.a) {
      }
      div:is(.a, .b, .c) {
      }
      div:where(.a + .b) {
      }
      div:where(.a) {
      }
      div:where(.a, .b, .c) {
      }
      ==================================================
      |}]
  ;;

  let%expect_test "random weird tags should not be considered as identifiers" =
    test
      ~autoforced:[ 0; 1; 2 ]
      ~groups:[]
      {|
        weird { }
        basdf { }
        q123 { }
    |};
    [%expect
      {|
      (Autoforced (identifiers ()))


      q123 {
      }
      basdf {
      }
      weird {
      }
      ==================================================
      |}]
  ;;

  let%expect_test "dont_hash_prefixes with '--' should only affect variables" =
    test
      ~autoforced:[]
      ~groups:
        [ Css_identifier.[ Class "a" ]
        ; Css_identifier.[ Class "b" ]
        ; Css_identifier.[ Class "c" ]
        ]
      ~dont_hash_prefixes:[ "--" ]
      {|
        .a {
          background-color: var(--some-var);
        }
        .b {
          width: var(--some-width);
        }
        .c {
          padding: var(--some-padding);
        }
    |};
    [%expect
      {|
      (group_0 (identifiers ((Class c))))


      .c {
        padding: var(--some-padding);
      }
      ==================================================


      (group_1 (identifiers ((Class a))))


      .a {
        background-color: var(--some-var);
      }
      ==================================================


      (group_2 (identifiers ((Class b))))


      .b {
        width: var(--some-width);
      }
      ==================================================
      |}]
  ;;

  let%expect_test "Always hash takes precedence over dont hash and dont hash prefixes" =
    test
      ~autoforced:[ 2 ]
      ~groups:[ Css_identifier.[ Class "a" ]; Css_identifier.[ Class "b" ] ]
      ~dont_hash:(String.Set.of_list [ "a" ])
      ~dont_hash_prefixes:[ "b" ]
      ~always_hash:(String.Set.of_list [ "a"; "b" ])
      {|
        .a {}
        .b {}
        .b1 {}
    |};
    [%expect
      {|
      (Autoforced (identifiers ()))


      .b1 {
      }
      ==================================================


      (group_0 (identifiers ((Class a))))


      .a {
      }
      ==================================================


      (group_1 (identifiers ((Class b))))


      .b {
      }
      ==================================================
      |}]
  ;;

  let%expect_test "Scary attributes need to be forced" =
    test
      ~groups:[]
      ~autoforced:[ 0; 1; 2; 3; 4; 5 ]
      {|
        div[has=".t .q .r"] {}
        div[attr='.t .q .r'] {}
        div:asdf(".t .q .r") {}
        div:asdf(.t .q .r) {}
        div:qwerty(.t .q .r) {}
        div[href=#] {}
    |};
    [%expect
      {|
      (Autoforced (identifiers ()))


      div[href=#] {
      }
      div:qwerty(.t .q .r) {
      }
      div:asdf(.t .q .r) {
      }
      div:asdf(".t .q .r") {
      }
      div[attr='.t .q .r'] {
      }
      div[has=".t .q .r"] {
      }
      ==================================================
      |}]
  ;;

  let%expect_test "ids and classes are not treated as the same node in the graph" =
    test
      ~autoforced:[]
      ~groups:[ Css_identifier.[ Class "a"; Class "b" ]; Css_identifier.[ Id "b" ] ]
      {|
      .a       .b {}
      :not(.a) .b {}
      :not(#a) #b {}
    |};
    [%expect
      {|
      (group_0 (identifiers ((Id b))))


      :not(#a) #b {
      }
      ==================================================


      (group_1 (identifiers ((Class a) (Class b))))


      :not(.a) .b {
      }
      .a .b {
      }
      ==================================================
      |}];
    test
      ~autoforced:[]
      ~groups:[ Css_identifier.[ Class "a"; Class "b" ]; Css_identifier.[ Id "d" ] ]
      {|
      .a       .b {}
      :not(.a) .b {}
      :not(#c) #d {}
    |};
    [%expect
      {|
      (group_0 (identifiers ((Id d))))


      :not(#c) #d {
      }
      ==================================================


      (group_1 (identifiers ((Class a) (Class b))))


      :not(.a) .b {
      }
      .a .b {
      }
      ==================================================
      |}];
    test
      ~autoforced:[ 0; 3; 4 ]
      ~groups:
        [ Css_identifier.[ Class "b"; Class "c" ]
        ; Css_identifier.[ Class "not-autoforced"; Class "ba"; Class "ca"; Class "da" ]
        ]
      {|
      .a {
        :not(&) {}
      }
      .b {
        :has(.c, &) {}
      }
      .not-autoforced {
        & .ba, .ca, .da & {
        }
      }
      .autoforce-due-to-nested-at-rule {
        #id {}
        @layer test {
          .x {}
        }
      }
      .z {
        .b + .d > .g {
          .k {
            .x {}
            :not(&, & > .a) {}
          }
          background-color: red;
        }
        background-color: tomato;
      }
    |};
    [%expect
      {|
      (Autoforced (identifiers ()))


      .z {
        .b + .d > .g {
          .k {
            .x {
            }
            :not(&, & > .a) {
            }
          }
          background-color: red;
        }
        background-color: tomato;
      }
      .autoforce-due-to-nested-at-rule {
        #id {
        }
        @layer test {
          .x {
          }
        }
      }
      .a {
        :not(&) {
        }
      }
      ==================================================


      (group_0
       (identifiers ((Class ba) (Class ca) (Class da) (Class not-autoforced))))


      .not-autoforced {
        & .ba, .ca, .da & {
        }
      }
      ==================================================


      (group_1 (identifiers ((Class b) (Class c))))


      .b {
        :has(.c, &) {
        }
      }
      ==================================================
      |}]
  ;;
end
