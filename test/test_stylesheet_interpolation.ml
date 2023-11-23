open! Core
open Ppxlib
open Ppx_css

let loc = Location.none

let catch_location_error ~f =
  try f () with
  | ex ->
    (match Location.Error.of_exn ex with
     | Some error -> print_endline (Location.Error.message error)
     | None -> raise ex)
;;

let test expr =
  catch_location_error ~f:(fun () ->
    let%tydi { txt = structure; ppx_css_string_expression } =
      expr |> For_testing.generate_struct
    in
    structure |> Pprintast.string_of_structure |> print_endline;
    print_endline
      (Pprintast.string_of_structure
         [ For_testing.ppx_css_expression_to_structure_item ~loc ppx_css_string_expression
         ]))
;;

let%expect_test "basic" =
  test [%expr stylesheet {|.hello {
      background-color: %{color};
  }|}];
  [%expect
    {xxx|
      [@@@ocaml.warning "-32"]
      let __type_info_for_ppx_css :
        ?rewrite:(string * string) list ->
          ?dont_hash:string list ->
            ?dont_hash_prefixes:string list -> string -> unit
        = fun ?rewrite:_ ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
      module type S  =
        sig
          module For_referencing : sig val hello : string end
          val hello : Virtual_dom.Vdom.Attr.t
        end
      type t = (module S)
      module Default : S =
        struct
          let ppx_css__internal_anonymous_variables__001_ =
            let ppx_css_temp_variable__002_ = (((color)[@merlin.focus ]) : string) in
            Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
              [({|--ppx_css_anonymous_var_1_hash_91f1d840ce|},
                 ppx_css_temp_variable__002_)]
          module For_referencing = struct let hello = {|hello_hash_91f1d840ce|} end
          let hello =
            ((Virtual_dom.Vdom.Attr.combine
                (Virtual_dom.Vdom.Attr.class_ {|hello_hash_91f1d840ce|})
                ppx_css__internal_anonymous_variables__001_)
            [@merlin.focus ])
        end
      include Default
      let default : t = (module Default)
      let () =
        Inline_css.Private.append
          {|
      /* _none_ */

      *.hello_hash_91f1d840ce {
       background-color:var(--ppx_css_anonymous_var_1_hash_91f1d840ce)
      }|} |xxx}]
;;

let%expect_test "basic with many variables" =
  test
    [%expr
      stylesheet
        {|.hello {
      background-color: %{color};
      color: %{color2};
  }|}];
  [%expect
    {xxx|
      [@@@ocaml.warning "-32"]
      let __type_info_for_ppx_css :
        ?rewrite:(string * string) list ->
          ?dont_hash:string list ->
            ?dont_hash_prefixes:string list -> string -> unit
        = fun ?rewrite:_ ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
      module type S  =
        sig
          module For_referencing : sig val hello : string end
          val hello : Virtual_dom.Vdom.Attr.t
        end
      type t = (module S)
      module Default : S =
        struct
          let ppx_css__internal_anonymous_variables__003_ =
            let ppx_css_temp_variable__004_ = (((color)[@merlin.focus ]) : string) in
            let ppx_css_temp_variable__005_ = (((color2)[@merlin.focus ]) : string) in
            Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
              [({|--ppx_css_anonymous_var_2_hash_968a0c3f9c|},
                 ppx_css_temp_variable__004_);
              ({|--ppx_css_anonymous_var_3_hash_968a0c3f9c|},
                ppx_css_temp_variable__005_)]
          module For_referencing = struct let hello = {|hello_hash_968a0c3f9c|} end
          let hello =
            ((Virtual_dom.Vdom.Attr.combine
                (Virtual_dom.Vdom.Attr.class_ {|hello_hash_968a0c3f9c|})
                ppx_css__internal_anonymous_variables__003_)
            [@merlin.focus ])
        end
      include Default
      let default : t = (module Default)
      let () =
        Inline_css.Private.append
          {|
      /* _none_ */

      *.hello_hash_968a0c3f9c {
       background-color:var(--ppx_css_anonymous_var_2_hash_968a0c3f9c);
       color:var(--ppx_css_anonymous_var_3_hash_968a0c3f9c)
      }|} |xxx}]
;;

let%expect_test "basic with modules" =
  test
    [%expr
      stylesheet
        {|.hello {
      background-color: %{color#Css_gen.Color};
      min-width: %{width#Css_gen.Length};
  }|}];
  [%expect
    {xxx|
      [@@@ocaml.warning "-32"]
      let __type_info_for_ppx_css :
        ?rewrite:(string * string) list ->
          ?dont_hash:string list ->
            ?dont_hash_prefixes:string list -> string -> unit
        = fun ?rewrite:_ ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
      module type S  =
        sig
          module For_referencing : sig val hello : string end
          val hello : Virtual_dom.Vdom.Attr.t
        end
      type t = (module S)
      module Default : S =
        struct
          let ppx_css__internal_anonymous_variables__006_ =
            let ppx_css_temp_variable__007_ = (((Css_gen.Color.to_string_css color)
              [@merlin.focus ]) : string) in
            let ppx_css_temp_variable__008_ =
              (((Css_gen.Length.to_string_css width)[@merlin.focus ]) : string) in
            Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
              [({|--ppx_css_anonymous_var_4_hash_56beab377a|},
                 ppx_css_temp_variable__007_);
              ({|--ppx_css_anonymous_var_5_hash_56beab377a|},
                ppx_css_temp_variable__008_)]
          module For_referencing = struct let hello = {|hello_hash_56beab377a|} end
          let hello =
            ((Virtual_dom.Vdom.Attr.combine
                (Virtual_dom.Vdom.Attr.class_ {|hello_hash_56beab377a|})
                ppx_css__internal_anonymous_variables__006_)
            [@merlin.focus ])
        end
      include Default
      let default : t = (module Default)
      let () =
        Inline_css.Private.append
          {|
      /* _none_ */

      *.hello_hash_56beab377a {
       background-color:var(--ppx_css_anonymous_var_4_hash_56beab377a);
       min-width:var(--ppx_css_anonymous_var_5_hash_56beab377a)
      }|} |xxx}]
;;

let%expect_test "Evaluation order matters" =
  test
    [%expr
      stylesheet
        {|.hello {
      background-color: %{first ()#Css_gen.Color};
      background-color: %{second ()#Beep};
      background-color: %{third ()};
      background-color: %{fourth ()#Css_gen.Color};
  }|}];
  [%expect
    {xxx|
      [@@@ocaml.warning "-32"]
      let __type_info_for_ppx_css :
        ?rewrite:(string * string) list ->
          ?dont_hash:string list ->
            ?dont_hash_prefixes:string list -> string -> unit
        = fun ?rewrite:_ ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
      module type S  =
        sig
          module For_referencing : sig val hello : string end
          val hello : Virtual_dom.Vdom.Attr.t
        end
      type t = (module S)
      module Default : S =
        struct
          let ppx_css__internal_anonymous_variables__009_ =
            let ppx_css_temp_variable__010_ =
              (((Css_gen.Color.to_string_css (first ()))[@merlin.focus ]) :
              string) in
            let ppx_css_temp_variable__011_ = (((Beep.to_string_css (second ()))
              [@merlin.focus ]) : string) in
            let ppx_css_temp_variable__012_ = (((third ())
              [@merlin.focus ]) : string) in
            let ppx_css_temp_variable__013_ =
              (((Css_gen.Color.to_string_css (fourth ()))
              [@merlin.focus ]) : string) in
            Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
              [({|--ppx_css_anonymous_var_6_hash_4428b6d16a|},
                 ppx_css_temp_variable__010_);
              ({|--ppx_css_anonymous_var_7_hash_4428b6d16a|},
                ppx_css_temp_variable__011_);
              ({|--ppx_css_anonymous_var_8_hash_4428b6d16a|},
                ppx_css_temp_variable__012_);
              ({|--ppx_css_anonymous_var_9_hash_4428b6d16a|},
                ppx_css_temp_variable__013_)]
          module For_referencing = struct let hello = {|hello_hash_4428b6d16a|} end
          let hello =
            ((Virtual_dom.Vdom.Attr.combine
                (Virtual_dom.Vdom.Attr.class_ {|hello_hash_4428b6d16a|})
                ppx_css__internal_anonymous_variables__009_)
            [@merlin.focus ])
        end
      include Default
      let default : t = (module Default)
      let () =
        Inline_css.Private.append
          {|
      /* _none_ */

      *.hello_hash_4428b6d16a {
       background-color:var(--ppx_css_anonymous_var_6_hash_4428b6d16a);
       background-color:var(--ppx_css_anonymous_var_7_hash_4428b6d16a);
       background-color:var(--ppx_css_anonymous_var_8_hash_4428b6d16a);
       background-color:var(--ppx_css_anonymous_var_9_hash_4428b6d16a)
      }|} |xxx}]
;;

let%expect_test "Anonymous variable and user defined variable are both hashed (different \
                 from inline expression.)"
  =
  test
    [%expr
      stylesheet
        {|.hello {
      background-color: %{color};
      width : var(--bar);
  }|}];
  [%expect
    {xxx|
      [@@@ocaml.warning "-32"]
      let __type_info_for_ppx_css :
        ?rewrite:(string * string) list ->
          ?dont_hash:string list ->
            ?dont_hash_prefixes:string list -> string -> unit
        = fun ?rewrite:_ ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
      module type S  =
        sig
          module Variables :
          sig
            val set : ?bar:string -> unit -> Virtual_dom.Vdom.Attr.t
            val set_all : bar:string -> Virtual_dom.Vdom.Attr.t
          end
          module For_referencing : sig val bar : string val hello : string end
          val hello : Virtual_dom.Vdom.Attr.t
        end
      type t = (module S)
      module Default : S =
        struct
          let ppx_css__internal_anonymous_variables__014_ =
            let ppx_css_temp_variable__015_ = (((color)[@merlin.focus ]) : string) in
            Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
              [({|--ppx_css_anonymous_var_10_hash_24118383ee|},
                 ppx_css_temp_variable__015_)]
          module Variables =
            struct
              let ppx_css_variable_set__018_ ?bar () =
                let ppx_css_acc__016_ = [] in
                let ppx_css_acc__016_ =
                  match bar with
                  | None -> ppx_css_acc__016_
                  | Some ppx_css_value__017_ ->
                      ({|--bar_hash_24118383ee|}, ppx_css_value__017_) ::
                      ppx_css_acc__016_ in
                Virtual_dom.Vdom.Attr.combine
                  ppx_css__internal_anonymous_variables__014_
                  (Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__016_)
              let set = ppx_css_variable_set__018_
              let set_all ~bar = ppx_css_variable_set__018_ () ~bar
            end
          module For_referencing =
            struct
              let hello = {|hello_hash_24118383ee|}
              let bar = {|--bar_hash_24118383ee|}
            end
          let hello =
            ((Virtual_dom.Vdom.Attr.combine
                (Virtual_dom.Vdom.Attr.class_ {|hello_hash_24118383ee|})
                ppx_css__internal_anonymous_variables__014_)
            [@merlin.focus ])
        end
      include Default
      let default : t = (module Default)
      let () =
        Inline_css.Private.append
          {|
      /* _none_ */

      *.hello_hash_24118383ee {
       background-color:var(--ppx_css_anonymous_var_10_hash_24118383ee);
       width:var(--bar_hash_24118383ee)
      }|} |xxx}]
;;

let%expect_test "Anonymous variable and user defined variable are both hashed (different \
                 from inline expression.)"
  =
  test
    [%expr
      stylesheet
        {|.hello {
      background-color: %{color};
      width : var(--bar);
  }|}
        ~rewrite:[ "--bar", "--foo" ]];
  [%expect
    {xxx|
      [@@@ocaml.warning "-32"]
      let __type_info_for_ppx_css :
        ?rewrite:(string * string) list ->
          ?dont_hash:string list ->
            ?dont_hash_prefixes:string list -> string -> unit
        = fun ?rewrite:_ ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
      module type S  =
        sig
          module Variables :
          sig
            val set : ?bar:string -> unit -> Virtual_dom.Vdom.Attr.t
            val set_all : bar:string -> Virtual_dom.Vdom.Attr.t
          end
          module For_referencing : sig val bar : string val hello : string end
          val hello : Virtual_dom.Vdom.Attr.t
        end
      type t = (module S)
      module Default : S =
        struct
          let ppx_css__internal_anonymous_variables__019_ =
            let ppx_css_temp_variable__020_ = (((color)[@merlin.focus ]) : string) in
            Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
              [({|--ppx_css_anonymous_var_11_hash_178431e471|},
                 ppx_css_temp_variable__020_)]
          module Variables =
            struct
              let ppx_css_variable_set__023_ ?bar () =
                let ppx_css_acc__021_ = [] in
                let ppx_css_acc__021_ =
                  match bar with
                  | None -> ppx_css_acc__021_
                  | Some ppx_css_value__022_ -> ({|--foo|}, ppx_css_value__022_) ::
                      ppx_css_acc__021_ in
                Virtual_dom.Vdom.Attr.combine
                  ppx_css__internal_anonymous_variables__019_
                  (Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__021_)
              let set = ppx_css_variable_set__023_
              let set_all ~bar = ppx_css_variable_set__023_ () ~bar
            end
          module For_referencing =
            struct let hello = {|hello_hash_178431e471|}
                   let bar = {|--foo|} end
          let hello =
            ((Virtual_dom.Vdom.Attr.combine
                (Virtual_dom.Vdom.Attr.class_ {|hello_hash_178431e471|})
                ppx_css__internal_anonymous_variables__019_)
            [@merlin.focus ])
        end
      include Default
      let default : t = (module Default)
      let () =
        Inline_css.Private.append
          {|
      /* _none_ */

      *.hello_hash_178431e471 {
       background-color:var(--ppx_css_anonymous_var_11_hash_178431e471);
       width:var(--foo)
      }|} |xxx}]
;;

let%expect_test "dont hash" =
  test
    [%expr
      stylesheet
        {|.hello {
      background-color: %{color};
      width : var(--bar);
  }|}
        ~dont_hash:[ "--bar" ]];
  [%expect
    {xxx|
      [@@@ocaml.warning "-32"]
      let __type_info_for_ppx_css :
        ?rewrite:(string * string) list ->
          ?dont_hash:string list ->
            ?dont_hash_prefixes:string list -> string -> unit
        = fun ?rewrite:_ ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
      module type S  =
        sig
          module Variables :
          sig
            val set : ?bar:string -> unit -> Virtual_dom.Vdom.Attr.t
            val set_all : bar:string -> Virtual_dom.Vdom.Attr.t
          end
          module For_referencing : sig val bar : string val hello : string end
          val hello : Virtual_dom.Vdom.Attr.t
        end
      type t = (module S)
      module Default : S =
        struct
          let ppx_css__internal_anonymous_variables__024_ =
            let ppx_css_temp_variable__025_ = (((color)[@merlin.focus ]) : string) in
            Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
              [({|--ppx_css_anonymous_var_12_hash_16f4d23f2b|},
                 ppx_css_temp_variable__025_)]
          module Variables =
            struct
              let ppx_css_variable_set__028_ ?bar () =
                let ppx_css_acc__026_ = [] in
                let ppx_css_acc__026_ =
                  match bar with
                  | None -> ppx_css_acc__026_
                  | Some ppx_css_value__027_ -> ({|--bar|}, ppx_css_value__027_) ::
                      ppx_css_acc__026_ in
                Virtual_dom.Vdom.Attr.combine
                  ppx_css__internal_anonymous_variables__024_
                  (Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__026_)
              let set = ppx_css_variable_set__028_
              let set_all ~bar = ppx_css_variable_set__028_ () ~bar
            end
          module For_referencing =
            struct let hello = {|hello_hash_16f4d23f2b|}
                   let bar = {|--bar|} end
          let hello =
            ((Virtual_dom.Vdom.Attr.combine
                (Virtual_dom.Vdom.Attr.class_ {|hello_hash_16f4d23f2b|})
                ppx_css__internal_anonymous_variables__024_)
            [@merlin.focus ])
        end
      include Default
      let default : t = (module Default)
      let () =
        Inline_css.Private.append
          {|
      /* _none_ */

      *.hello_hash_16f4d23f2b {
       background-color:var(--ppx_css_anonymous_var_12_hash_16f4d23f2b);
       width:var(--bar)
      }|} |xxx}]
;;

let%expect_test "dont hash prefixes does not affect anonymous variables" =
  test
    [%expr
      stylesheet
        {|.hello {
      background-color: %{color};
      width : var(--bar);
  }|}
        ~dont_hash_prefixes:[ "--" ]];
  [%expect
    {xxx|
      [@@@ocaml.warning "-32"]
      let __type_info_for_ppx_css :
        ?rewrite:(string * string) list ->
          ?dont_hash:string list ->
            ?dont_hash_prefixes:string list -> string -> unit
        = fun ?rewrite:_ ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
      module type S  =
        sig
          module Variables :
          sig
            val set : ?bar:string -> unit -> Virtual_dom.Vdom.Attr.t
            val set_all : bar:string -> Virtual_dom.Vdom.Attr.t
          end
          module For_referencing : sig val bar : string val hello : string end
          val hello : Virtual_dom.Vdom.Attr.t
        end
      type t = (module S)
      module Default : S =
        struct
          let ppx_css__internal_anonymous_variables__029_ =
            let ppx_css_temp_variable__030_ = (((color)[@merlin.focus ]) : string) in
            Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
              [({|--ppx_css_anonymous_var_13_hash_c7c909c86a|},
                 ppx_css_temp_variable__030_)]
          module Variables =
            struct
              let ppx_css_variable_set__033_ ?bar () =
                let ppx_css_acc__031_ = [] in
                let ppx_css_acc__031_ =
                  match bar with
                  | None -> ppx_css_acc__031_
                  | Some ppx_css_value__032_ -> ({|--bar|}, ppx_css_value__032_) ::
                      ppx_css_acc__031_ in
                Virtual_dom.Vdom.Attr.combine
                  ppx_css__internal_anonymous_variables__029_
                  (Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__031_)
              let set = ppx_css_variable_set__033_
              let set_all ~bar = ppx_css_variable_set__033_ () ~bar
            end
          module For_referencing =
            struct let hello = {|hello_hash_c7c909c86a|}
                   let bar = {|--bar|} end
          let hello =
            ((Virtual_dom.Vdom.Attr.combine
                (Virtual_dom.Vdom.Attr.class_ {|hello_hash_c7c909c86a|})
                ppx_css__internal_anonymous_variables__029_)
            [@merlin.focus ])
        end
      include Default
      let default : t = (module Default)
      let () =
        Inline_css.Private.append
          {|
      /* _none_ */

      *.hello_hash_c7c909c86a {
       background-color:var(--ppx_css_anonymous_var_13_hash_c7c909c86a);
       width:var(--bar)
      }|} |xxx}]
;;

let%expect_test "dont-hash-prefixes unused" =
  test
    [%expr
      stylesheet
        {|.hello {
      background-color: %{color};
  }|}
        ~dont_hash_prefixes:[ "--" ]];
  [%expect {xxx|
      Unused prefixes: (--) |xxx}]
;;

let%expect_test "duplicate classes, many classes" =
  (* Interpolation expressions are only evaluated once despite appearing in many spots. *)
  test
    [%expr
      stylesheet
        {|
        .a, .b, .c:not(.d) { background-color: %{f ()}; }

        .d:hover .a { background-color: %{g ()#Color}; }
        |}];
  [%expect
    {xxx|
      [@@@ocaml.warning "-32"]
      let __type_info_for_ppx_css :
        ?rewrite:(string * string) list ->
          ?dont_hash:string list ->
            ?dont_hash_prefixes:string list -> string -> unit
        = fun ?rewrite:_ ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
      module type S  =
        sig
          module For_referencing :
          sig val a : string val b : string val c : string val d : string end
          val a : Virtual_dom.Vdom.Attr.t
          val b : Virtual_dom.Vdom.Attr.t
          val c : Virtual_dom.Vdom.Attr.t
          val d : Virtual_dom.Vdom.Attr.t
        end
      type t = (module S)
      module Default : S =
        struct
          let ppx_css__internal_anonymous_variables__035_ =
            let ppx_css_temp_variable__036_ = (((f ())[@merlin.focus ]) : string) in
            let ppx_css_temp_variable__037_ = (((Color.to_string_css (g ()))
              [@merlin.focus ]) : string) in
            Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
              [({|--ppx_css_anonymous_var_15_hash_60b0ee503a|},
                 ppx_css_temp_variable__036_);
              ({|--ppx_css_anonymous_var_16_hash_60b0ee503a|},
                ppx_css_temp_variable__037_)]
          module For_referencing =
            struct
              let c = {|c_hash_60b0ee503a|}
              let b = {|b_hash_60b0ee503a|}
              let d = {|d_hash_60b0ee503a|}
              let a = {|a_hash_60b0ee503a|}
            end
          let c =
            ((Virtual_dom.Vdom.Attr.combine
                (Virtual_dom.Vdom.Attr.class_ {|c_hash_60b0ee503a|})
                ppx_css__internal_anonymous_variables__035_)
            [@merlin.focus ])
          let b =
            ((Virtual_dom.Vdom.Attr.combine
                (Virtual_dom.Vdom.Attr.class_ {|b_hash_60b0ee503a|})
                ppx_css__internal_anonymous_variables__035_)
            [@merlin.focus ])
          let d =
            ((Virtual_dom.Vdom.Attr.combine
                (Virtual_dom.Vdom.Attr.class_ {|d_hash_60b0ee503a|})
                ppx_css__internal_anonymous_variables__035_)
            [@merlin.focus ])
          let a =
            ((Virtual_dom.Vdom.Attr.combine
                (Virtual_dom.Vdom.Attr.class_ {|a_hash_60b0ee503a|})
                ppx_css__internal_anonymous_variables__035_)
            [@merlin.focus ])
        end
      include Default
      let default : t = (module Default)
      let () =
        Inline_css.Private.append
          {|
      /* _none_ */

      *.a_hash_60b0ee503a,*.b_hash_60b0ee503a,*.c_hash_60b0ee503a:not(.d_hash_60b0ee503a) {
       background-color:var(--ppx_css_anonymous_var_15_hash_60b0ee503a)
      }

      *.d_hash_60b0ee503a:hover *.a_hash_60b0ee503a {
       background-color:var(--ppx_css_anonymous_var_16_hash_60b0ee503a)
      }|} |xxx}]
;;
