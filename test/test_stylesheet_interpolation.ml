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
    let%tydi { txt = structure; ppx_css_string_expression; _ } =
      expr |> For_testing.generate_struct
    in
    structure |> Pprintast.string_of_structure |> print_endline;
    print_endline
      (Pprintast.string_of_structure
         [ For_testing.ppx_css_expression_to_structure_item ~loc ppx_css_string_expression
         ]))
;;

let%expect_test "basic" =
  test
    [%expr
      stylesheet
        {|.hello {
      background-color: %{color};
  }|}];
  [%expect
    {xxx|
    [@@@ocaml.warning "-32"]
    let __type_info_for_ppx_css :
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
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
            [({|--ppx_css_anonymous_var_1_hash_c368ca6a23|},
               ppx_css_temp_variable__002_)]
        module For_referencing = struct let hello = {|hello_hash_c368ca6a23|} end
        let hello =
          ((Virtual_dom.Vdom.Attr.combine
              (Virtual_dom.Vdom.Attr.class_ {|hello_hash_c368ca6a23|})
              ppx_css__internal_anonymous_variables__001_)
          [@merlin.focus ])
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.hello_hash_c368ca6a23 {
     background-color:var(--ppx_css_anonymous_var_1_hash_c368ca6a23)
    }|}
    |xxx}]
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
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
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
            [({|--ppx_css_anonymous_var_2_hash_7ae0464426|},
               ppx_css_temp_variable__004_);
            ({|--ppx_css_anonymous_var_3_hash_7ae0464426|},
              ppx_css_temp_variable__005_)]
        module For_referencing = struct let hello = {|hello_hash_7ae0464426|} end
        let hello =
          ((Virtual_dom.Vdom.Attr.combine
              (Virtual_dom.Vdom.Attr.class_ {|hello_hash_7ae0464426|})
              ppx_css__internal_anonymous_variables__003_)
          [@merlin.focus ])
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.hello_hash_7ae0464426 {
     background-color:var(--ppx_css_anonymous_var_2_hash_7ae0464426);
     color:var(--ppx_css_anonymous_var_3_hash_7ae0464426)
    }|}
    |xxx}]
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
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
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
            [({|--ppx_css_anonymous_var_4_hash_87c0496d61|},
               ppx_css_temp_variable__007_);
            ({|--ppx_css_anonymous_var_5_hash_87c0496d61|},
              ppx_css_temp_variable__008_)]
        module For_referencing = struct let hello = {|hello_hash_87c0496d61|} end
        let hello =
          ((Virtual_dom.Vdom.Attr.combine
              (Virtual_dom.Vdom.Attr.class_ {|hello_hash_87c0496d61|})
              ppx_css__internal_anonymous_variables__006_)
          [@merlin.focus ])
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.hello_hash_87c0496d61 {
     background-color:var(--ppx_css_anonymous_var_4_hash_87c0496d61);
     min-width:var(--ppx_css_anonymous_var_5_hash_87c0496d61)
    }|}
    |xxx}]
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
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
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
            [({|--ppx_css_anonymous_var_6_hash_31d44df124|},
               ppx_css_temp_variable__010_);
            ({|--ppx_css_anonymous_var_7_hash_31d44df124|},
              ppx_css_temp_variable__011_);
            ({|--ppx_css_anonymous_var_8_hash_31d44df124|},
              ppx_css_temp_variable__012_);
            ({|--ppx_css_anonymous_var_9_hash_31d44df124|},
              ppx_css_temp_variable__013_)]
        module For_referencing = struct let hello = {|hello_hash_31d44df124|} end
        let hello =
          ((Virtual_dom.Vdom.Attr.combine
              (Virtual_dom.Vdom.Attr.class_ {|hello_hash_31d44df124|})
              ppx_css__internal_anonymous_variables__009_)
          [@merlin.focus ])
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.hello_hash_31d44df124 {
     background-color:var(--ppx_css_anonymous_var_6_hash_31d44df124);
     background-color:var(--ppx_css_anonymous_var_7_hash_31d44df124);
     background-color:var(--ppx_css_anonymous_var_8_hash_31d44df124);
     background-color:var(--ppx_css_anonymous_var_9_hash_31d44df124)
    }|}
    |xxx}]
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
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
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
            [({|--ppx_css_anonymous_var_10_hash_bd7325e441|},
               ppx_css_temp_variable__015_)]
        module Variables =
          struct
            let ppx_css_variable_set__018_ ?bar () =
              let ppx_css_acc__016_ = [] in
              let ppx_css_acc__016_ =
                match bar with
                | None -> ppx_css_acc__016_
                | Some ppx_css_value__017_ ->
                    ({|--bar_hash_bd7325e441|}, ppx_css_value__017_) ::
                    ppx_css_acc__016_ in
              Virtual_dom.Vdom.Attr.combine
                ppx_css__internal_anonymous_variables__014_
                (Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__016_)
            let set = ppx_css_variable_set__018_
            let set_all ~bar = ppx_css_variable_set__018_ () ~bar
          end
        module For_referencing =
          struct
            let hello = {|hello_hash_bd7325e441|}
            let bar = {|--bar_hash_bd7325e441|}
          end
        let hello =
          ((Virtual_dom.Vdom.Attr.combine
              (Virtual_dom.Vdom.Attr.class_ {|hello_hash_bd7325e441|})
              ppx_css__internal_anonymous_variables__014_)
          [@merlin.focus ])
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.hello_hash_bd7325e441 {
     background-color:var(--ppx_css_anonymous_var_10_hash_bd7325e441);
     width:var(--bar_hash_bd7325e441)
    }|}
    |xxx}]
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
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
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
            [({|--ppx_css_anonymous_var_11_hash_7e30fd8af0|},
               ppx_css_temp_variable__020_)]
        module Variables =
          struct
            let ppx_css_variable_set__023_ ?bar () =
              let ppx_css_acc__021_ = [] in
              let ppx_css_acc__021_ =
                match bar with
                | None -> ppx_css_acc__021_
                | Some ppx_css_value__022_ -> ({|--bar|}, ppx_css_value__022_) ::
                    ppx_css_acc__021_ in
              Virtual_dom.Vdom.Attr.combine
                ppx_css__internal_anonymous_variables__019_
                (Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__021_)
            let set = ppx_css_variable_set__023_
            let set_all ~bar = ppx_css_variable_set__023_ () ~bar
          end
        module For_referencing =
          struct let hello = {|hello_hash_7e30fd8af0|}
                 let bar = {|--bar|} end
        let hello =
          ((Virtual_dom.Vdom.Attr.combine
              (Virtual_dom.Vdom.Attr.class_ {|hello_hash_7e30fd8af0|})
              ppx_css__internal_anonymous_variables__019_)
          [@merlin.focus ])
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.hello_hash_7e30fd8af0 {
     background-color:var(--ppx_css_anonymous_var_11_hash_7e30fd8af0);
     width:var(--bar)
    }|}
    |xxx}]
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
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
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
            [({|--ppx_css_anonymous_var_12_hash_cc977643bc|},
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
          struct let hello = {|hello_hash_cc977643bc|}
                 let bar = {|--bar|} end
        let hello =
          ((Virtual_dom.Vdom.Attr.combine
              (Virtual_dom.Vdom.Attr.class_ {|hello_hash_cc977643bc|})
              ppx_css__internal_anonymous_variables__024_)
          [@merlin.focus ])
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.hello_hash_cc977643bc {
     background-color:var(--ppx_css_anonymous_var_12_hash_cc977643bc);
     width:var(--bar)
    }|}
    |xxx}]
;;

let%expect_test "dont-hash-prefixes unused" =
  test
    [%expr
      stylesheet
        {|.hello {
      background-color: %{color};
  }|}
        ~dont_hash_prefixes:[ "--" ]];
  [%expect {xxx| Unused prefixes: (--) |xxx}]
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
      ?dont_hash:string list -> ?dont_hash_prefixes:string list -> string -> unit
      = fun ?dont_hash:_ ?dont_hash_prefixes:_ _ -> ()
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
        let ppx_css__internal_anonymous_variables__030_ =
          let ppx_css_temp_variable__031_ = (((f ())[@merlin.focus ]) : string) in
          let ppx_css_temp_variable__032_ = (((Color.to_string_css (g ()))
            [@merlin.focus ]) : string) in
          Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
            [({|--ppx_css_anonymous_var_14_hash_5c7e0b4335|},
               ppx_css_temp_variable__031_);
            ({|--ppx_css_anonymous_var_15_hash_5c7e0b4335|},
              ppx_css_temp_variable__032_)]
        module For_referencing =
          struct
            let c = {|c_hash_5c7e0b4335|}
            let b = {|b_hash_5c7e0b4335|}
            let d = {|d_hash_5c7e0b4335|}
            let a = {|a_hash_5c7e0b4335|}
          end
        let c =
          ((Virtual_dom.Vdom.Attr.combine
              (Virtual_dom.Vdom.Attr.class_ {|c_hash_5c7e0b4335|})
              ppx_css__internal_anonymous_variables__030_)
          [@merlin.focus ])
        let b =
          ((Virtual_dom.Vdom.Attr.combine
              (Virtual_dom.Vdom.Attr.class_ {|b_hash_5c7e0b4335|})
              ppx_css__internal_anonymous_variables__030_)
          [@merlin.focus ])
        let d =
          ((Virtual_dom.Vdom.Attr.combine
              (Virtual_dom.Vdom.Attr.class_ {|d_hash_5c7e0b4335|})
              ppx_css__internal_anonymous_variables__030_)
          [@merlin.focus ])
        let a =
          ((Virtual_dom.Vdom.Attr.combine
              (Virtual_dom.Vdom.Attr.class_ {|a_hash_5c7e0b4335|})
              ppx_css__internal_anonymous_variables__030_)
          [@merlin.focus ])
      end
    include Default
    let default : t = (module Default)
    let () =
      Inline_css.Private.append_but_do_not_update
        {|
    /* _none_ */

    *.a_hash_5c7e0b4335,*.b_hash_5c7e0b4335,*.c_hash_5c7e0b4335:not(.d_hash_5c7e0b4335) {
     background-color:var(--ppx_css_anonymous_var_14_hash_5c7e0b4335)
    }

    *.d_hash_5c7e0b4335:hover *.a_hash_5c7e0b4335 {
     background-color:var(--ppx_css_anonymous_var_15_hash_5c7e0b4335)
    }|}
    |xxx}]
;;
