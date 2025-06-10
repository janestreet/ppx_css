open! Core
open Ppxlib
open Ppx_css

let loc = Test_util.loc_with_mock_name
let () = Ppx_css_syntax.Preprocess_arguments.set_lazy_loading_optimization (Some true)

let catch_location_error ~f =
  try f () with
  | ex ->
    (match Location.Error.of_exn ex with
     | Some error -> print_endline (Location.Error.message error)
     | None -> raise ex)
;;

let test expr =
  catch_location_error ~f:(fun () ->
    let%tydi { txt = structure; hoisted_structure_items; _ } =
      expr |> For_testing.generate_struct ~loc ~disable_hashing:false
    in
    structure |> Pprintast.string_of_structure |> print_endline;
    print_endline (Pprintast.string_of_structure hoisted_structure_items))
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
      = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
    module type S  =
      sig
        module For_referencing : sig val hello : string end
        val hello : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        let ppx_css__internal_anonymous_variables__001_ =
          let ppx_css_temp_variable__003_ = (((color)[@merlin.focus ]) : string) in
          Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
            [({|--ppx_css_anonymous_var_1_hash_1cf8ea41b4|},
               ppx_css_temp_variable__003_)]
        module For_referencing = struct let hello = {|hello_hash_1cf8ea41b4|} end
        let hello =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__002___1cf8ea41b4__group_0;
                ((Virtual_dom.Vdom.Attr.combine
                    (Virtual_dom.Vdom.Attr.class_ {|hello_hash_1cf8ea41b4|})
                    ppx_css__internal_anonymous_variables__001_)
                [@merlin.focus ])))
      end
    include Default
    let default : t = (module Default)
    let sheet_x__002___1cf8ea41b4__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__002___1cf8ea41b4__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__002___1cf8ea41b4__0
           {|
    /* app/foo/foo.ml */

    .hello_hash_1cf8ea41b4 {
      background-color: var(--ppx_css_anonymous_var_1_hash_1cf8ea41b4);
    }|})
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
      = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
    module type S  =
      sig
        module For_referencing : sig val hello : string end
        val hello : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        let ppx_css__internal_anonymous_variables__004_ =
          let ppx_css_temp_variable__006_ = (((color)[@merlin.focus ]) : string) in
          let ppx_css_temp_variable__007_ = (((color2)[@merlin.focus ]) : string) in
          Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
            [({|--ppx_css_anonymous_var_2_hash_1934b652f4|},
               ppx_css_temp_variable__006_);
            ({|--ppx_css_anonymous_var_3_hash_1934b652f4|},
              ppx_css_temp_variable__007_)]
        module For_referencing = struct let hello = {|hello_hash_1934b652f4|} end
        let hello =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__005___1934b652f4__group_0;
                ((Virtual_dom.Vdom.Attr.combine
                    (Virtual_dom.Vdom.Attr.class_ {|hello_hash_1934b652f4|})
                    ppx_css__internal_anonymous_variables__004_)
                [@merlin.focus ])))
      end
    include Default
    let default : t = (module Default)
    let sheet_x__005___1934b652f4__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__005___1934b652f4__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__005___1934b652f4__0
           {|
    /* app/foo/foo.ml */

    .hello_hash_1934b652f4 {
      background-color: var(--ppx_css_anonymous_var_2_hash_1934b652f4);
      color: var(--ppx_css_anonymous_var_3_hash_1934b652f4);
    }|})
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
      = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
    module type S  =
      sig
        module For_referencing : sig val hello : string end
        val hello : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        let ppx_css__internal_anonymous_variables__008_ =
          let ppx_css_temp_variable__010_ = (((Css_gen.Color.to_string_css color)
            [@merlin.focus ]) : string) in
          let ppx_css_temp_variable__011_ =
            (((Css_gen.Length.to_string_css width)[@merlin.focus ]) : string) in
          Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
            [({|--ppx_css_anonymous_var_4_hash_58ff4c7f8d|},
               ppx_css_temp_variable__010_);
            ({|--ppx_css_anonymous_var_5_hash_58ff4c7f8d|},
              ppx_css_temp_variable__011_)]
        module For_referencing = struct let hello = {|hello_hash_58ff4c7f8d|} end
        let hello =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__009___58ff4c7f8d__group_0;
                ((Virtual_dom.Vdom.Attr.combine
                    (Virtual_dom.Vdom.Attr.class_ {|hello_hash_58ff4c7f8d|})
                    ppx_css__internal_anonymous_variables__008_)
                [@merlin.focus ])))
      end
    include Default
    let default : t = (module Default)
    let sheet_x__009___58ff4c7f8d__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__009___58ff4c7f8d__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__009___58ff4c7f8d__0
           {|
    /* app/foo/foo.ml */

    .hello_hash_58ff4c7f8d {
      background-color: var(--ppx_css_anonymous_var_4_hash_58ff4c7f8d);
      min-width: var(--ppx_css_anonymous_var_5_hash_58ff4c7f8d);
    }|})
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
      = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
    module type S  =
      sig
        module For_referencing : sig val hello : string end
        val hello : Virtual_dom.Vdom.Attr.t
      end
    type t = (module S)
    module Default : S =
      struct
        let ppx_css__internal_anonymous_variables__012_ =
          let ppx_css_temp_variable__014_ =
            (((Css_gen.Color.to_string_css (first ()))[@merlin.focus ]) :
            string) in
          let ppx_css_temp_variable__015_ = (((Beep.to_string_css (second ()))
            [@merlin.focus ]) : string) in
          let ppx_css_temp_variable__016_ = (((third ())
            [@merlin.focus ]) : string) in
          let ppx_css_temp_variable__017_ =
            (((Css_gen.Color.to_string_css (fourth ()))
            [@merlin.focus ]) : string) in
          Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
            [({|--ppx_css_anonymous_var_6_hash_3813a8f4d2|},
               ppx_css_temp_variable__014_);
            ({|--ppx_css_anonymous_var_7_hash_3813a8f4d2|},
              ppx_css_temp_variable__015_);
            ({|--ppx_css_anonymous_var_8_hash_3813a8f4d2|},
              ppx_css_temp_variable__016_);
            ({|--ppx_css_anonymous_var_9_hash_3813a8f4d2|},
              ppx_css_temp_variable__017_)]
        module For_referencing = struct let hello = {|hello_hash_3813a8f4d2|} end
        let hello =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__013___3813a8f4d2__group_0;
                ((Virtual_dom.Vdom.Attr.combine
                    (Virtual_dom.Vdom.Attr.class_ {|hello_hash_3813a8f4d2|})
                    ppx_css__internal_anonymous_variables__012_)
                [@merlin.focus ])))
      end
    include Default
    let default : t = (module Default)
    let sheet_x__013___3813a8f4d2__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__013___3813a8f4d2__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__013___3813a8f4d2__0
           {|
    /* app/foo/foo.ml */

    .hello_hash_3813a8f4d2 {
      background-color: var(--ppx_css_anonymous_var_6_hash_3813a8f4d2);
      background-color: var(--ppx_css_anonymous_var_7_hash_3813a8f4d2);
      background-color: var(--ppx_css_anonymous_var_8_hash_3813a8f4d2);
      background-color: var(--ppx_css_anonymous_var_9_hash_3813a8f4d2);
    }|})
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
      = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
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
        let ppx_css__internal_anonymous_variables__018_ =
          let ppx_css_temp_variable__020_ = (((color)[@merlin.focus ]) : string) in
          Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
            [({|--ppx_css_anonymous_var_10_hash_a6ac6c0f21|},
               ppx_css_temp_variable__020_)]
        module Variables =
          struct
            let ppx_css_variable_set__023_ ?bar () =
              let ppx_css_acc__021_ = [] in
              let ppx_css_acc__021_ =
                match bar with
                | None -> ppx_css_acc__021_
                | Some ppx_css_value__022_ ->
                    ({|--bar_hash_a6ac6c0f21|}, ppx_css_value__022_) ::
                    ppx_css_acc__021_ in
              Virtual_dom.Vdom.Attr.combine
                ppx_css__internal_anonymous_variables__018_
                (Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__021_)
            let set = ppx_css_variable_set__023_
            let set_all ~bar = ppx_css_variable_set__023_ () ~bar
          end
        module For_referencing =
          struct
            let bar = {|--bar_hash_a6ac6c0f21|}
            let hello = {|hello_hash_a6ac6c0f21|}
          end
        let hello =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__019___a6ac6c0f21__group_0;
                ((Virtual_dom.Vdom.Attr.combine
                    (Virtual_dom.Vdom.Attr.class_ {|hello_hash_a6ac6c0f21|})
                    ppx_css__internal_anonymous_variables__018_)
                [@merlin.focus ])))
      end
    include Default
    let default : t = (module Default)
    let sheet_x__019___a6ac6c0f21__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__019___a6ac6c0f21__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__019___a6ac6c0f21__0
           {|
    /* app/foo/foo.ml */

    .hello_hash_a6ac6c0f21 {
      background-color: var(--ppx_css_anonymous_var_10_hash_a6ac6c0f21);
      width: var(--bar_hash_a6ac6c0f21);
    }|})
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
  }|}
        ~rewrite:[ "--bar", "--foo" ]];
  [%expect
    {xxx|
    The use of ~rewrite is deprecated, please use ~dont_hash or ~dont_hash_prefixes
           instead. Alternatively, consider writing all of your CSS in the same %css
           stylesheet incantation. Deprecating ~rewrite allows for more optimiztions in ppx_css,
           at the expense of expressibility. We've audited bonsai apps and believe this expressibility
           was unused so we've removed it. If this conflicts with your use case please reach out.
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
      = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
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
          let ppx_css_temp_variable__026_ = (((color)[@merlin.focus ]) : string) in
          Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
            [({|--ppx_css_anonymous_var_11_hash_a6ac6c0f21|},
               ppx_css_temp_variable__026_)]
        module Variables =
          struct
            let ppx_css_variable_set__029_ ?bar () =
              let ppx_css_acc__027_ = [] in
              let ppx_css_acc__027_ =
                match bar with
                | None -> ppx_css_acc__027_
                | Some ppx_css_value__028_ -> ({|--bar|}, ppx_css_value__028_) ::
                    ppx_css_acc__027_ in
              Virtual_dom.Vdom.Attr.combine
                ppx_css__internal_anonymous_variables__024_
                (Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__027_)
            let set = ppx_css_variable_set__029_
            let set_all ~bar = ppx_css_variable_set__029_ () ~bar
          end
        module For_referencing =
          struct let bar = {|--bar|}
                 let hello = {|hello_hash_a6ac6c0f21|} end
        let hello =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__025___a6ac6c0f21__group_0;
                ((Virtual_dom.Vdom.Attr.combine
                    (Virtual_dom.Vdom.Attr.class_ {|hello_hash_a6ac6c0f21|})
                    ppx_css__internal_anonymous_variables__024_)
                [@merlin.focus ])))
      end
    include Default
    let default : t = (module Default)
    let sheet_x__025___a6ac6c0f21__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__025___a6ac6c0f21__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__025___a6ac6c0f21__0
           {|
    /* app/foo/foo.ml */

    .hello_hash_a6ac6c0f21 {
      background-color: var(--ppx_css_anonymous_var_11_hash_a6ac6c0f21);
      width: var(--bar);
    }|})
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
      = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
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
        let ppx_css__internal_anonymous_variables__030_ =
          let ppx_css_temp_variable__032_ = (((color)[@merlin.focus ]) : string) in
          Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
            [({|--ppx_css_anonymous_var_12_hash_a6ac6c0f21|},
               ppx_css_temp_variable__032_)]
        module Variables =
          struct
            let ppx_css_variable_set__035_ ?bar () =
              let ppx_css_acc__033_ = [] in
              let ppx_css_acc__033_ =
                match bar with
                | None -> ppx_css_acc__033_
                | Some ppx_css_value__034_ -> ({|--bar|}, ppx_css_value__034_) ::
                    ppx_css_acc__033_ in
              Virtual_dom.Vdom.Attr.combine
                ppx_css__internal_anonymous_variables__030_
                (Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__033_)
            let set = ppx_css_variable_set__035_
            let set_all ~bar = ppx_css_variable_set__035_ () ~bar
          end
        module For_referencing =
          struct let bar = {|--bar|}
                 let hello = {|hello_hash_a6ac6c0f21|} end
        let hello =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__031___a6ac6c0f21__group_0;
                ((Virtual_dom.Vdom.Attr.combine
                    (Virtual_dom.Vdom.Attr.class_ {|hello_hash_a6ac6c0f21|})
                    ppx_css__internal_anonymous_variables__030_)
                [@merlin.focus ])))
      end
    include Default
    let default : t = (module Default)
    let sheet_x__031___a6ac6c0f21__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__031___a6ac6c0f21__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__031___a6ac6c0f21__0
           {|
    /* app/foo/foo.ml */

    .hello_hash_a6ac6c0f21 {
      background-color: var(--ppx_css_anonymous_var_12_hash_a6ac6c0f21);
      width: var(--bar);
    }|})
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
      = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
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
        let ppx_css__internal_anonymous_variables__036_ =
          let ppx_css_temp_variable__038_ = (((color)[@merlin.focus ]) : string) in
          Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
            [({|--ppx_css_anonymous_var_13_hash_a6ac6c0f21|},
               ppx_css_temp_variable__038_)]
        module Variables =
          struct
            let ppx_css_variable_set__041_ ?bar () =
              let ppx_css_acc__039_ = [] in
              let ppx_css_acc__039_ =
                match bar with
                | None -> ppx_css_acc__039_
                | Some ppx_css_value__040_ -> ({|--bar|}, ppx_css_value__040_) ::
                    ppx_css_acc__039_ in
              Virtual_dom.Vdom.Attr.combine
                ppx_css__internal_anonymous_variables__036_
                (Virtual_dom.Vdom.Attr.__css_vars_no_kebabs ppx_css_acc__039_)
            let set = ppx_css_variable_set__041_
            let set_all ~bar = ppx_css_variable_set__041_ () ~bar
          end
        module For_referencing =
          struct let bar = {|--bar|}
                 let hello = {|hello_hash_a6ac6c0f21|} end
        let hello =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__037___a6ac6c0f21__group_0;
                ((Virtual_dom.Vdom.Attr.combine
                    (Virtual_dom.Vdom.Attr.class_ {|hello_hash_a6ac6c0f21|})
                    ppx_css__internal_anonymous_variables__036_)
                [@merlin.focus ])))
      end
    include Default
    let default : t = (module Default)
    let sheet_x__037___a6ac6c0f21__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__037___a6ac6c0f21__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__037___a6ac6c0f21__0
           {|
    /* app/foo/foo.ml */

    .hello_hash_a6ac6c0f21 {
      background-color: var(--ppx_css_anonymous_var_13_hash_a6ac6c0f21);
      width: var(--bar);
    }|})
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
      = fun ?dont_hash:_  ?dont_hash_prefixes:_  _ -> ()
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
        let ppx_css__internal_anonymous_variables__043_ =
          let ppx_css_temp_variable__045_ = (((f ())[@merlin.focus ]) : string) in
          let ppx_css_temp_variable__046_ = (((Color.to_string_css (g ()))
            [@merlin.focus ]) : string) in
          Virtual_dom.Vdom.Attr.__css_vars_no_kebabs
            [({|--ppx_css_anonymous_var_15_hash_e9cc665259|},
               ppx_css_temp_variable__045_);
            ({|--ppx_css_anonymous_var_16_hash_e9cc665259|},
              ppx_css_temp_variable__046_)]
        module For_referencing =
          struct
            let a = {|a_hash_e9cc665259|}
            let b = {|b_hash_e9cc665259|}
            let c = {|c_hash_e9cc665259|}
            let d = {|d_hash_e9cc665259|}
          end
        let a =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__044___e9cc665259__group_0;
                ((Virtual_dom.Vdom.Attr.combine
                    (Virtual_dom.Vdom.Attr.class_ {|a_hash_e9cc665259|})
                    ppx_css__internal_anonymous_variables__043_)
                [@merlin.focus ])))
        let b =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__044___e9cc665259__group_0;
                ((Virtual_dom.Vdom.Attr.combine
                    (Virtual_dom.Vdom.Attr.class_ {|b_hash_e9cc665259|})
                    ppx_css__internal_anonymous_variables__043_)
                [@merlin.focus ])))
        let c =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__044___e9cc665259__group_0;
                ((Virtual_dom.Vdom.Attr.combine
                    (Virtual_dom.Vdom.Attr.class_ {|c_hash_e9cc665259|})
                    ppx_css__internal_anonymous_variables__043_)
                [@merlin.focus ])))
        let d =
          Virtual_dom.Vdom.Attr.lazy_
            (lazy
               (Inline_css.Ppx_css_runtime.force
                  Ppx_css_hoister_do_not_collide.update_sheet_lazy_fn_x__044___e9cc665259__group_0;
                ((Virtual_dom.Vdom.Attr.combine
                    (Virtual_dom.Vdom.Attr.class_ {|d_hash_e9cc665259|})
                    ppx_css__internal_anonymous_variables__043_)
                [@merlin.focus ])))
      end
    include Default
    let default : t = (module Default)
    let sheet_x__044___e9cc665259__0 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let sheet_x__044___e9cc665259__1 =
      let sheet = Inline_css.Private.create_stylesheet () in
      Inline_css.Private.append_stylesheet sheet; sheet
    let update_sheet_lazy_fn_x__044___e9cc665259__group_0 =
      lazy
        (Inline_css.Private.update_stylesheet sheet_x__044___e9cc665259__0
           {|
    /* app/foo/foo.ml */

    .a_hash_e9cc665259, .b_hash_e9cc665259, .c_hash_e9cc665259:not(.d_hash_e9cc665259) {
      background-color: var(--ppx_css_anonymous_var_15_hash_e9cc665259);
    }|};
         Inline_css.Private.update_stylesheet sheet_x__044___e9cc665259__1
           {|
    /* app/foo/foo.ml */

    .d_hash_e9cc665259:hover .a_hash_e9cc665259 {
      background-color: var(--ppx_css_anonymous_var_16_hash_e9cc665259);
    }|})
    |xxx}]
;;
