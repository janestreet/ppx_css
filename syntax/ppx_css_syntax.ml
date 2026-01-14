open! Core
open! Ppxlib
module String_constant = String_constant

type t =
  { dont_hash : String.Set.t
  ; css_string : String_constant.t
  ; dont_hash_prefixes : string list
  ; lazy_loading_optimization : Preprocess_arguments.lazy_loading_optimization
  }

module Serializable_options = struct
  type t =
    { dont_hash : string list [@sexp.list]
    ; dont_hash_prefixes : string list [@sexp.list]
    ; lazy_loading_optimization : bool option [@sexp.option]
    }
  [@@sexp.allow_extra_fields] [@@deriving sexp]

  let test s =
    let parsed = t_of_sexp (Sexp.of_string s) in
    print_s (sexp_of_t parsed)
  ;;

  let%expect_test "Regression test against non-forwards compatible serialization with \
                   Jenga Rule."
    =
    test {|((rewrite ()))|};
    [%expect {| () |}];
    test
      {|((rewrite ())
    (brand_new_field ()))|};
    [%expect {| () |}];
    test
      {|((rewrite ())
    (dont_hash ()))|};
    [%expect {| () |}];
    test
      {|((rewrite ()) (dont_hash (1
    2 3)))|};
    [%expect {| ((dont_hash (1 2 3))) |}];
    test
      {|((lazy_loading_optimization true) (dont_hash (1
    2 3)))|};
    [%expect {| ((dont_hash (1 2 3)) (lazy_loading_optimization true)) |}];
    test
      {|((lazy_loading_optimization false) (dont_hash (1
    2 3)))|};
    [%expect {| ((dont_hash (1 2 3)) (lazy_loading_optimization false)) |}]
  ;;

  let to_stylesheet_options
    { dont_hash; dont_hash_prefixes; lazy_loading_optimization }
    ~css_string
    =
    let lazy_loading_optimization =
      match lazy_loading_optimization with
      | Some true -> Preprocess_arguments.Lazy_graph
      | None | Some false -> Preprocess_arguments.Eager
    in
    { dont_hash = String.Set.of_list dont_hash
    ; css_string
    ; dont_hash_prefixes
    ; lazy_loading_optimization
    }
  ;;
end

let empty_stylesheet ~css_string =
  { dont_hash = String.Set.empty
  ; css_string
  ; dont_hash_prefixes = []
  ; lazy_loading_optimization = Preprocess_arguments.Eager
  }
;;

(* Parses the AST of a list of expressions into an actual [expression list]. *)
let parse_expr_list ~on_error expression =
  let rec helper ~acc ~expression =
    match expression.pexp_desc with
    | Pexp_construct ({ txt = Lident "[]"; _ }, None) ->
      (* NOTE: This list is reversed, but it does not matter since currently it is only
         used to populate a map, but if it ever were to matter that the list is reversed,
         then this list would need to be reversed here. *)
      acc
    | Pexp_construct ({ txt = Lident "::"; _ }, Some { pexp_desc; pexp_loc; _ }) ->
      (match Ppxlib_jane.Shim.Expression_desc.of_parsetree pexp_desc ~loc:pexp_loc with
       | Pexp_tuple [ (None, expression); (None, child) ] ->
         helper ~acc:(expression :: acc) ~expression:child
       | _ -> on_error ~loc:expression.pexp_loc)
    | _ -> on_error ~loc:expression.pexp_loc
  in
  helper ~acc:[] ~expression
;;

let raise_due_to_malformed_rewrite ~loc =
  Location.raise_errorf
    ~loc
    "%s"
    (String.strip
       {| The use of ~rewrite is deprecated, please use ~dont_hash or ~dont_hash_prefixes
       instead. Alternatively, consider writing all of your CSS in the same %css
       stylesheet incantation. Deprecating ~rewrite allows for more optimiztions in ppx_css,
       at the expense of expressibility. We've audited bonsai apps and believe this expressibility
       was unused so we've removed it. If this conflicts with your use case please reach out.|})
;;

(* Parses the AST of a [(string * string) list] of expressions where each tuple's first
   element is a string constant into a [expression String.Map.t] map. e.g parses:

   ["class1", Style.x; "class2"; "class2"]

   to:

   [String.Map.of_alist_exn ["class1", Style.x; "class2"; "class2"]] *)
let parse_rewrite ~loc:_ expression =
  parse_expr_list expression ~on_error:raise_due_to_malformed_rewrite
  |> List.map ~f:(fun expression ->
    match expression with
    | { pexp_desc = _; _ } -> raise_due_to_malformed_rewrite ~loc:expression.pexp_loc)
;;

let malformed_dont_hash_error_message =
  lazy
    (String.strip
       {|
The dont_hash argument to 'stylesheet' must be called with a list literal containing string literals.

example:
  stylesheet ~dont_hash:[ "foo_bar" ] (* Does not hash instances of "foo_bar". *)
  stylesheet ~dont_hash:[ "--bg-color" ] (* Does not hash instances of "--bg-color". *)
|})
;;

let malformed_dont_hash_prefixes_error_message =
  lazy
    (String.strip
       {|
The dont_hash_prefixes argument to 'stylesheet' must be called with a list literal containing string literals.

example:
  stylesheet ~dont_hash_prefixes:[ "--bg" ] (* Does not hashes identifiers that start with "--bg" (e.g. "--bg-color"). *)
  stylesheet ~dont_hash_prefixes:[ "--" ] (* Does not hash css variables. *)
|})
;;

let parse_string_list ~name ~syntax_error_message ~loc expression =
  let raise_on_error ~loc =
    Location.raise_errorf ~loc "%s" (force syntax_error_message)
  in
  let out =
    parse_expr_list expression ~on_error:raise_on_error
    |> List.map ~f:(fun expression ->
      match expression.pexp_desc with
      | Pexp_constant (Pconst_string (s, _, _)) -> s
      | _ -> raise_on_error ~loc:expression.pexp_loc)
  in
  let dups = List.find_all_dups out ~compare:String.compare in
  match dups with
  | [] -> out
  | dups ->
    Location.raise_errorf
      ~loc
      {| Found duplicate values %s inside of [%s]. |}
      (Sexp.to_string ([%sexp_of: string list] dups))
      name
;;

module List = struct
  include List

  (* Like [find_map], but also returns the resulting list without the found element. *)
  let take_map l ~f =
    let rec loop ~acc = function
      | [] -> None
      | hd :: tl ->
        (match f hd with
         | None -> loop ~acc:Reversed_list.(hd :: acc) tl
         | Some res -> Some (res, Reversed_list.rev_append acc tl))
    in
    loop ~acc:Reversed_list.[] l
  ;;
end

let raise_misparse_with_syntax_instructions ~extra_message ~loc =
  Location.raise_errorf
    ~loc
    "%s%%css must contain a call to [?dont_hash:string list -> dont_hash_prefixes:string \
     list -> string -> unit]"
    extra_message
;;

let raise_if_both_of_these_are_present_at_the_same_time_and_explain ~loc a b args =
  let contains identifier =
    List.exists args ~f:(fun (label, _) ->
      match label with
      | Labelled x when String.equal x identifier -> true
      | _ -> false)
  in
  match contains a, contains b with
  | true, true ->
    Location.raise_errorf
      ~loc
      {| ppx_css found unexpected arguments. Found two uses of alternate syntax in the same place which might be ambiguous/result in unexpected results. Only use 1 of "%s" or "%s".|}
      a
      b
  | (false | true), (false | true) -> ()
;;

let alternate_syntaxes =
  [ "dont_hash", "don't_hash"; "dont_hash_prefix", "don't_hash_prefix" ]
;;

let raise_if_alternate_syntaxes ~loc args =
  List.iter alternate_syntaxes ~f:(fun (a, b) ->
    raise_if_both_of_these_are_present_at_the_same_time_and_explain ~loc a b args)
;;

let validate_args ~loc ~(kind : [ `Stylesheet | `Styled_component ]) args =
  let open Option.Let_syntax in
  raise_if_alternate_syntaxes ~loc args;
  let raise_if_styled_component_because_it_is_disallowed ~loc ~argument_name =
    match kind with
    | `Stylesheet -> ()
    | `Styled_component ->
      Location.raise_errorf
        ~loc
        "~%s is a no-op as classes and ids in the *inline* ppx_css syntax are not hashed."
        argument_name
  in
  let args =
    let%map css_string, remaining_args =
      List.take_map args ~f:(fun (_, (arg : expression)) ->
        match arg.pexp_desc with
        | Pexp_constant (Pconst_string (css_string, string_loc, delimiter)) ->
          Some { String_constant.css_string; string_loc; delimiter }
        | _ -> None)
    in
    let _rewrite, remaining_args =
      List.take_map remaining_args ~f:(function
        | Labelled "rewrite", expression -> Some (parse_rewrite ~loc expression)
        | _ -> None)
      |> Option.value ~default:([], remaining_args)
    in
    let dont_hash, remaining_args =
      List.take_map remaining_args ~f:(function
        | Labelled (("dont_hash" | "don't_hash") as name), expression ->
          raise_if_styled_component_because_it_is_disallowed
            ~loc:expression.pexp_loc
            ~argument_name:name;
          Some
            (parse_string_list
               ~name
               ~syntax_error_message:malformed_dont_hash_error_message
               ~loc:expression.pexp_loc
               expression)
        | _ -> None)
      |> Option.value ~default:([], remaining_args)
    in
    let dont_hash = String.Set.of_list dont_hash in
    let dont_hash_prefixes, remaining_args =
      List.take_map remaining_args ~f:(function
        | Labelled (("dont_hash_prefixes" | "don't_hash_prefixes") as name), expression ->
          raise_if_styled_component_because_it_is_disallowed
            ~loc:expression.pexp_loc
            ~argument_name:name;
          Some
            (parse_string_list
               ~name
               ~syntax_error_message:malformed_dont_hash_prefixes_error_message
               ~loc:expression.pexp_loc
               expression)
        | _ -> None)
      |> Option.value ~default:([], remaining_args)
    in
    css_string, dont_hash, remaining_args, dont_hash_prefixes
  in
  match args with
  | None | Some (_, _, _ :: _, _) ->
    raise_misparse_with_syntax_instructions
      ~extra_message:"ppx_css found unexpected arguments. "
      ~loc
  | Some (css_string, dont_hash, [], dont_hash_prefixes) ->
    css_string, dont_hash, dont_hash_prefixes
;;

let add_identifiers_from_jbuild_parameters ~loc (options : t) =
  let open (val Ast_builder.make loc) in
  let { Preprocess_arguments.dont_hash = jbuild_dont_hash
      ; dont_hash_prefixes = jbuild_dont_hash_prefixes
      ; lazy_loading_optimization
      ; _
      }
    =
    Preprocess_arguments.get ()
  in
  let dont_hash_prefixes =
    options.dont_hash_prefixes @ Set.to_list jbuild_dont_hash_prefixes
  in
  let dont_hash = Set.union options.dont_hash jbuild_dont_hash in
  { dont_hash
  ; dont_hash_prefixes
  ; css_string = options.css_string
  ; lazy_loading_optimization
  }
;;

let parse_stylesheet_exn (expression : expression) =
  let loc = expression.pexp_loc in
  let loc = { loc with loc_ghost = true } in
  match expression.pexp_desc with
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "stylesheet"; loc }; _ }, args) ->
    let css_string, dont_hash, dont_hash_prefixes =
      validate_args ~loc ~kind:`Stylesheet args
    in
    add_identifiers_from_jbuild_parameters
      ~loc
      { css_string
      ; dont_hash
      ; dont_hash_prefixes
        (* Lazy loading optimization is ignored here as the value is pulled from the
           jbuild and not the individual calls to ppx_css *)
      ; lazy_loading_optimization = Preprocess_arguments.Eager
      }
  | _ -> raise_misparse_with_syntax_instructions ~extra_message:"" ~loc
;;

let parse_inline_expression_exn (expression : expression) =
  let loc = expression.pexp_loc in
  let loc = { loc with loc_ghost = true } in
  let css_string, dont_hash, dont_hash_prefixes =
    match expression.pexp_desc with
    | Pexp_apply
        (({ pexp_desc = Pexp_constant (Pconst_string (_, loc, _)); _ } as string), args)
      ->
      let args = (Nolabel, string) :: args in
      let css_string, dont_hash, dont_hash_prefixes =
        validate_args ~loc ~kind:`Styled_component args
      in
      css_string, dont_hash, dont_hash_prefixes
    | Pexp_constant (Pconst_string (_, loc, _)) ->
      let args = [ Nolabel, expression ] in
      let css_string, dont_hash, dont_hash_prefixes =
        validate_args ~loc ~kind:`Styled_component args
      in
      css_string, dont_hash, dont_hash_prefixes
    | _ ->
      Location.raise_errorf
        ~loc
        "%%css must contain a call to [val {|css string|} : ?rewrite:(string * string) \
         list -> ?dont_hash:string list -> dont_hash_prefixes:string list -> string -> \
         unit]"
  in
  add_identifiers_from_jbuild_parameters
    ~loc
    { css_string
    ; dont_hash
    ; dont_hash_prefixes
      (* Lazy loading optimization is ignored here as the value is pulled from the jbuild
         and not the individual calls to ppx_css *)
    ; lazy_loading_optimization = Preprocess_arguments.Eager
    }
;;

module Preprocess_arguments = Preprocess_arguments
