open! Core
open! Ppxlib

type t =
  { rewrite : expression String.Map.t
  ; css_string : string
  ; stylesheet_location : location
  ; dont_hash_prefixes : string list
  }

let combine_rewrite_and_dont_hash ~loc ~rewrite ~dont_hash =
  List.fold dont_hash ~init:rewrite ~f:(fun acc dont_hash_this ->
    Map.update acc dont_hash_this ~f:(function
      | None ->
        let open (val Ast_builder.make loc) in
        pexp_constant (Pconst_string (dont_hash_this, loc, None))
      | Some _ ->
        Location.raise_errorf
          ~loc
          {|Found duplicate value \"%s\" between [dont_hash] and [rewrite].|}
          dont_hash_this))
;;

module Serializable_options = struct
  type t =
    { rewrite : string String.Map.t
    ; dont_hash : string list [@sexp.list]
    ; dont_hash_prefixes : string list [@sexp.list]
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
    [%expect {| ((rewrite ())) |}];
    test {|((rewrite ()) (brand_new_field ()))|};
    [%expect {| ((rewrite ())) |}];
    test {|((rewrite ()) (dont_hash ()))|};
    [%expect {| ((rewrite ())) |}];
    test {|((rewrite ()) (dont_hash (1 2 3)))|};
    [%expect {| ((rewrite ()) (dont_hash (1 2 3))) |}]
  ;;

  (* Parses the string "A.B.x" into a Ppxlib AST corresponding to an identifier.

     Also parses the string "x" into a Ppxlib AST correponding to the string constant
     "x".
  *)
  let parse_string_to_expression : string -> expression =
    fun s ->
    let loc = Location.none in
    let open (val Ast_builder.make loc) in
    let items = String.split ~on:'.' s in
    match items with
    | [ single ] -> pexp_constant (Pconst_string (single, Location.none, None))
    | first :: tl ->
      List.fold ~init:(Lident first) tl ~f:(fun acc item -> Ldot (acc, item))
      |> Located.mk
      |> pexp_ident
    | _ -> raise_s (Sexp.Atom "Expected a valid Ocaml identifier expression")
  ;;

  let to_options { rewrite; dont_hash; dont_hash_prefixes } ~css_string =
    { rewrite =
        (let rewrite = Map.map rewrite ~f:(fun s -> parse_string_to_expression s) in
         combine_rewrite_and_dont_hash ~loc:Location.none ~rewrite ~dont_hash)
    ; css_string
    ; stylesheet_location = Location.none
    ; dont_hash_prefixes
    }
  ;;
end

let empty ~css_string =
  { rewrite = String.Map.empty
  ; css_string
  ; stylesheet_location = Location.none
  ; dont_hash_prefixes = []
  }
;;

(* Parses the AST of a list of expressions into an actual [expression list]. *)
let parse_expr_list ~on_error expression =
  let rec helper ~acc ~expression =
    match expression.pexp_desc with
    | Pexp_construct ({ txt = Lident "[]"; _ }, None) ->
      (* NOTE: This list is reversed, but it does not matter since currently it is only
         used to populate a map, but if it ever were to matter that the list is
         reversed, then this list would need to be reversed here.   *)
      acc
    | Pexp_construct
        ( { txt = Lident "::"; _ }
        , Some { pexp_desc = Pexp_tuple [ expression; child ]; _ } ) ->
      helper ~acc:(expression :: acc) ~expression:child
    | _ -> on_error ~loc:expression.pexp_loc
  in
  helper ~acc:[] ~expression
;;

let loc_ghoster =
  object
    inherit Ast_traverse.map as super
    method! location location = super#location { location with loc_ghost = true }
  end
;;

let raise_due_to_malformed_rewrite ~loc =
  Location.raise_errorf
    ~loc
    "%s"
    (String.strip
       {|
The rewrite argument to 'stylesheet' must be called with a list literal containing tuple literals,
where the first element of the tuple must be a string literal (the second element in the tuple can be
any expression which evaluates to a string.)

examples:
  stylesheet ~rewrite:[ "foo_bar", "foo-bar" ] (* Rewrites instances of "foo_bar" in the css string to "foo-bar" *)
  stylesheet ~rewrite:[ "foo-bar", "foo-bar" ] (* Prevents the "foo-bar" identifier from being hashed for uniqueness *)
  stylesheet ~rewrite:[ "my_table", My_table_component.table ] (* References an identifier defined in another module *)
|})
;;

(* Parses the AST of a [(string * string) list] of expressions where each tuple's first
   element is a string constant into a [expression String.Map.t] map. e.g parses:

   ["class1", Style.x; "class2"; "class2"]

   to:

   [String.Map.of_alist_exn ["class1", Style.x; "class2"; "class2"]]  *)
let parse_rewrite ~loc expression =
  let alist =
    parse_expr_list expression ~on_error:raise_due_to_malformed_rewrite
    |> List.map ~f:(fun expression ->
      match expression with
      | { pexp_desc =
            Pexp_tuple
              [ { pexp_desc = Pexp_constant (Pconst_string (key, _, _)); _ }; value ]
        ; _
        } -> key, loc_ghoster#expression value
      | { pexp_desc = _; _ } -> raise_due_to_malformed_rewrite ~loc:expression.pexp_loc)
  in
  match String.Map.of_alist alist with
  | `Ok rewrite -> rewrite
  | `Duplicate_key key ->
    Location.raise_errorf ~loc "Found duplicate key \"%s\" inside of [rewrite]." key
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
    "%s%%css must contain a call to [val stylesheet : ?rewrite:(string * string) list -> \
     ?dont_hash:string list -> dont_hash_prefixes:string list -> string -> unit]"
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

let validate_args ~loc args =
  let open Option.Let_syntax in
  raise_if_alternate_syntaxes ~loc args;
  let args =
    let%map css_string, remaining_args =
      List.take_map args ~f:(fun (_, (arg : expression)) ->
        match arg.pexp_desc with
        | Pexp_constant (Pconst_string (l, _, _)) -> Some l
        | _ -> None)
    in
    let rewrite, remaining_args =
      List.take_map remaining_args ~f:(function
        | Labelled "rewrite", expression -> Some (parse_rewrite ~loc expression)
        | _ -> None)
      |> Option.value ~default:(String.Map.empty, remaining_args)
    in
    let dont_hash, remaining_args =
      List.take_map remaining_args ~f:(function
        | Labelled ("dont_hash" | "don't_hash"), expression ->
          Some
            (parse_string_list
               ~name:"dont_hash"
               ~syntax_error_message:malformed_dont_hash_error_message
               ~loc:expression.pexp_loc
               expression)
        | _ -> None)
      |> Option.value ~default:([], remaining_args)
    in
    let dont_hash_prefixes, remaining_args =
      List.take_map remaining_args ~f:(function
        | Labelled ("dont_hash_prefixes" | "don't_hash_prefixes"), expression ->
          Some
            (parse_string_list
               ~name:"dont_hash_prefixes"
               ~syntax_error_message:malformed_dont_hash_prefixes_error_message
               ~loc:expression.pexp_loc
               expression)
        | _ -> None)
      |> Option.value ~default:([], remaining_args)
    in
    let rewrite =
      List.fold dont_hash ~init:rewrite ~f:(fun acc dont_hash_this ->
        Map.update acc dont_hash_this ~f:(function
          | None ->
            let open (val Ast_builder.make loc) in
            pexp_constant (Pconst_string (dont_hash_this, loc, None))
          | Some _ ->
            Location.raise_errorf
              ~loc
              {|Found duplicate value \"%s\" between [dont_hash] and [rewrite].|}
              dont_hash_this))
    in
    css_string, rewrite, remaining_args, dont_hash_prefixes
  in
  match args with
  | None | Some (_, _, _ :: _, _) ->
    raise_misparse_with_syntax_instructions
      ~extra_message:"ppx_css found unexpected arguments. "
      ~loc
  | Some (css_string, rewrite, [], dont_hash_prefixes) ->
    css_string, rewrite, dont_hash_prefixes
;;

let parse (expression : expression) =
  let loc = expression.pexp_loc in
  let loc = { loc with loc_ghost = true } in
  match expression.pexp_desc with
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "stylesheet"; loc }; _ }, args) ->
    let css_string, rewrite, dont_hash_prefixes = validate_args ~loc args in
    { css_string
    ; rewrite
    ; stylesheet_location = { loc with loc_ghost = true }
    ; dont_hash_prefixes
    }
  | _ -> raise_misparse_with_syntax_instructions ~extra_message:"" ~loc
;;
