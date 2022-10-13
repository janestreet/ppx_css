open! Core
open! Ppxlib

type t = { dont_hash : String.Set.t }

let empty = { dont_hash = String.Set.empty }

let exprs_to_class_names ~loc (expr : expression) =
  let rec helper ~acc ~expr =
    match expr.pexp_desc with
    | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> acc
    | Pexp_construct
        ( { txt = Lident "::"; _ }
        , Some
            { pexp_desc =
                Pexp_tuple
                  [ { pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _ }; child ]
            ; _
            } ) -> helper ~acc:(Set.add acc s) ~expr:child
    | _ ->
      Location.raise_errorf
        ~loc
        {|[dont_hash] expects a string list, but found something else|}
  in
  helper ~acc:String.Set.empty ~expr
;;

let validate_args ~loc (args : (arg_label * expression) list) =
  match args with
  | [] -> empty
  | [ (Labelled "dont_hash", expr) ] ->
    let dont_hash = exprs_to_class_names ~loc expr in
    { dont_hash }
  | _ ->
    Location.raise_errorf
      ~loc
      {|ppx_css only supports a css_string or a css_string with a single named "dont_hash" argument.|}
;;

let parse (expression : expression) ~loc =
  match expression.pexp_desc with
  | Pexp_constant (Pconst_string (l, _, _)) -> l, empty
  | Pexp_apply ({ pexp_desc = Pexp_constant (Pconst_string (l, _, _)); _ }, args) ->
    l, validate_args ~loc args
  | _ ->
    Location.raise_errorf
      ~loc
      {|%%css must take a single string as input with an optional parameter "dont_hash"|}
;;
