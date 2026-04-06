open! Core

(* For context, we have "sanity check" in our auto-formatting integration that asserts
   that we do not modify the AST upon running the auto-formatter.

   The first version of the check was roughly something like:

   [equal before_formatting after_formatting]

   This proves tricky, because sometimes we do want to edit the AST in non-meaningful ways
   e.g. we would like to treat colors like "#FFFFFF" and "#ffffff" as "equivalent".

   as such, we "normalize" the AST to some common ground, so that our AST equivalence
   check is:

   [equal (normalize before_formatting) (normalize after_formatting)]

   this module implements [normalize]

   CSS is by default case-insensitive, but there are several notable exceptions. In order
   to ensure that we are not accidentally mutating the CSS in significant ways, we have
   opted to treat CSS as if it is case-sensitive with exceptions (which are outlined in
   this normalization function).
*)

let lowercase_hex_colors =
  object
    inherit Traverse.map as super

    method! component_value component_value =
      match component_value with
      | Hash hash ->
        (* We "normalize" capital hex colors to their lowercase version. *)
        Hash (Tuple2.map_fst ~f:String.lowercase hash)
      | _ -> super#component_value component_value
  end
;;

let normalize_attribute_values =
  (* According to the CSS Selectors Level 4 spec, the following attribute values can
     either be a string or an identifier:

     - [attr=val]
     - [attr~=val]
     - [attr|=val]
     - [attr^=val]
     - [attr$=val]
     - [attr*=val]

     We will normalize all of these to be strings prior to passing the values to
     apply-style in order to comply with how [biome] formats things + our internal AST
     equivalence check

     https://www.w3.org/TR/selectors-4/#attribute-representation
     https://www.w3.org/TR/selectors-4/#attribute-substrings
  *)
  let open Types.Component_value in
  let rec normalize (cvs : Types.Component_value.t Types.with_loc list) =
    match cvs with
    | [] -> []
    | ((Ident _, _) as attr_cv)
      :: ((Delim ("~" | "|" | "^" | "$" | "*"), _) as match_token)
      :: ((Delim "=", _) as equal_token)
      :: (Ident (attr_value_name, _attr_value_loc), attr_value_cv_loc)
      :: rest ->
      (* All of the cases that have a leading match token *)
      attr_cv
      :: match_token
      :: equal_token
      :: (String { value = attr_value_name; quote_type = Double }, attr_value_cv_loc)
      :: normalize rest
    | ((Ident _, _) as attr_cv)
      :: ((Delim "=", _) as equal_token)
      :: (Ident (attr_value_name, _attr_value_loc), attr_value_cv_loc)
      :: rest ->
      (* [attr=val] *)
      attr_cv
      :: equal_token
      :: (String { value = attr_value_name; quote_type = Double }, attr_value_cv_loc)
      :: normalize rest
    | cv :: rest -> cv :: normalize rest
  in
  fun (component_values : Types.Component_value.t Types.with_loc list) ->
    normalize component_values
;;

let mapper =
  object
    inherit Traverse.map as super

    method! string_token_quote_type _quote_type =
      (* We "normalize" single quotes to double quotes so that they are treated as
         equivalent. *)
      Double

    method! selector selector =
      let open Types.Selector in
      match selector with
      | Attribute (component_values, cv_loc) ->
        (* Normalize idents to strings in attribute selector values *)
        let normalized_values = normalize_attribute_values component_values in
        let normalized_attributes = Attribute (normalized_values, cv_loc) in
        super#selector normalized_attributes
      | _ -> super#selector selector

    method! declaration declaration =
      let value =
        Tuple2.map_fst
          declaration.value
          ~f:(List.map ~f:(Tuple2.map_fst ~f:lowercase_hex_colors#component_value))
      in
      super#declaration { declaration with value }
  end
;;

let normalize_stylesheet : Types.Stylesheet.t -> Types.Stylesheet.t =
  fun stylesheet -> mapper#stylesheet stylesheet
;;

let normalize_style_block : Types.Style_block.t -> Types.Style_block.t =
  fun style_block -> mapper#style_block style_block
;;
