open! Core

(* For context, we have "sanity check" in our auto-formatting integration that
   asserts that we do not modify the AST upon running the auto-formatter.

   The first version of the check was roughly something like:

   [equal before_formatting after_formatting]

   This proves tricky, because sometimes we do want to edit the AST in non-meaningful ways
   e.g. we would like to treat colors like "#FFFFFF" and "#ffffff" as "equivalent". 

   as such, we "normalize" the AST to some common ground, so that our AST equivalence check is:

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

let mapper =
  object
    inherit Traverse.map as super

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
