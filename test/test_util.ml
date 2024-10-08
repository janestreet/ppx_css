open! Core
open Ppxlib
open Ppx_css

let loc_with_mock_name =
  let loc = Location.none in
  let pos_fname = "app/foo/foo.ml" in
  let loc_start = { loc.loc_start with pos_fname }
  and loc_end = { loc.loc_end with pos_fname } in
  { loc with loc_start; loc_end }
;;

let loc = Location.none

let catch_location_error ~f =
  try f () with
  | ex ->
    (match Location.Error.of_exn ex with
     | Some error -> print_endline (Location.Error.message error)
     | None -> raise ex)
;;

let test_struct expr =
  catch_location_error ~f:(fun () ->
    let%tydi { txt = structure; ppx_css_string_expression; css_string_for_testing = _ } =
      For_testing.generate_struct expr
    in
    let structure =
      structure
      @ [ For_testing.ppx_css_expression_to_structure_item
            ~loc:Location.none
            ppx_css_string_expression
        ]
    in
    print_endline (Pprintast.string_of_structure structure))
;;

let test_sig s =
  catch_location_error ~f:(fun () ->
    let transformed = Ppx_css.For_css_inliner.gen_sig s in
    print_endline transformed)
;;

let print_heading s =
  let bar = String.init (String.length s) ~f:(Fn.const '-') in
  print_endline s;
  print_endline bar
;;

let test_expression expr =
  catch_location_error ~f:(fun () ->
    let%tydi { txt = expression; ppx_css_string_expression; css_string_for_testing = _ } =
      expr |> For_testing.generate_inline_expression
    in
    print_heading "Expression context:";
    expression |> Pprintast.string_of_expression |> print_endline;
    print_heading "Hoisted context:";
    print_endline
      (Pprintast.string_of_structure
         [ For_testing.ppx_css_expression_to_structure_item ~loc ppx_css_string_expression
         ]))
;;
