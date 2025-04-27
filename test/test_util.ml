open! Core
open Ppxlib
open Ppx_css

let loc_with_mock_name =
  let loc = Location.none in
  let pos_fname = "app/foo/foo.ml" in
  let loc_start = { loc.loc_start with pos_fname; pos_cnum = 1; pos_lnum = 1 }
  and loc_end = { loc.loc_end with pos_fname; pos_cnum = 1; pos_lnum = 1 } in
  { loc with loc_start; loc_end }
;;

let catch_location_error ~f =
  try f () with
  | ex ->
    (match Location.Error.of_exn ex with
     | Some error -> print_endline (Location.Error.message error)
     | None -> raise ex)
;;

let test_struct expr =
  catch_location_error ~f:(fun () ->
    let%tydi { txt = structure; hoisted_structure_items; _ } =
      For_testing.generate_struct ~loc:loc_with_mock_name ~disable_hashing:false expr
    in
    print_endline (Pprintast.string_of_structure structure);
    print_endline "\nHoisted module:\n";
    print_endline (Pprintast.string_of_structure hoisted_structure_items))
;;

let test_sig s =
  catch_location_error ~f:(fun () ->
    let transformed =
      Ppx_css.For_css_inliner.gen_sig ~stylesheet_location:loc_with_mock_name s
    in
    print_endline transformed)
;;

let print_heading s =
  let bar = String.init (String.length s) ~f:(Fn.const '-') in
  print_endline s;
  print_endline bar
;;

let test_expression expr =
  catch_location_error ~f:(fun () ->
    let%tydi { txt = expression; hoisted_structure_items; _ } =
      expr
      |> For_testing.generate_inline_expression
           ~loc:loc_with_mock_name
           ~disable_hashing:false
    in
    print_heading "Expression context:";
    expression |> Pprintast.string_of_expression |> print_endline;
    print_heading "Hoisted context:";
    print_endline (Pprintast.string_of_structure hoisted_structure_items))
;;
