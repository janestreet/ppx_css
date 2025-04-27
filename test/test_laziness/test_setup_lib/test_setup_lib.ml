include Core
include Bonsai_web_proc
include Bonsai_web_test
module Test_fn = Instantiate_dummy_functions

let vdom_handle vdom = Handle.create (Result_spec.vdom Fn.id) (Bonsai.const vdom)

let css_attr_handle css_attr =
  vdom_handle (Vdom.Node.div ~attrs:[ css_attr ] [ Vdom.Node.text "hi" ])
;;

let prev_value = ref ""

let get_value () =
  let value = Test_fn.to_string () in
  prev_value := value;
  value
;;

let print_css () = get_value () |> print_endline

(* This uses the previous value reference to compare the currently available css to the 
   value that was available last time we printed the CSS or called this function. Useful
   so that we don't print a gigantic stylesheet every time we need to check what's changed
   in the CSS
*)
let compare_against_prev () =
  let prev_value = !prev_value in
  let value = get_value () in
  match String.equal prev_value value with
  | true -> print_endline "No change in css"
  | false -> Expect_test_patdiff.print_patdiff prev_value value
;;

let clear_before_test () = prev_value := ""
