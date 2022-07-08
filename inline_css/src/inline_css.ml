open! Core
open Js_of_ocaml

module State = struct
  let all_css = ref Reversed_list.[]
  let to_string () = !all_css |> Reversed_list.rev |> String.concat ~sep:"\n"
  let append a = all_css := Reversed_list.(a :: !all_css)

  let print_for_testing =
    let regex = Re.Str.regexp "_hash_\\([a-z0-9]+\\)*" in
    fun () ->
      to_string ()
      |> Re.Str.global_replace regex "_hash_replaced_in_test"
      |> print_endline
  ;;
end

module Strategy = struct
  let strategies : (module Strategy_intf.S) list =
    [ (module Constructed_stylesheet_strategy)
    ; (module Style_element_strategy.Into_head)
    ; (module Style_element_strategy.Into_body)
    ; (module Style_element_strategy.Into_root_element)
    ]
  ;;

  let find () =
    Or_error.find_map_ok strategies ~f:(fun (module Strategy) ->
      let%map.Or_error state =
        Or_error.tag (Strategy.initialize ()) ~tag:Strategy.name
      in
      Strategy_intf.T { state; strategy = (module Strategy) })
  ;;

  let selected : Strategy_intf.t Or_error.t Lazy.t = lazy (find ())
end

let install_in_dom () =
  match Lazy.force Strategy.selected with
  | Ok (T { state; strategy = (module M) }) -> M.update state (State.to_string ())
  | Error e -> eprint_s (Error.sexp_of_t e)
;;

let () =
  let ready_state =
    Option.try_with (fun () -> Js.to_string Dom_html.document##.readyState)
  in
  match ready_state with
  | Some ("interactive" | "complete") -> install_in_dom ()
  | Some ("loading" | _) ->
    let _id : Dom_html.event_listener_id =
      Dom_html.addEventListenerWithOptions
        ~passive:Js._true
        ~capture:Js._false
        ~once:Js._true
        Dom_html.window
        Dom_html.Event.domContentLoaded
        (Dom.handler (fun _ ->
           install_in_dom ();
           Js._true))
    in
    ()
  | None -> ()
;;

let append a =
  State.append a;
  match Lazy.peek Strategy.selected with
  | None ->
    (* If the lazy isn't forced, then we don't need to do anything because all the
       contents will be loaded when [install_in_dom] is called. *)
    ()
  | Some (Ok (T { state; strategy = (module M) })) -> M.update state (State.to_string ())
  | Some (Error _) ->
    (* don't print the error because it already got printed inside [install_in_dom] *) ()
;;

let print_for_testing = State.print_for_testing

module Private = struct
  let append = append
end
