open! Core
open Js_of_ocaml

module State = struct
  type t =
    { memo : String.Hash_set.t
    ; mutable ordered : string Reversed_list.t
    }

  let all_css = { memo = String.Hash_set.create (); ordered = Reversed_list.[] }
  let to_string () = all_css.ordered |> Reversed_list.rev |> String.concat ~sep:"\n"

  let append a =
    let already_appended = Hash_set.mem all_css.memo a in
    if already_appended
    then `Already_appended
    else (
      Hash_set.add all_css.memo a;
      all_css.ordered <- Reversed_list.(a :: all_css.ordered);
      `Newly_appended)
  ;;

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
    let maybe_initialize : (module Strategy_intf.S) -> Strategy_intf.t Or_error.t =
      fun (module Strategy) ->
      let%map.Or_error state = Or_error.tag (Strategy.initialize ()) ~tag:Strategy.name in
      Strategy_intf.T { state; strategy = (module Strategy) }
    in
    match am_running_test with
    | true -> maybe_initialize (module Testing_strategy)
    | false -> Or_error.find_map_ok strategies ~f:maybe_initialize
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
  let already_appended = State.append a in
  match already_appended with
  | `Already_appended -> ()
  | `Newly_appended ->
    (match Lazy.peek Strategy.selected with
     | None ->
       (* If the lazy isn't forced, then we don't need to do anything because all the
          contents will be loaded when [install_in_dom] is called. *)
       ()
     | Some (Ok (T { state; strategy = (module M) })) ->
       M.update state (State.to_string ())
     | Some (Error _) ->
       (* don't print the error because it already got printed inside [install_in_dom] *)
       ())
;;

module For_testing = struct
  let print = State.print_for_testing

  let with_strategy ~f =
    match Lazy.force Strategy.selected with
    | Error error ->
      let error = Error.to_string_hum error in
      failwith [%string "no strategy selected. error: %{error}"]
    | Ok strategy -> f strategy
  ;;

  let strategy_name () = with_strategy ~f:(fun (T { strategy = (module M); _ }) -> M.name)

  let dump_strategy_state () =
    with_strategy ~f:(fun (T { strategy = (module M); state }) ->
      print_endline (M.For_testing.dump_testing_state state))
  ;;
end

module Private = struct
  let append = append
end
