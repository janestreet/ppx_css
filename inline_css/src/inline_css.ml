open! Core
open Js_of_ocaml

module Prepend_and_append_list : sig
  (** A list builder that allows inserting both at the front and end of the list *)

  type 'a t

  val empty : 'a t
  val append : 'a t -> 'a -> 'a t
  val prepend : 'a t -> 'a -> 'a t

  (* Converts the lists into a list. It returns a new [t] which can be used to ensure
     future [to_list] calls after new things are appended are more efficient while
     returning the same result. *)

  val to_list : 'a t -> 'a list * 'a t
end = struct
  type 'a t =
    { prepended : 'a list
    ; appended : 'a Reversed_list.t
    }

  let empty = { prepended = []; appended = [] }
  let append t item = { t with appended = item :: t.appended }
  let prepend t item = { t with prepended = item :: t.prepended }

  let to_list t =
    if Reversed_list.is_empty t.appended
    then t.prepended, t
    else (
      let joined = t.prepended @ Reversed_list.rev t.appended in
      joined, { prepended = joined; appended = [] })
  ;;
end

module State = struct
  type t =
    { mutable items : string Prepend_and_append_list.t
    ; mutable updated : bool
    ; mutable future_appends_should_always_update : bool
    }

  let all_css =
    { items = Prepend_and_append_list.empty
    ; updated = false
    ; future_appends_should_always_update = false
    }
  ;;

  let to_string () =
    let joined, items' = Prepend_and_append_list.to_list all_css.items in
    all_css.items <- items';
    String.concat joined ~sep:"\n"
  ;;

  let append a =
    all_css.updated <- false;
    all_css.items <- Prepend_and_append_list.append all_css.items a
  ;;

  let prepend a =
    all_css.items <- Prepend_and_append_list.prepend all_css.items a;
    all_css.updated <- false
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

let update_strategy_from_state () =
  match Lazy.peek Strategy.selected with
  | None ->
    (* If the lazy isn't forced, then we don't need to do anything because all the
       contents will be loaded when [install_in_dom] is called. *)
    ()
  | Some (Error _) ->
    (* don't print the error because it already got printed inside [install_in_dom] *)
    ()
  | Some (Ok (T { state; strategy = (module M) })) ->
    (match State.all_css.updated with
     | true -> ()
     | false ->
       M.update state (State.to_string ());
       State.all_css.updated <- true)
;;

let append a =
  State.append a;
  update_strategy_from_state ()
;;

let update_if_not_already_updated () =
  update_strategy_from_state ();
  State.all_css.future_appends_should_always_update <- true
;;

let is_in_browser =
  Js.Optdef.test
    (Js.Unsafe.pure_js_expr {|globalThis?.window?.requestAnimationFrame|} : _ Js.Optdef.t)
;;

let animation_frame_enqueued = ref false

let schedule_an_update_on_animation_frame () =
  (* NOTE: Calling [requestAnimationFrame] is expensive even if the callback it is given
     is a no-op. Gating this behind a global bool ref makes it so that we do not ever have
     two of these [requestAnimationFrame]s in progress. *)
  if is_in_browser && not !animation_frame_enqueued
  then (
    let animation_frame_id =
      animation_frame_enqueued := true;
      Dom_html.window##requestAnimationFrame
        (Js.wrap_callback (fun _ ->
           animation_frame_enqueued := false;
           update_if_not_already_updated ()))
    in
    ignore animation_frame_id)
;;

let append_but_do_not_update a =
  State.append a;
  if State.all_css.future_appends_should_always_update
  then update_strategy_from_state ()
  else schedule_an_update_on_animation_frame ()
;;

let prepend a =
  State.prepend a;
  update_strategy_from_state ()
;;

let prepend_but_do_not_update a =
  State.prepend a;
  if State.all_css.future_appends_should_always_update
  then update_strategy_from_state ()
  else schedule_an_update_on_animation_frame ()
;;

module For_testing = struct
  let to_string () = State.to_string ()

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
  let prepend = prepend
  let append_but_do_not_update = append_but_do_not_update
  let prepend_but_do_not_update = prepend_but_do_not_update
  let update_if_not_already_updated = update_if_not_already_updated

  module Dynamic = Inline_css_dynamic
end
