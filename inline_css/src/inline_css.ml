open! Core
open Js_of_ocaml

module Style_sheet = struct
  class type t =
    object
      method replaceSync : Js.js_string Js.t -> unit Js.meth
    end

  let t : t Js.t Js.constr = Js.Unsafe.global##._CSSStyleSheet

  let append : t Js.t -> unit =
    Js.Unsafe.pure_js_expr
      {js|
  (function (style_sheet) {
    // push doesn't work because this field is really weird.
    document.adoptedStyleSheets =
      Array.prototype.concat.apply(document.adoptedStyleSheets, [style_sheet]);
  })
|js}
  ;;
end

let all_css = ref Reversed_list.[]
let global_style_sheet = ref None
let to_string () = !all_css |> Reversed_list.rev |> String.concat ~sep:"\n"

let print_for_testing =
  let regex = Re.Str.regexp "_hash_\\([a-z0-9]+\\)*" in
  fun () ->
    to_string () |> Re.Str.global_replace regex "_hash_replaced_in_test" |> print_endline
;;

let install_in_dom () =
  let style_sheet = new%js Style_sheet.t in
  Style_sheet.append style_sheet;
  style_sheet##replaceSync (Js.string (to_string ()));
  global_style_sheet := Some style_sheet
;;

(* If the ref-cell is None, then we don't need to do anything
   because all the contents will be loaded when [install_in_dom]
   is called. *)
let maybe_update_in_dom () =
  match !global_style_sheet with
  | Some style_sheet -> style_sheet##replaceSync (Js.string (to_string ()))
  | None -> ()
;;

let append a =
  (all_css := Reversed_list.(a :: !all_css));
  maybe_update_in_dom ()
;;

let () =
  let ready_state =
    Option.try_with (fun () -> Dom_html.document##.readyState |> Js.to_string)
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

module Private = struct
  let append = append
end
