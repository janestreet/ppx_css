open! Core
open Virtual_dom

module Input = struct
  type t = string [@@deriving sexp_of, equal]

  let combine = ( ^ )
end

module State = struct
  type t = Stylesheet.t Or_error.t

  let iter (t : t) ~f =
    match t with
    | Error error ->
      print_s
        [%message
          "Error while attempting to create CSSStyleSheet. Styles may not load."
            (error : Error.t)]
    | Ok stylesheet -> f stylesheet
  ;;
end

module S = struct
  module Input = Input
  module State = State

  let init string _element =
    let state = Stylesheet.create_stylesheet () in
    State.iter state ~f:(fun stylesheet ->
      Stylesheet.append_stylesheet stylesheet;
      Stylesheet.update_stylesheet stylesheet string);
    state
  ;;

  let on_mount = `Do_nothing

  let update ~old_input ~new_input state _element =
    if phys_equal old_input new_input || Input.equal old_input new_input
    then ( (* do nothing if the inputs don't change *) )
    else
      State.iter state ~f:(fun stylesheet ->
        Stylesheet.update_stylesheet stylesheet new_input)
  ;;

  let destroy _input state _element =
    State.iter state ~f:(fun stylesheet -> Stylesheet.delete_stylesheet stylesheet)
  ;;
end

module Hook = Vdom.Attr.Hooks.Make (S)

let attr s = Vdom.Attr.create_hook "inline-css-dynamic" (Hook.create s)
