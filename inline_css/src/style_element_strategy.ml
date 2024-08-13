open! Core
open Js_of_ocaml

module Make (T : sig
    val name : string
    val parent_selector : string
  end) =
struct
  include T

  type t = Dom_html.styleElement Js.t

  let make_style_element () = Dom_html.createStyle Dom_html.document
  let update t content = t##.innerText := Js.string content

  let initialize () =
    let parent = Dom_html.document##querySelector (Js.string parent_selector) in
    match Js.Opt.to_option parent with
    | None -> Or_error.error_string (parent_selector ^ " not found in document")
    | Some parent ->
      (try
         let style_element = make_style_element () in
         ignore (parent##appendChild (style_element :> Dom.node Js.t) : Dom.node Js.t);
         Ok style_element
       with
       | exn -> Or_error.of_exn exn)
  ;;

  module For_testing = struct
    let dump_testing_state _ = [%string "%{T.name} has an opauque testing state"]
  end
end

module Into_head = Make (struct
    let name = "<style> element inside <head> element"
    let parent_selector = "head"
  end)

module Into_body = Make (struct
    let name = "<style> element inside <body> element"
    let parent_selector = "body"
  end)

module Into_root_element = Make (struct
    let name = "any element inside the document"
    let parent_selector = "*"
  end)
