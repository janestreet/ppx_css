open! Core
open Js_of_ocaml

module Style_sheet = struct
  class type t = object
    method replaceSync : Js.js_string Js.t -> unit Js.meth
  end

  let t : t Js.t Js.constr Js.Optdef.t = Js.Unsafe.global##._CSSStyleSheet

  let append : t Js.t -> unit =
    let f =
      Js.Unsafe.pure_js_expr
        {js|
  (function (style_sheet) {
    // push doesn't work because this field is really weird.
    document.adoptedStyleSheets =
      Array.prototype.concat.apply(document.adoptedStyleSheets, [style_sheet]);
  })
|js}
    in
    fun style_sheet -> Js.Unsafe.fun_call f [| Js.Unsafe.inject style_sheet |]
  ;;

  let delete : t Js.t -> unit =
    let f =
      Js.Unsafe.pure_js_expr
        {js|
  (function (style_sheet) {
    document.adoptedStyleSheets = document.adoptedStyleSheets.filter(x => !(x === style_sheet))
  })
|js}
    in
    fun style_sheet -> Js.Unsafe.fun_call f [| Js.Unsafe.inject style_sheet |]
  ;;
end

type t = Style_sheet.t Js.t

let delete_stylesheet = Style_sheet.delete
let name = "constructed stylesheet"

let initialize () =
  match Js.Optdef.to_option Style_sheet.t with
  | Some stylesheet_constructor ->
    (try
       let style_sheet = new%js stylesheet_constructor in
       Style_sheet.append style_sheet;
       Ok style_sheet
     with
     | exn -> Or_error.of_exn exn)
  | None -> Or_error.error_string "window.CSSStyleSheet doesn't exist"
;;

let update t content = t##replaceSync (Js.string content)

module For_testing = struct
  let dump_testing_state _ = "constructed-stylesheet-strategy has an opaque state"
end
