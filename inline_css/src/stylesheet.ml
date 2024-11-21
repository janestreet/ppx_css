open! Core
open Js_of_ocaml

module Style_sheet = struct
  class type t = object
    method replaceSync : Js.js_string Js.t -> unit Js.meth
  end

  let t : t Js.t Js.constr Js.Optdef.t ref = ref Js.Unsafe.global##._CSSStyleSheet

  let append : t Js.t -> unit =
    let f =
      Js.Unsafe.pure_js_expr
        {js|
  (function (style_sheet) {
    globalThis.document.adoptedStyleSheets.push(style_sheet);
  })
|js}
    in
    fun style_sheet -> Js.Unsafe.fun_call f [| Js.Unsafe.inject style_sheet |]
  ;;

  let reinitialize () = t := Js.Unsafe.global##._CSSStyleSheet

  let prepend : t Js.t -> unit =
    let f =
      Js.Unsafe.pure_js_expr
        {js|
  (function (style_sheet) {
    globalThis.document.adoptedStyleSheets.unshift(style_sheet);
  })
|js}
    in
    fun style_sheet -> Js.Unsafe.fun_call f [| Js.Unsafe.inject style_sheet |]
  ;;

  let create () =
    match Js.Optdef.to_option !t with
    | Some stylesheet_constructor ->
      (try
         let style_sheet = new%js stylesheet_constructor in
         Ok style_sheet
       with
       | exn -> Or_error.of_exn exn)
    | None -> Or_error.error_string "window.CSSStyleSheet doesn't exist"
  ;;

  let update (t : t Js.t) (css_string : string) : unit =
    t##replaceSync (Js.string css_string)
  ;;

  let delete : t Js.t -> unit =
    let f =
      Js.Unsafe.pure_js_expr
        {js|
  (function (style_sheet) {
    globalThis.document.adoptedStyleSheets = document.adoptedStyleSheets.filter(x => (x !== style_sheet))
  })
|js}
    in
    fun style_sheet -> Js.Unsafe.fun_call f [| Js.Unsafe.inject style_sheet |]
  ;;
end

type t = Style_sheet.t Js.t

let delete_stylesheet = Style_sheet.delete
let update_stylesheet = Style_sheet.update
let create_stylesheet = Style_sheet.create
let append_stylesheet = Style_sheet.append
let prepend_stylesheet = Style_sheet.prepend

module For_testing = struct
  let reinitialize = Style_sheet.reinitialize
end
