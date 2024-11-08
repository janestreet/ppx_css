open! Core
open Js_of_ocaml

let reinitialize () =
  let global = Js.Unsafe.global in
  Js.Unsafe.set
    global
    "document"
    (Js.Unsafe.js_expr
       {js|
           (function () {
             if (globalThis?.document) {
               if (!globalThis.document?.adoptedStyleSheets) {
                 globalThis.document.adoptedStyleSheets = [];
               }
               return globalThis?.document;
             }
             return {
               adoptedStyleSheets: []
             };
           })()
        |js});
  Js.Unsafe.set
    global
    "CSSStyleSheet"
    (Js.Unsafe.js_expr
       {js|
           (globalThis?.CSSStyleSheet || (class CSSStyleSheet {
             constructor() {
               this.text = "";
             }
            
             replaceSync(css_string) {
               this.text = css_string;
             }
           }))
        |js});
  Inline_css.For_testing.reinitialize_stylesheet ()
;;

let to_string () =
  Js_of_ocaml.Js.Unsafe.js_expr
    {js|
     (function () {
       return [...document.adoptedStyleSheets].map((sheet) => sheet.text).join("\n");
     }()
     )
  |js}
  |> Js_of_ocaml.Js.to_string
;;

let () = reinitialize ()
