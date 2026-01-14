open! Core
open Js_of_ocaml

(* We have to establish a fake global/window so that we can run this example as it doesn't
   run in the browser *)
let mock_constructed_stylesheet =
  lazy
    (let global = Js.Unsafe.global in
     Js.Unsafe.set
       global
       "requestAnimationFrame"
       (Js.wrap_callback (fun f -> Js.Unsafe.fun_call f [| Js.Unsafe.inject () |]));
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
     Inline_css.For_testing.reinitialize_stylesheet ())
;;

let () = force mock_constructed_stylesheet

let get_id =
  lazy
    (Sys.opaque_identity (fun () ->
       force mock_constructed_stylesheet;
       ()))
;;
