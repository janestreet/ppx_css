open! Js_of_ocaml
open! Core

let () = (Lazy.force Css_js_mocker.get_id) ()
let app = "hi there"
let () = Inline_css.Private.append "/* direct from lib */"

module Style =
  [%css
  stylesheet
    {|
      .foo #foo #bar {
        background: 5;
        red: blue;

        .hashed {
        }
      }
      |}
    ~dont_hash:[ "foo"; "bar" ]]

let () = (ignore : Style.t -> unit) (module Style)
