open! Bonsai_web

let () = Inline_css.Private.append "/* direct from lib */"
let app = "hi there"

module Style =
  [%css
    stylesheet {|
   .foo #foo #bar {
       background: 5;
       red: blue;
   }
|}]
