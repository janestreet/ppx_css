open! Core

let print_or_fail_silently err =
  match Am_running_how_js.am_in_browser with
  | true -> eprint_s [%message (err : Error.t)]
  | false -> ()
;;

type direction =
  | Append
  | Prepend

let append_or_prepend ~direction sheet =
  match sheet with
  | Ok sheet ->
    (match direction with
     | Append -> Stylesheet.append_stylesheet sheet
     | Prepend -> Stylesheet.prepend_stylesheet sheet)
  | Error err -> print_or_fail_silently err
;;

let append a = append_or_prepend ~direction:Append a
let prepend a = append_or_prepend ~direction:Prepend a

let create_and_dir ~direction css_str =
  match Stylesheet.create_stylesheet () with
  | Ok sheet ->
    Stylesheet.update_stylesheet sheet css_str;
    append_or_prepend ~direction (Ok sheet)
  | Error error -> print_or_fail_silently error
;;

let create_and_append css_str = create_and_dir ~direction:Append css_str
let create_and_prepend css_str = create_and_dir ~direction:Prepend css_str

let update_stylesheet sheet css_string =
  match sheet with
  | Ok sheet -> Stylesheet.update_stylesheet sheet css_string
  | Error error -> print_or_fail_silently error
;;

module For_testing = struct
  let reinitialize_stylesheet = Stylesheet.For_testing.reinitialize
end

module Ppx_css_runtime = struct
  let force lazy_ = Core.Lazy.force lazy_
end

module Private = struct
  let create_stylesheet = Stylesheet.create_stylesheet
  let append_stylesheet = append
  let prepend_stylesheet = prepend
  let append = create_and_append
  let prepend = create_and_prepend
  let update_stylesheet = update_stylesheet

  module Dynamic = Inline_css_dynamic
end
