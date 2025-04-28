open! Core
include Int

let identify_stylesheet (rules : Css_parser.Rule.t list) =
  List.foldi rules ~init:Map.empty ~f:(fun id acc rule ->
    Core.Map.set acc ~key:id ~data:rule)
;;

let of_int_potentially_unsafe = Fn.id
