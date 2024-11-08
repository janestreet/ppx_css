open! Core
open Ppxlib
open Css_jane

type t = Rule.t Rule_id.Map.t * location

let of_stylesheet : Stylesheet.t -> t = Tuple2.map_fst ~f:Rule_id.identify_stylesheet
