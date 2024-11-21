open! Core
open Ppxlib
open Css_jane

type t = Rule.t Rule_id.Map.t * location

val of_stylesheet : Stylesheet.t -> t
