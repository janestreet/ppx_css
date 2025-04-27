open! Core
open Ppxlib
open Css_parser

type t = Rule.t Rule_id.Map.t * location

val of_stylesheet : Stylesheet.t -> t
