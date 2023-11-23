open! Core
open Ppxlib

type t =
  { css_string : string
  ; string_loc : location
  ; delimiter : string option
  }
