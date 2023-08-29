open! Core
open Ppxlib

type 'a t =
  { txt : 'a
  ; temporary_name : string
  }

let mint txt =
  let temporary_name = gen_symbol ~prefix:"ppx_css_temp_variable" () in
  { txt; temporary_name }
;;
