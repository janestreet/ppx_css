open! Core

type 'a t = private
  { txt : 'a
  ; temporary_name : string
  }

val mint : 'a -> 'a t
