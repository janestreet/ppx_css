open! Core

type t = { handle_recoverable_error : Recoverable_error.Handler.t }

val raise_on_recoverable_errors : t
val ignore_recoverable_errors : t
