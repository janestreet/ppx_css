open! Core
open! Ppxlib

module For_testing : sig
  val generate_struct : expression -> module_expr
  val generate_sig : core_type -> module_type
end
