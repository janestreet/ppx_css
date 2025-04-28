open! Core
open Ppxlib

(** This module represens an "anonymous variable" that encode the interpolation
    replacements that [%css] performs.

    for instance, the declarations:

    {[
      {|background-color: %{color};
        color: %{background_color}
      |}
    ]}

    will expand to something like:

    {[
      {|background-color: var(--anon-variable1);
        color: var(--anon-variable2);
      |}
    ]}

    this module contains functions that abstract away anonymous variables. *)
module Name : sig
  type t

  include Identifiable.S with type t := t

  val to_css_variable : t -> string
end

type t

val of_expression : expression -> t

(** The made up name that is minted for the variable. *)
val name : t -> Name.t

(** The expression that the variable should be interpolated into. *)
val expression : t -> expression

module Collection : sig
  type variable := t

  type nonrec t = private
    { variables : t list
    ; unique_name : string
    }

  val of_list : variable list -> t
  val empty : t
end

module For_testing : sig
  val restart_identifiers : unit -> unit
end
