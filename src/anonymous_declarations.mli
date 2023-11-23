open! Core
open! Ppxlib

(** "anonymous declarations" is a made up term by css that defines a css declarations
    that live without a rule.

    For instance, the string:

    {[
      {|
      background-color: tomato;
      transition: 1s;
      |}
    ]}

    has two anonymous declarations.

    This module contains functions to parse and manipulate anonymous_declarations.
*)

type t

(** Take in a string of anonymous declarations. For example:

    {[ create ~string_loc:loc {|background-color: tomato|} ]} *)
val create : Ppx_css_syntax.String_constant.t -> t

(** Gives an anonymous declarations an "anonymous classname" that's just something like:

    {|
    .anonymous_classname { %{original_declarations} }
    |}
*)
val to_stylesheet_string : t -> string

(** The "anonymous classname" for the declarations. *)
val anonymous_class_name : string

(** [anonymous_variables] encode the interpolation replacements that [%css] performs.

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

    and this function returns the anonymous variables within the declarations. *)
val anonymous_variables : t -> Anonymous_variable.Collection.t

(** We do not want to hash "user-provided variables" for instance:

    {[
      {|
        background-color: var(--foo);
      |}
    ]}

    so this function tells us which css identifiers we should not hash.
*)
val inferred_do_not_hash : t -> string list

(**
   We _always_ want to hash the anonymous classname and the anonymous variables, so
   this function tells us the css identifiers to _always_ hash, regardless of user-provided
   dont_hash/rewrite/dont_hash_prefixes. *)
val always_hash : t -> String.Set.t

module For_stylesheet : sig
  type t

  val create : Ppx_css_syntax.String_constant.t -> t
  val anonymous_variables : t -> Anonymous_variable.Collection.t
  val to_stylesheet_string : t -> string
  val always_hash : t -> String.Set.t
end
