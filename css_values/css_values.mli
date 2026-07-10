(** This library exposes the values that can be used in a CSS declaration as polymorphic
    variants. It does not expose a state machine for validating expressions as valid CSS *)

include module type of Css_data_type
include module type of Generated_types
