open! Core

(** Top-level expect tests cannot _only_ be run in a jsoo
    context. This makes testing of compilation error locations
    tricky to test as the virtual_dom library cannot be depended on
    as the real virtual_dom library depends on js_of_ocaml.

    This library stubs out Virtual_dom and other ppx_css-related libraries
    to in a native context. . *)
module Vdom : sig
  module Attr : sig
    type t

    val __css_vars_no_kebabs : (string * string) list -> t
    val combine : t -> t -> t
    val class_ : string -> t
  end
end

module Css_gen : sig
  module Color : sig
    val to_string_css : 'a -> string
  end
end

module Inline_css : sig
  module Private : sig
    val append : string -> unit
    val append_but_do_not_update : string -> unit
  end
end
