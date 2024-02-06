open! Core
open Virtual_dom

(**
   This library allows for "dynamic" use of the CSS Constructed Stylesheet API.
   It is more powerful than [inline_css], but can be much less performant.

   Tl;dr: you should try as hard as possible for the input to [attr] to be a
   constant, not a [string Value.t].

   [inline_css] registers the provided string as a constructed
   stylesheet as a side effect. It will never change or remove the string.

   In contrast, this library's produces a [Vdom.Attr.t], which will also remove
   the stylesheet when it is removed from the DOM, and will update the stylesheet
   whenever the provided string changes, e.g. if the attr is produced by a
   Bonsai [Value.t]. Every such update will trigger a browser style recomputation,
   which can be very expensive if many DOM elements are affected.
*)

val attr : string -> Vdom.Attr.t
