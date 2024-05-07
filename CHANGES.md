## Release v0.17.0

- `ppx_css` can now be embedded within an OCaml expression, spiritually similar to
  [styled components](https://styled-components.com/):

```ocaml
Vdom.Node.div ~attrs:[ [%css {|
  background-color: tomato;
  min-width: 2rem;
  min-height: 2rem;
|}] ] []
```

- `ppx_css` now has an interpolation syntax similar to
  [ppx_string's](https://github.com/janestreet/ppx_string) 's inteporlation syntax:

```ocaml
let f (color : string) =
  Vdom.Node.div ~attrs:[ [%css {|
    border: 0.3rem solid %{color};
  |}] ] []
```

- `ppx_css` now supports [Nested
  CSS](https://developer.chrome.com/articles/css-nesting/), including the `&` selector:

```ocaml
[%css {|
  background-color: tomato;
  &:hover {
    background-color: red;
  }
|}]
```

- `ppx_css`'s embedded OCaml now interacts nicely with merlin

- Added `Variables.set_all` function to set all of the CSS variables present in a
  `ppx_css` stylesheet at once.

- Added ability to configure caching behavior at the "jbuild" level.

- Fixed a performance regression in `ppx_css` where `ppx_css` performed unnecessary style
  recomputations.

- Fixed a bug where `ppx_css` falsely claimed a variable was unused.

- Fixed a bug in `ppx_css` where classnames in the expression syntax where not sanitized
  properly.


## Release v0.16.0

- `ppx_css` now works on browser versions that don't have access to `CSSStyleSheet`.
- `ppx_css` now supports CSS variables.
   * CSS variables are now hashed.
   * A function, `Variables.set` is generated whose signature is `?variable_name1:string -> ?variable_name2:string -> ... -> Vdom.Attr.t`
- New flags to control hashing behavior. Syntax is `[%css stylesheet {|...|} ~dont_hash:[] ~dont_hash_prefixes:[] ~rewrite:[]]`.
   * `dont_hash: string list` can be used to disable hashing behavior.
   * `dont_hash_prefixes: string list` can be used to disable hashing behavior based on a prefix.
   * `rewrite: (string * string) list` can be used to override hashing behavior by
     "rewriting" identifiers (e.g. `["a", "b"; "c", "d"]` results in "a" being "rewritten"
     to "b" and "c" to "d").

Breaking changes:

-  Renamed the callsite from `[%css.raw {|...|}]` to `[%css stylesheet {|...|}]`.
- `ppx_css` used to generate OCaml strings `val identifier : string`, now the identifiers
   generate `val identifier : Vdom.Attr.t`. Where the attribute is applied with `Vdom.Attr.class_`
   or `Vdom.Attr.id` depending on the identifier that was used. If there are classes and id's with
   the same name, `<identifier>_id` and `<identifier>_class` are generated instead. This
   was only done for convenience of users that use ppx_css alongside the library
   `virtual_dom` or `bonsai_web` that always called `Vdom.Attr.class_`  or `Vdom.Attr.id`.
   If you would like to still access the strings directly, they will be contained within
   newly generated `For_referencing` module.
