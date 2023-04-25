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
