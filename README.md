# PPX CSS

__PPX CSS__ is a
[PPX](https://ocaml.org/docs/metaprogramming#:~:text=OCaml%20supports%20the%20execution%20of,acronym%20for%20Pre%2DProcessor%20eXtension.)
for dealing with [CSS](https://developer.mozilla.org/en-US/docs/Web/CSS) from within OCaml
    🐫. It is designed to be used within apps made with [Bonsai
    🌳](https://opensource.janestreet.com/bonsai/),
    [Incr_dom](https://opensource.janestreet.com/incr_dom/), or anything
    that uses the [virtual_dom](https://github.com/janestreet/virtual_dom) library. It
    makes your CSS _safe_, _composable_, and _convenient_.


## "Styled Components" Syntax

`css` can be embedded within an OCaml expression. Spiritually similar to [styled
components](https://styled-components.com/). This will expand into a `Vdom.Attr.t`:

```ocaml
Vdom.Node.div ~attrs:[ [%css {|
  background-color: tomato;
  min-width: 2rem;
  min-height: 2rem;
|}] ] []
```

It also has the same interpolation syntax as [ppx_string](https://github.com/janestreet/ppx_string):

```ocaml
let f (color : string) =
  Vdom.Node.div ~attrs:[ [%css {|
    border: 0.3rem solid %{color};
  |}] ] []
```

like `ppx_string`, you can specify a module, it will call the module's `to_string_css` function:

```ocaml
let f (color : Css_gen.Color.t) =
  Vdom.Node.div ~attrs:[ [%css {|
    border: 0.3rem solid %{color#Css_gen.Color};
  |}] ] []
```

You can also use [Nested CSS](https://developer.chrome.com/articles/css-nesting/):

```ocaml
[%css {|
  background-color: tomato;
  &:hover {
    background-color: red;
  }
|}]
```

## "Stylesheet" Syntax

The recommended way of using ppx_css is via the styled_component syntax. However, in some
situations, you do need to use @media/@keyframes other things that are not expressible in the
styled_component syntax/not expressible with nested CSS. In such scenarios, `css` also has
a "stylesheet" syntax available:

```ocaml
module Styles = [%css stylesheet {|
 .card:hover {
    background-color: tomato;
 }

 :root {
    background-color: tomato;
  }
|}]
```

It will generate `Styles.card : Vdom.Attr.t` that you can then attach to style your app.
PPX CSS will additionally do the following things behind the scenes:

- It will **hash** your identifiers. Normal CSS is prone to name clashes. PPX_CSS adds a
  unique hash to your classnames (e.g. "card_hash_12345") to be resilient against CSS
  naming clashes. You do not need to add unique prefixes while using ppx_css.
- It will **register** your CSS. You do not need to serve/bundle your own CSS files. Under
  the hood, ppx_css registers your styles using the [CSSStylesheet
  API](https://developer.mozilla.org/en-US/docs/Web/API/CSSStyleSheet).


You can opt-out of hashing behavior/customize it with the following options.
The syntax for sending options is: `[%css stylesheet {|...css string...|} ~option_name:OPTION_VALUE]`

- `~dont_hash:string list`: Will not hash the identifiers it sees.  
  (e.g. `[%css stylesheet {||} ~dont_hash:["card"]]`)
- `~dont_hash_prefixes:string list`: Will not hash identifiers that match that prefix.
  (e.g. `[%css stylesheet {||} ~dont_hash_prefixes:[--]]` will stop hashing all css
  variables.)

The stylesheet syntax also generates a `For_referencing` module that includes -
  `Styles.For_referencing.card : string` - the post-hashed name.

If you use CSS Variables, it will also generate a `Variables` module that has two functions:
`set: ?css_variable1:string -> ?css_variable2:string -> unit -> Vdom.Attr.t` and `set_all
: css_variable1:string -> css_variable2:string -> Vdom.Attr.t` that let you set the post-hashed
variables.

## Lazy Loading Optimization

**Note**: This optimization mutates the input CSS

### How to enable

To enable the optimization within a specific library, you can:

Use `(preprocess (pps (ppx_css -lazy-loading-optimization=true ppx_bonsai …)))` instead 
of just `(preprocess (pps (ppx_css ppx_bonsai …)))`, if using ppx_css 
Use `(css (<filename> (lazy_loading_optimization true)))` if using the CSS inliner jbuild 
rule If you turn this optimization on for your library, it will be enabled for all apps 
that haven’t explicitly opted out via the environmental variable! 
**Please thoroughly test your library to make sure all styles in all states are consistent 
with what you expect before doing so.**

It takes the input CSS and splits it by top-level rules. An empty constructed stylesheet 
is created for each rule and is registered with the document so that order of the rules is 
maintained. For example, the below input will be split into 3 individual stylesheets:

```css
/* This code block acts as a singular input to PPX_CSS. It will be split into 3 individual stylesheets due to the optimization */

/* Sheet 1 */
.a + .b {
  .c {
  } 
}

/* Sheet 2 */
.d .e {
  .f {}
}

/* Sheet 3 */
#g {
  .a {}
}
```

`@layer` rules are special-cased and will recursively split themselves into individual style 
rules nested within the layer.

For example, this input:

```css
/* Input stylesheet */
@layer layer-1 {
  @layer layer-2 {
    .a {
      .b {}
    }
   
    .c {}
  }
  
  .d {}

  @layer layer-3 {
    .e {}
  }
}
```

Will become this:

```css
/* Sheet 1 */
@layer layer-1 {
  @layer layer-2 {
    .a {
      .b {}
    }
  }
}

/* Sheet 2 */
@layer layer-1 {
  @layer layer-2 {
    .c {}
  }
}

/* Sheet 3 */
@layer layer-1 {
  .d {}
}

/* Sheet 4 */
@layer layer-1 {
  @layer layer-3 {
    .e {}
  }
}
```

Order of the rules is maintained. Layers nested within style blocks will not be split.

Every `@-rule` aside from `@layer` is considered to not be eligible for laziness and will 
automatically update its related constructed stylesheet and be registered at app 
instantiation.

Each of the newly created sub-stylesheets should have one rule each. We retrieve all 
selectors in the top level of the rule and compute if the rule should be automatically 
forced or not. 

We also retrieve the selectors within specific pseudoclass functions to include in this 
calculation, such as `:has`. These functions are special-cased after being audited for 
compatibility, as certain functions such as `:not` apply to almost everything and cannot be 
considered for laziness. If the selectors within `:not` were made lazy, there is a chance 
that those styles would never be applied within the app.  There is also a chance that CSS 
adds more functions like this in the future, and we’d like to err on the side of caution 
in the laziness calculation.

For example:

```css
/* This code block acts as a singular input to PPX_CSS. It will be split into 3 individual stylesheets due to the optimization */

/* Sheet 1 */
.a + .b {
  .c {
  } 
}

/* Sheet 2 */
div, .b {
  .f {}
}

/* Sheet 3 */
div > .b {
}

/* Sheet 4 */
.e {
  :not(&) {}
}
```


Sheet 1 can be made lazy, as none of the styles will ever be applied unless elements with 
selector `.a` and `.b` exist in the app

Sheet 2 cannot be made lazy, the rules styles apply to `div` elements OR `.b` elements. 
For our purposes, we consider `div`, `span`, `*`, and any other type selector to be ineligible 
for laziness if it is the only selector that appears within the selector.

Sheet 3 can be made lazy even though it includes a type selector, as it also requires a `.b`
element to exist within the app. The difference here is that it has a combinator between 
the type selector and the `.b` selector, while Sheet 2 includes the two as different 
selectors in a selector list. 

Sheet 4 also cannot be made lazy, as the inner rule applies to anything that is not `.e`, 
which is basically everything.

For the sheets that can be made lazy, we then retrieve all class and ID selectors within 
the rules and insert them into a graph. We then check to see which selectors are connected 
in the graph, and group the related stylesheets based on their selector connectivity. 
We create a lazy update function for this group that registers the styles for all related 
constructed stylesheets and create a lazy `Vdom.Attr.t` for each selector. Whenever a node 
that has the lazy attr is attached to the DOM, it attempts to force the lazy update 
function, which will register the styles of its associated group with the app. 
This means that for the example above, if the stylesheet was created with something like 
`module Styles = [%css stylesheet …]` if a node with `Styles.c` is rendered into the DOM, 
the entirety of Sheet 1 as well as Sheet 2 will be registered with the app as they both 
contain `.b`.

