(** This module contains all of the base data types that are not defined in the
    machine-readable CSS spec.

    It is currently missing the following definitions:
    - age
    - counter-name
    - counter-style-name
    - gender
    - system-color (deprecated)
    - timeline-range-name
    - transform-function (planned to be implemented eventually)

    Most of the missing definitions are incredibly unlikely to be used, so they have been
    omitted. *)

open! Core

module Css_string : sig
  type t = [ `String of string ]

  val to_string_css : t -> string
end

module Ident : sig
  module Custom : sig
    (** [Custom] is a CSS custom-ident data type. It cannot be one of the CSS-wide
        keywords, or any keyword that is specific to the property the custom-ident is
        being used in. *)
    type ident

    type t = [ `Custom_ident of ident ]

    (** [create] takes an input string and normalizes it so it fits the syntax of the CSS
        custom-ident data type.

        In order to prevent collisions with keywords, we add a suffix to the custom
        identifier. *)
    val create : string -> [> t ]

    (** [create_unsafe] will return the input string as a [t]. Please note that this
        method does not do any normalizations and cannot guarantee that the custom
        identifier returned does not overlaps with a keyword. *)
    val create_unsafe : string -> [> t ]

    val to_string_css : t -> string
  end

  module Dashed : sig
    (** [Dashed] is a CSS dashed-ident data type. [ident] will always be prefixed with
        [--] in order to ensure that this is a dashed identifier. *)
    type ident

    type t = [ `Dashed_ident of ident ]

    (** [create] takes an input string and normalizes it so it fits the syntax of the CSS
        ident data type, and then prefixes it with [--] if it doesn't start with [--]. As
        no CSS keyword starts with [--], we don't have to add a collision suffix.
        Therefore, every call to [create] with the same input will result with the same
        output *)
    val create : string -> [> t ]

    val to_string_css : t -> string
  end

  type ident

  type t =
    [ Custom.t
    | Dashed.t
    | `Ident of ident
    ]

  (** [create] normalizes the input string so it fits a valid CSS identifier. As valid
      identifiers include CSS keywords, we do not need to ensure any uniqueness.

      If the output is an empty string, we will return an error *)
  val create : string -> [> `Ident of ident ] Or_error.t

  (** [create_exn] normalizes the input string so it fits a valid CSS identifier. As valid
      identifiers include CSS keywords, we do not need to ensure any uniqueness.

      If the output is an empty string, we will raise an exception *)
  val create_exn : string -> [> `Ident of ident ]

  val to_string_css : t -> string
end

module Custom_property_name : sig
  (** [Custom_property_name] is the same as [Ident.Dashed], except it has the added
      restriction of not allowing for the string [--]

      https://www.w3.org/TR/css-variables-1/#typedef-custom-property-name *)

  type ident
  type t = [ `Custom_property_name of ident ]

  val create : string -> [> t ] Or_error.t
  val create_exn : string -> [> t ]
  val to_string_css : t -> string
end

module Custom_property : sig
  (** [Custom_property] is the name that goes into [var(<custom-property>)].

      The [string] given to the create functions that accept [string] __must__ be prefixed
      with [--], but cannot be [--] itself. *)

  type 'a t

  val name : 'a t -> Custom_property_name.t
  val default : 'a t -> 'a option

  module Expert : sig
    (** [create_without_registering] does not register the property as a custom property,
        meaning that CSS will not guarantee that the value matches ['a]. *)
    val create_without_registering : ?default:'a -> string -> 'a t Or_error.t

    val create_without_registering' : ?default:'a -> Custom_property_name.t -> 'a t
    val create_without_registering_exn : ?default:'a -> string -> 'a t
  end
end

module Var : sig
  (** [Var] refers to the function [var()] in CSS.

      https://www.w3.org/TR/css-variables-1/#using-variables *)
  type 'a t = [ `Var of 'a Custom_property.t ]

  val to_string_css : ('a -> string) -> 'a t -> string
end

module Global_values : sig
  (** These global values come from the CSS spec and can be used by any property.
      Annoyingly, they are not defined in a singular place in the spec but come from
      several different specs. The level 5 spec is currently the latest one that defines
      the keywords

      https://www.w3.org/TR/css-cascade-5/#defaulting-keywords *)
  type 'a t =
    [ 'a Var.t
    | `Initial
    | `Inherit
    | `Revert
    | `Revert_layer
    | `Unset
    ]

  val to_string_css : ('a -> string) -> 'a t -> string
end

module Integer : sig
  (** [Integer] refers to the integer type. Notably, different properties will restrict
      the values of integers that can be used within them, which cannot be captured in the
      type below

      Note that this is different from [Css_gen] which defines an integer as
      [`Number of int]

      https://www.w3.org/TR/css-values-3/#integers *)
  type t = [ `Int of int ]

  val to_string_css : t -> string
end

module Number : sig
  (** [Number] refers to any real number, which by definition includes integers. We are
      not exposing [`Integer_with_exponent] as users can just use [Number_with_exponent].

      https://www.w3.org/TR/css-values-3/#numbers *)
  type t =
    [ Integer.t
    | `Float of float
    | `Number_with_exponent of float * float
    ]

  val to_string_css : t -> string
end

module Percentage : sig
  (** [Percentage] represents a CSS [number] followed by the [%] symbol.

      https://www.w3.org/TR/css-values-3/#percentages *)
  type t = [ `Percent of Percent.t ]

  val to_string_css : t -> string
end

module Time : sig
  type t =
    [ `Seconds of float
    | `Milliseconds of float
    ]

  val to_string_css : t -> string
end

module Flex_fr : sig
  (** [Flex_fr] refers to the flexible length unit [fr]. It's defined as not being
      equivalent to a [Length] unit, which means they must be considered a distinct type.

      It's named [Flex_fr] to disambiguate from the CSS property name [flex]

      https://www.w3.org/TR/css-grid-1/#typedef-flex *)
  type t = [ `Fr of float ]

  val to_string_css : t -> string
end

module Angle : sig
  (** [Angle] represents all units classified by the dimension <angle>
      https://www.w3.org/TR/css-values-3/#angles *)

  type t =
    [ `Deg of float
    | `Grad of float
    | `Rad of float
    | `Turn of float
    ]

  val to_string_css : t -> string
end

module Alpha_value : sig
  (** [Alpha_value] is not a base type, but is defined here so that we can define the CSS
      color functions.

      https://www.w3.org/TR/css-color-4/#alpha-syntax *)

  type t =
    [ Number.t
    | Percentage.t
    ]

  val to_string_css : t -> string
end

module Color : sig
  module Named : sig
    (** [Named] represents named colors. These values are outlined in the spec below:

        https://www.w3.org/TR/css-color-3/#colorunits *)

    type t =
      [ `Red
      | `Purple
      | `Teal
      | `Yellow
      | `Green
      ]

    val to_string_css : t -> string
  end

  module Hex : sig
    (** [Hex] represents a hex color.

        [`Hex_color of hexcode] represents a valid hex color string. *)

    type hexcode
    type t = [ `Hex_color of hexcode ]

    val to_string_css : t -> string

    (** [create] validates the provided string to ensure that it's a valid hex string.

        It will add a leading [#] if not provided.

        It will return an error if the string does not match a valid hex color value *)
    val create : string -> [> t ] Or_error.t

    (** [create] validates the provided string to ensure that it's a valid hex string.

        It will add a leading [#] if not provided.

        It will raise an exception if the string does not match a valid hex color value *)
    val create_exn : string -> [> t ]
  end

  module Hue : sig
    type t =
      [ Number.t
      | Angle.t
      ]

    val to_string_css : [< t ] -> string
  end

  type t =
    [ `Rgba of rgba
    | `Hsla of hsla
    | `Lcha of lcha
    | `Hwb of hwb
    | `Lab of lab
    | `Oklab of oklab
    | `Oklcha of oklcha
    | `Alpha of alpha
    | `Named of Named.t
    | Hex.t
    | `Light_dark of color_with_var * color_with_var
    ]

  and oklcha
  and lcha
  and rgba
  and hsla
  and lab
  and oklab
  and alpha
  and hwb

  (** Unfortunately recursive polymorphic variant types cannot directly include a type
      that it's recursive with unless that type is wrapped in a tag/constructor *)
  and color_with_var =
    [ `Color of t
    | color_with_var Var.t
    ]

  type color_channel :=
    [ Percentage.t
    | Number.t
    | `None
    ]

  type hue_channel :=
    [ Hue.t
    | `None
    ]

  type alpha_channel :=
    [ Alpha_value.t
    | `None
    ]

  module With_var : sig
    type t = color_with_var

    val to_string_css : t -> string
  end

  module Rgba : sig
    type t = rgba

    val create
      :  r:color_channel
      -> g:color_channel
      -> b:color_channel
      -> ?a:alpha_channel
      -> unit
      -> t

    (** [create_relative ~r ~g ~b ?a from] creates a color relative to [from]. Passing in
        [`Relative] to a color channel will use the appropriate color channel keyword in
        the generated CSS *)
    val create_relative
      :  r:[ color_channel | `Relative ]
      -> g:[ color_channel | `Relative ]
      -> b:[ color_channel | `Relative ]
      -> ?a:[ alpha_channel | `Relative ]
      -> color_with_var
      -> t

    val from : t -> color_with_var option
    val r : t -> [ color_channel | `Relative_to of color_with_var ]
    val g : t -> [ color_channel | `Relative_to of color_with_var ]
    val b : t -> [ color_channel | `Relative_to of color_with_var ]
    val a : t -> [ alpha_channel | `Relative_to of color_with_var ] option
  end

  module Hsla : sig
    type t = hsla

    val create
      :  h:hue_channel
      -> s:color_channel
      -> l:color_channel
      -> ?a:alpha_channel
      -> unit
      -> t

    (** [create_relative ~h ~s ~l ?a from] creates a color relative to [from]. Passing in
        [`Relative] to a color channel will use the appropriate color channel keyword in
        the generated CSS *)
    val create_relative
      :  h:[ hue_channel | `Relative ]
      -> s:[ color_channel | `Relative ]
      -> l:[ color_channel | `Relative ]
      -> ?a:[ alpha_channel | `Relative ]
      -> color_with_var
      -> t

    val from : t -> color_with_var option
    val h : t -> [ hue_channel | `Relative_to of color_with_var ]
    val s : t -> [ color_channel | `Relative_to of color_with_var ]
    val l : t -> [ color_channel | `Relative_to of color_with_var ]
    val a : t -> [ alpha_channel | `Relative_to of color_with_var ] option
  end

  module Hwb : sig
    type t = hwb

    val create
      :  h:hue_channel
      -> w:color_channel
      -> b:color_channel
      -> ?a:alpha_channel
      -> unit
      -> t

    (** [create_relative ~h ~w ~b ?a from] creates a color relative to [from]. Passing in
        [`Relative] to a color channel will use the appropriate color channel keyword in
        the generated CSS *)
    val create_relative
      :  h:[ hue_channel | `Relative ]
      -> w:[ color_channel | `Relative ]
      -> b:[ color_channel | `Relative ]
      -> ?a:[ alpha_channel | `Relative ]
      -> color_with_var
      -> t

    val from : t -> color_with_var option
    val h : t -> [ hue_channel | `Relative_to of color_with_var ]
    val w : t -> [ color_channel | `Relative_to of color_with_var ]
    val b : t -> [ color_channel | `Relative_to of color_with_var ]
    val a : t -> [ alpha_channel | `Relative_to of color_with_var ] option
  end

  module Oklcha : sig
    type t = oklcha

    val create
      :  l:color_channel
      -> c:color_channel
      -> h:hue_channel
      -> ?a:alpha_channel
      -> unit
      -> t

    (** [create_relative ~l ~c ~h ?a from] creates a color relative to [from]. Passing in
        [`Relative] to a color channel will use the appropriate color channel keyword in
        the generated CSS *)
    val create_relative
      :  l:[ color_channel | `Relative ]
      -> c:[ color_channel | `Relative ]
      -> h:[ hue_channel | `Relative ]
      -> ?a:[ alpha_channel | `Relative ]
      -> color_with_var
      -> t

    val from : t -> color_with_var option
    val l : t -> [ color_channel | `Relative_to of color_with_var ]
    val c : t -> [ color_channel | `Relative_to of color_with_var ]
    val h : t -> [ hue_channel | `Relative_to of color_with_var ]
    val a : t -> [ alpha_channel | `Relative_to of color_with_var ] option
  end

  module Lcha : sig
    type t = lcha

    val create
      :  l:color_channel
      -> c:color_channel
      -> h:hue_channel
      -> ?a:alpha_channel
      -> unit
      -> t

    (** [create_relative ~l ~c ~h ?a from] creates a color relative to [from]. Passing in
        [`Relative] to a color channel will use the appropriate color channel keyword in
        the generated CSS *)
    val create_relative
      :  l:[ color_channel | `Relative ]
      -> c:[ color_channel | `Relative ]
      -> h:[ hue_channel | `Relative ]
      -> ?a:[ alpha_channel | `Relative ]
      -> color_with_var
      -> t

    val from : t -> color_with_var option
    val l : t -> [ color_channel | `Relative_to of color_with_var ]
    val c : t -> [ color_channel | `Relative_to of color_with_var ]
    val h : t -> [ hue_channel | `Relative_to of color_with_var ]
    val a : t -> [ alpha_channel | `Relative_to of color_with_var ] option
  end

  module Oklab : sig
    type t = oklab

    val create
      :  l:color_channel
      -> a:color_channel
      -> b:color_channel
      -> ?alpha:alpha_channel
      -> unit
      -> t

    (** [create_relative ~l ~a ~b ?alpha from] creates a color relative to [from]. Passing
        in [`Relative] to a color channel will use the appropriate color channel keyword
        in the generated CSS *)
    val create_relative
      :  l:[ color_channel | `Relative ]
      -> a:[ color_channel | `Relative ]
      -> b:[ color_channel | `Relative ]
      -> ?alpha:[ alpha_channel | `Relative ]
      -> color_with_var
      -> t

    val from : t -> color_with_var option
    val l : t -> [ color_channel | `Relative_to of color_with_var ]
    val a : t -> [ color_channel | `Relative_to of color_with_var ]
    val b : t -> [ color_channel | `Relative_to of color_with_var ]
    val alpha : t -> [ alpha_channel | `Relative_to of color_with_var ] option
  end

  module Lab : sig
    type t = lab

    val create
      :  l:color_channel
      -> a:color_channel
      -> b:color_channel
      -> ?alpha:alpha_channel
      -> unit
      -> t

    (** [create_relative ~l ~a ~b ?alpha from] creates a color relative to [from]. Passing
        in [`Relative] to a color channel will use the appropriate color channel keyword
        in the generated CSS *)
    val create_relative
      :  l:[ color_channel | `Relative ]
      -> a:[ color_channel | `Relative ]
      -> b:[ color_channel | `Relative ]
      -> ?alpha:[ alpha_channel | `Relative ]
      -> color_with_var
      -> t

    val from : t -> color_with_var option
    val l : t -> [ color_channel | `Relative_to of color_with_var ]
    val a : t -> [ color_channel | `Relative_to of color_with_var ]
    val b : t -> [ color_channel | `Relative_to of color_with_var ]
    val alpha : t -> [ alpha_channel | `Relative_to of color_with_var ] option
  end

  module Alpha : sig
    type t = alpha

    val create : from:color_with_var -> ?alpha:[ alpha_channel | `Relative ] -> unit -> t
    val alpha : t -> [ alpha_channel | `Relative_to of color_with_var ] option
  end

  val to_string_css : t -> string

  val rgba
    :  r:color_channel
    -> g:color_channel
    -> b:color_channel
    -> ?a:alpha_channel
    -> unit
    -> [> `Rgba of rgba ]

  val rgba_relative
    :  r:[ color_channel | `Relative ]
    -> g:[ color_channel | `Relative ]
    -> b:[ color_channel | `Relative ]
    -> ?a:[ alpha_channel | `Relative ]
    -> color_with_var
    -> [> `Rgba of rgba ]

  val hsla
    :  h:hue_channel
    -> s:color_channel
    -> l:color_channel
    -> ?a:alpha_channel
    -> unit
    -> [> `Hsla of hsla ]

  val hsla_relative
    :  h:[ hue_channel | `Relative ]
    -> s:[ color_channel | `Relative ]
    -> l:[ color_channel | `Relative ]
    -> ?a:[ alpha_channel | `Relative ]
    -> color_with_var
    -> [> `Hsla of hsla ]

  val hwb
    :  h:hue_channel
    -> w:color_channel
    -> b:color_channel
    -> ?a:alpha_channel
    -> unit
    -> [> `Hwb of hwb ]

  val hwb_relative
    :  h:[ hue_channel | `Relative ]
    -> w:[ color_channel | `Relative ]
    -> b:[ color_channel | `Relative ]
    -> ?a:[ alpha_channel | `Relative ]
    -> color_with_var
    -> [> `Hwb of hwb ]

  val lcha
    :  l:color_channel
    -> c:color_channel
    -> h:hue_channel
    -> ?a:alpha_channel
    -> unit
    -> [> `Lcha of lcha ]

  val lcha_relative
    :  l:[ color_channel | `Relative ]
    -> c:[ color_channel | `Relative ]
    -> h:[ hue_channel | `Relative ]
    -> ?a:[ alpha_channel | `Relative ]
    -> color_with_var
    -> [> `Lcha of lcha ]

  val oklcha
    :  l:color_channel
    -> c:color_channel
    -> h:hue_channel
    -> ?a:alpha_channel
    -> unit
    -> [> `Oklcha of oklcha ]

  val oklcha_relative
    :  l:[ color_channel | `Relative ]
    -> c:[ color_channel | `Relative ]
    -> h:[ hue_channel | `Relative ]
    -> ?a:[ alpha_channel | `Relative ]
    -> color_with_var
    -> [> `Oklcha of oklcha ]

  val oklab
    :  l:color_channel
    -> a:color_channel
    -> b:color_channel
    -> ?alpha:alpha_channel
    -> unit
    -> [> `Oklab of oklab ]

  val oklab_relative
    :  l:[ color_channel | `Relative ]
    -> a:[ color_channel | `Relative ]
    -> b:[ color_channel | `Relative ]
    -> ?alpha:[ alpha_channel | `Relative ]
    -> color_with_var
    -> [> `Oklab of oklab ]

  val lab
    :  l:color_channel
    -> a:color_channel
    -> b:color_channel
    -> ?alpha:alpha_channel
    -> unit
    -> [> `Lab of lab ]

  val lab_relative
    :  l:[ color_channel | `Relative ]
    -> a:[ color_channel | `Relative ]
    -> b:[ color_channel | `Relative ]
    -> ?alpha:[ alpha_channel | `Relative ]
    -> color_with_var
    -> [> `Lab of lab ]

  val alpha
    :  from:color_with_var
    -> ?alpha:[ alpha_channel | `Relative ]
    -> unit
    -> [> `Alpha of alpha ]
end

module Length : sig
  module Relative : sig
    module Font : sig
      (** These units are relative to the font size
          https://www.w3.org/TR/css-values-4/#font-relative-lengths *)
      type t =
        [ `Em of float
        | `Rem of float
        | `Ex of float
        | `Rex of float
        | `Cap of float
        | `Rcap of float
        | `Ch of float
        | `Rch of float
        | `Ic of float
        | `Ric of float
        | `Lh of float
        | `Rlh of float
        ]

      val to_string_css : t -> string
    end

    module Viewport : sig
      (** These units are percentages of the viewport

          https://www.w3.org/TR/css-values-4/#viewport-relative-lengths *)
      type t =
        [ `Vw of Percent.t
        | `Vh of Percent.t
        | `Vi of Percent.t
        | `Vb of Percent.t
        | `Vmin of Percent.t
        | `Vmax of Percent.t
        | `Svw of Percent.t
        | `Svh of Percent.t
        | `Svi of Percent.t
        | `Svb of Percent.t
        | `Svmin of Percent.t
        | `Svmax of Percent.t
        | `Lvw of Percent.t
        | `Lvh of Percent.t
        | `Lvi of Percent.t
        | `Lvb of Percent.t
        | `Lvmin of Percent.t
        | `Lvmax of Percent.t
        | `Dvw of Percent.t
        | `Dvh of Percent.t
        | `Dvi of Percent.t
        | `Dvb of Percent.t
        | `Dvmin of Percent.t
        | `Dvmax of Percent.t
        ]

      val to_string_css : t -> string
    end

    (** [Relative] units are defined here:

        https://www.w3.org/TR/css-values-4/#relative-lengths *)
    type t =
      [ Font.t
      | Viewport.t
      ]

    val to_string_css : t -> string
  end

  module Absolute : sig
    (** [Absolute] units are defined here:
        https://www.w3.org/TR/css-values-4/#absolute-lengths *)
    type t =
      [ `Cm of float
      | `Mm of float
      | `Quarter_mm of float
      | `Inch of float
      | `Pica of float
      | `Pt of float
      | `Px of float
      ]

    val to_string_css : t -> string
  end

  (** [Length] represents all possible values that can be used as a CSS length.

      For the most part, the value of the argument will be a [float].

      https://www.w3.org/TR/css-values-4/#lengths *)
  type t =
    [ Relative.t
    | Absolute.t
    | `Zero
    ]

  val to_string_css : t -> string
end

module Decibel : sig
  (** https://www.w3.org/TR/css-speech-1/#typedef-voice-volume-decibel *)
  type t = [ `Decibel of float ]

  val to_string_css : t -> string
end

module Frequency : sig
  (** https://www.w3.org/TR/css-values-4/#frequency-value *)
  type t =
    [ `Hz of float
    | `Khz of float
    ]

  val to_string_css : t -> string
end

module Resolution : sig
  (** https://www.w3.org/TR/css-values-3/#resolution *)
  type t =
    [ `Dpi of float
    | `Dpcm of float
    | `Dppx of float
    ]

  val to_string_css : t -> string
end
