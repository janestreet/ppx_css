module Attachment_type = struct
  type t =
    [ `Fixed
    | `Local
    | `Scroll
    ]

  let to_string_css = function
    | `Fixed -> "fixed"
    | `Local -> "local"
    | `Scroll -> "scroll"
  ;;
end

module Autospace_type = struct
  type t =
    [ `Ideograph_alpha
    | `Ideograph_numeric
    | `Insert
    | `No_autospace
    | `Punctuation
    | `Replace
    ]

  let to_string_css = function
    | `Ideograph_alpha -> "ideograph-alpha"
    | `Ideograph_numeric -> "ideograph-numeric"
    | `Insert -> "insert"
    | `No_autospace -> "no-autospace"
    | `Punctuation -> "punctuation"
    | `Replace -> "replace"
  ;;
end

module Axis_type = struct
  type t =
    [ `Block
    | `Inline
    | `X
    | `Y
    ]

  let to_string_css = function
    | `Block -> "block"
    | `Inline -> "inline"
    | `X -> "x"
    | `Y -> "y"
  ;;
end

module Baseline_position_type = struct
  type t =
    [ `Baseline
    | `First
    | `Last
    ]

  let to_string_css = function
    | `Baseline -> "baseline"
    | `First -> "first"
    | `Last -> "last"
  ;;
end

module Blend_mode_type = struct
  type t =
    [ `Color_
    | `Color_burn
    | `Color_dodge
    | `Darken
    | `Difference
    | `Exclusion
    | `Hard_light
    | `Hue
    | `Lighten
    | `Luminosity
    | `Multiply
    | `Normal
    | `Overlay
    | `Saturation
    | `Screen
    | `Soft_light
    ]

  let to_string_css = function
    | `Color_ -> "color"
    | `Color_burn -> "color-burn"
    | `Color_dodge -> "color-dodge"
    | `Darken -> "darken"
    | `Difference -> "difference"
    | `Exclusion -> "exclusion"
    | `Hard_light -> "hard-light"
    | `Hue -> "hue"
    | `Lighten -> "lighten"
    | `Luminosity -> "luminosity"
    | `Multiply -> "multiply"
    | `Normal -> "normal"
    | `Overlay -> "overlay"
    | `Saturation -> "saturation"
    | `Screen -> "screen"
    | `Soft_light -> "soft-light"
  ;;
end

module Common_lig_values_type = struct
  type t =
    [ `Common_ligatures
    | `No_common_ligatures
    ]

  let to_string_css = function
    | `Common_ligatures -> "common-ligatures"
    | `No_common_ligatures -> "no-common-ligatures"
  ;;
end

module Compositing_operator_type = struct
  type t =
    [ `Add
    | `Exclude
    | `Intersect
    | `Subtract
    ]

  let to_string_css = function
    | `Add -> "add"
    | `Exclude -> "exclude"
    | `Intersect -> "intersect"
    | `Subtract -> "subtract"
  ;;
end

module Content_distribution_type = struct
  type t =
    [ `Space_around
    | `Space_between
    | `Space_evenly
    | `Stretch
    ]

  let to_string_css = function
    | `Space_around -> "space-around"
    | `Space_between -> "space-between"
    | `Space_evenly -> "space-evenly"
    | `Stretch -> "stretch"
  ;;
end

module Content_position_type = struct
  type t =
    [ `Center
    | `End
    | `Flex_end
    | `Flex_start
    | `Start
    ]

  let to_string_css = function
    | `Center -> "center"
    | `End -> "end"
    | `Flex_end -> "flex-end"
    | `Flex_start -> "flex-start"
    | `Start -> "start"
  ;;
end

module Contextual_alt_values_type = struct
  type t =
    [ `Contextual
    | `No_contextual
    ]

  let to_string_css = function
    | `Contextual -> "contextual"
    | `No_contextual -> "no-contextual"
  ;;
end

module Anchor_name_type = struct
  type t = [ | Css_data_type.Ident.Dashed.t ]

  let to_string_css = function
    | #Css_data_type.Ident.Dashed.t as t -> Css_data_type.Ident.Dashed.to_string_css t
  ;;
end

module Discretionary_lig_values_type = struct
  type t =
    [ `Discretionary_ligatures
    | `No_discretionary_ligatures
    ]

  let to_string_css = function
    | `Discretionary_ligatures -> "discretionary-ligatures"
    | `No_discretionary_ligatures -> "no-discretionary-ligatures"
  ;;
end

module East_asian_variant_values_type = struct
  type t =
    [ `Jis04
    | `Jis78
    | `Jis83
    | `Jis90
    | `Simplified
    | `Traditional
    ]

  let to_string_css = function
    | `Jis04 -> "jis04"
    | `Jis78 -> "jis78"
    | `Jis83 -> "jis83"
    | `Jis90 -> "jis90"
    | `Simplified -> "simplified"
    | `Traditional -> "traditional"
  ;;
end

module East_asian_width_values_type = struct
  type t =
    [ `Full_width
    | `Proportional_width
    ]

  let to_string_css = function
    | `Full_width -> "full-width"
    | `Proportional_width -> "proportional-width"
  ;;
end

module Historical_lig_values_type = struct
  type t =
    [ `Historical_ligatures
    | `No_historical_ligatures
    ]

  let to_string_css = function
    | `Historical_ligatures -> "historical-ligatures"
    | `No_historical_ligatures -> "no-historical-ligatures"
  ;;
end

module Feature_value_name_type = struct
  type t = [ | Css_data_type.Ident.t ]

  let to_string_css = function
    | #Css_data_type.Ident.t as t -> Css_data_type.Ident.to_string_css t
  ;;
end

module Grid_line_type = struct
  type t =
    [ Css_data_type.Ident.Custom.t
    | Css_data_type.Integer.t
    | `Auto
    | `Span
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Custom.t as t -> Css_data_type.Ident.Custom.to_string_css t
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | `Auto -> "auto"
    | `Span -> "span"
  ;;
end

module Bottom_type = struct
  type t =
    [ Css_data_type.Length.t
    | `Auto
    ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Left_type = struct
  type t =
    [ Css_data_type.Length.t
    | `Auto
    ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Line_names_type = struct
  type t = [ | Css_data_type.Ident.Custom.t ]

  let to_string_css = function
    | #Css_data_type.Ident.Custom.t as t -> Css_data_type.Ident.Custom.to_string_css t
  ;;
end

module Line_style_type = struct
  type t =
    [ `Dashed
    | `Dotted
    | `Double
    | `Groove
    | `Hidden
    | `Inset
    | `None
    | `Outset
    | `Ridge
    | `Solid
    ]

  let to_string_css = function
    | `Dashed -> "dashed"
    | `Dotted -> "dotted"
    | `Double -> "double"
    | `Groove -> "groove"
    | `Hidden -> "hidden"
    | `Inset -> "inset"
    | `None -> "none"
    | `Outset -> "outset"
    | `Ridge -> "ridge"
    | `Solid -> "solid"
  ;;
end

module Line_width_type = struct
  type t =
    [ Css_data_type.Length.t
    | `Medium
    | `Thick
    | `Thin
    ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | `Medium -> "medium"
    | `Thick -> "thick"
    | `Thin -> "thin"
  ;;
end

module Gap_rule_type = struct
  type t =
    [ Css_data_type.Color.t
    | Line_style_type.t
    | Line_width_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Line_style_type.t as t -> Line_style_type.to_string_css t
    | #Line_width_type.t as t -> Line_width_type.to_string_css t
  ;;
end

module Gap_rule_or_repeat_type = struct
  type t =
    [ Gap_rule_type.t
    | Css_data_type.Integer.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Gap_rule_type.t as t -> Gap_rule_type.to_string_css t
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
  ;;
end

module Gap_auto_rule_list_type = struct
  type t =
    [ Gap_rule_type.t
    | Gap_rule_or_repeat_type.t
    | `Auto
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Gap_rule_type.t as t -> Gap_rule_type.to_string_css t
    | #Gap_rule_or_repeat_type.t as t -> Gap_rule_or_repeat_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Gap_rule_list_type = struct
  type t = [ | Gap_rule_or_repeat_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Gap_rule_or_repeat_type.t as t -> Gap_rule_or_repeat_type.to_string_css t
  ;;
end

module Masking_mode_type = struct
  type t =
    [ `Alpha_
    | `Luminance
    | `Match_source
    ]

  let to_string_css = function
    | `Alpha_ -> "alpha"
    | `Luminance -> "luminance"
    | `Match_source -> "match-source"
  ;;
end

module Line_name_list_type = struct
  type t =
    [ Css_data_type.Integer.t
    | Line_names_type.t
    | `Auto_fill
    ]

  let to_string_css = function
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | #Line_names_type.t as t -> Line_names_type.to_string_css t
    | `Auto_fill -> "auto-fill"
  ;;
end

module Numeric_figure_values_type = struct
  type t =
    [ `Lining_nums
    | `Oldstyle_nums
    ]

  let to_string_css = function
    | `Lining_nums -> "lining-nums"
    | `Oldstyle_nums -> "oldstyle-nums"
  ;;
end

module Numeric_fraction_values_type = struct
  type t =
    [ `Diagonal_fractions
    | `Stacked_fractions
    ]

  let to_string_css = function
    | `Diagonal_fractions -> "diagonal-fractions"
    | `Stacked_fractions -> "stacked-fractions"
  ;;
end

module Numeric_spacing_values_type = struct
  type t =
    [ `Proportional_nums
    | `Tabular_nums
    ]

  let to_string_css = function
    | `Proportional_nums -> "proportional-nums"
    | `Tabular_nums -> "tabular-nums"
  ;;
end

module Overflow_position_type = struct
  type t =
    [ `Safe
    | `Unsafe
    ]

  let to_string_css = function
    | `Safe -> "safe"
    | `Unsafe -> "unsafe"
  ;;
end

module Alpha_value_type = struct
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
  ;;
end

module Calc_value_type = struct
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `Negative_infinity
    | `NaN
    | `E
    | `Infinity
    | `Pi
    ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `Negative_infinity -> "-infinity"
    | `NaN -> "NaN"
    | `E -> "e"
    | `Infinity -> "infinity"
    | `Pi -> "pi"
  ;;
end

module Length_percentage_type = struct
  type t =
    [ Css_data_type.Length.t
    | Css_data_type.Percentage.t
    ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
  ;;
end

module Bg_size_type = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    | `Contain
    | `Cover
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
    | `Contain -> "contain"
    | `Cover -> "cover"
  ;;
end

module Inflexible_breadth_type = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    | `Max_content
    | `Min_content
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
    | `Max_content -> "max-content"
    | `Min_content -> "min-content"
  ;;
end

module Color_stop_list_type = struct
  type t =
    [ Css_data_type.Color.t
    | Length_percentage_type.t
    ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Position_type = struct
  type t =
    [ Length_percentage_type.t
    | `Block_end
    | `Block_start
    | `Bottom
    | `Center
    | `End
    | `Inline_end
    | `Inline_start
    | `Left
    | `Right
    | `Start
    | `Top
    | `X_end
    | `X_start
    | `Y_end
    | `Y_start
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Block_end -> "block-end"
    | `Block_start -> "block-start"
    | `Bottom -> "bottom"
    | `Center -> "center"
    | `End -> "end"
    | `Inline_end -> "inline-end"
    | `Inline_start -> "inline-start"
    | `Left -> "left"
    | `Right -> "right"
    | `Start -> "start"
    | `Top -> "top"
    | `X_end -> "x-end"
    | `X_start -> "x-start"
    | `Y_end -> "y-end"
    | `Y_start -> "y-start"
  ;;
end

module Bg_position_type = struct
  type t =
    [ Length_percentage_type.t
    | Position_type.t
    | `Bottom
    | `Center
    | `Left
    | `Right
    | `Top
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | #Position_type.t as t -> Position_type.to_string_css t
    | `Bottom -> "bottom"
    | `Center -> "center"
    | `Left -> "left"
    | `Right -> "right"
    | `Top -> "top"
  ;;
end

module Random_value_sharing_type = struct
  type t =
    [ Css_data_type.Ident.Dashed.t
    | Css_data_type.Number.t
    | `Auto
    | `Element_shared
    | `Fixed
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Dashed.t as t -> Css_data_type.Ident.Dashed.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | `Auto -> "auto"
    | `Element_shared -> "element-shared"
    | `Fixed -> "fixed"
  ;;
end

module Ratio_type = struct
  type t = [ | Css_data_type.Number.t ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
  ;;
end

module Color_interpolation_method_type = struct
  type t =
    [ Css_data_type.Ident.Dashed.t
    | `A98_rgb
    | `Decreasing
    | `Display_p3
    | `Hsl_
    | `Hue
    | `Hwb_
    | `In
    | `Increasing
    | `Lab_
    | `Lch_
    | `Longer
    | `Oklab_
    | `Oklch_
    | `Prophoto_rgb
    | `Rec2020
    | `Shorter
    | `Srgb
    | `Srgb_linear
    | `Xyz
    | `Xyz_d50
    | `Xyz_d65
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Dashed.t as t -> Css_data_type.Ident.Dashed.to_string_css t
    | `A98_rgb -> "a98-rgb"
    | `Decreasing -> "decreasing"
    | `Display_p3 -> "display-p3"
    | `Hsl_ -> "hsl"
    | `Hue -> "hue"
    | `Hwb_ -> "hwb"
    | `In -> "in"
    | `Increasing -> "increasing"
    | `Lab_ -> "lab"
    | `Lch_ -> "lch"
    | `Longer -> "longer"
    | `Oklab_ -> "oklab"
    | `Oklch_ -> "oklch"
    | `Prophoto_rgb -> "prophoto-rgb"
    | `Rec2020 -> "rec2020"
    | `Shorter -> "shorter"
    | `Srgb -> "srgb"
    | `Srgb_linear -> "srgb-linear"
    | `Xyz -> "xyz"
    | `Xyz_d50 -> "xyz-d50"
    | `Xyz_d65 -> "xyz-d65"
  ;;
end

module Radial_gradient_syntax_type = struct
  type t =
    [ Color_interpolation_method_type.t
    | Color_stop_list_type.t
    | Css_data_type.Length.t
    | Length_percentage_type.t
    | Position_type.t
    | `At
    | `Circle
    | `Closest_corner
    | `Closest_side
    | `Ellipse
    | `Farthest_corner
    | `Farthest_side
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Color_interpolation_method_type.t as t ->
      Color_interpolation_method_type.to_string_css t
    | #Color_stop_list_type.t as t -> Color_stop_list_type.to_string_css t
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | #Position_type.t as t -> Position_type.to_string_css t
    | `At -> "at"
    | `Circle -> "circle"
    | `Closest_corner -> "closest-corner"
    | `Closest_side -> "closest-side"
    | `Ellipse -> "ellipse"
    | `Farthest_corner -> "farthest-corner"
    | `Farthest_side -> "farthest-side"
  ;;
end

module Line_color_or_repeat_type = struct
  type t =
    [ Css_data_type.Color.t
    | Css_data_type.Integer.t
    ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
  ;;
end

module Auto_line_color_list_type = struct
  type t =
    [ Css_data_type.Color.t
    | Line_color_or_repeat_type.t
    | `Auto
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Line_color_or_repeat_type.t as t -> Line_color_or_repeat_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Line_color_list_type = struct
  type t = [ | Line_color_or_repeat_type.t ]

  let to_string_css = function
    | #Line_color_or_repeat_type.t as t -> Line_color_or_repeat_type.to_string_css t
  ;;
end

module Line_style_or_repeat_type = struct
  type t =
    [ Css_data_type.Integer.t
    | Line_style_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | #Line_style_type.t as t -> Line_style_type.to_string_css t
  ;;
end

module Auto_line_style_list_type = struct
  type t =
    [ Line_style_type.t
    | Line_style_or_repeat_type.t
    | `Auto
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Line_style_type.t as t -> Line_style_type.to_string_css t
    | #Line_style_or_repeat_type.t as t -> Line_style_or_repeat_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Line_style_list_type = struct
  type t = [ | Line_style_or_repeat_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Line_style_or_repeat_type.t as t -> Line_style_or_repeat_type.to_string_css t
  ;;
end

module Line_width_or_repeat_type = struct
  type t =
    [ Css_data_type.Integer.t
    | Line_width_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | #Line_width_type.t as t -> Line_width_type.to_string_css t
  ;;
end

module Auto_line_width_list_type = struct
  type t =
    [ Line_width_type.t
    | Line_width_or_repeat_type.t
    | `Auto
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Line_width_type.t as t -> Line_width_type.to_string_css t
    | #Line_width_or_repeat_type.t as t -> Line_width_or_repeat_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Line_width_list_type = struct
  type t = [ | Line_width_or_repeat_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Line_width_or_repeat_type.t as t -> Line_width_or_repeat_type.to_string_css t
  ;;
end

module Repetition_type = struct
  type t =
    [ `No_repeat
    | `Repeat
    | `Round
    | `Space
    ]

  let to_string_css = function
    | `No_repeat -> "no-repeat"
    | `Repeat -> "repeat"
    | `Round -> "round"
    | `Space -> "space"
  ;;
end

module Repeat_style_type = struct
  type t =
    [ Repetition_type.t
    | `Repeat_x
    | `Repeat_y
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Repetition_type.t as t -> Repetition_type.to_string_css t
    | `Repeat_x -> "repeat-x"
    | `Repeat_y -> "repeat-y"
  ;;
end

module Right_type = struct
  type t =
    [ Css_data_type.Length.t
    | `Auto
    ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Self_position_type = struct
  type t =
    [ `Center
    | `End
    | `Flex_end
    | `Flex_start
    | `Self_end
    | `Self_start
    | `Start
    ]

  let to_string_css = function
    | `Center -> "center"
    | `End -> "end"
    | `Flex_end -> "flex-end"
    | `Flex_start -> "flex-start"
    | `Self_end -> "self-end"
    | `Self_start -> "self-start"
    | `Start -> "start"
  ;;
end

module Single_animation_direction_type = struct
  type t =
    [ `Alternate
    | `Alternate_reverse
    | `Normal
    | `Reverse
    ]

  let to_string_css = function
    | `Alternate -> "alternate"
    | `Alternate_reverse -> "alternate-reverse"
    | `Normal -> "normal"
    | `Reverse -> "reverse"
  ;;
end

module Single_animation_fill_mode_type = struct
  type t =
    [ `Backwards
    | `Both
    | `Forwards
    | `None
    ]

  let to_string_css = function
    | `Backwards -> "backwards"
    | `Both -> "both"
    | `Forwards -> "forwards"
    | `None -> "none"
  ;;
end

module Single_animation_iteration_count_type = struct
  type t =
    [ Css_data_type.Number.t
    | `Infinite
    ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | `Infinite -> "infinite"
  ;;
end

module Single_animation_play_state_type = struct
  type t =
    [ `Paused
    | `Running
    ]

  let to_string_css = function
    | `Paused -> "paused"
    | `Running -> "running"
  ;;
end

module Single_animation_trigger_behavior_type = struct
  type t =
    [ `Alternate
    | `Once
    | `Repeat
    | `State
    ]

  let to_string_css = function
    | `Alternate -> "alternate"
    | `Once -> "once"
    | `Repeat -> "repeat"
    | `State -> "state"
  ;;
end

module Single_transition_property_type = struct
  type t =
    [ Css_data_type.Ident.Custom.t
    | `All
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Custom.t as t -> Css_data_type.Ident.Custom.to_string_css t
    | `All -> "all"
  ;;
end

module Spacing_trim_type = struct
  type t =
    [ `Normal
    | `Space_all
    | `Space_first
    | `Trim_all
    | `Trim_both
    | `Trim_start
    ]

  let to_string_css = function
    | `Normal -> "normal"
    | `Space_all -> "space-all"
    | `Space_first -> "space-first"
    | `Trim_all -> "trim-all"
    | `Trim_both -> "trim-both"
    | `Trim_start -> "trim-start"
  ;;
end

module Family_name_type = struct
  type t =
    [ Css_data_type.Ident.Custom.t
    | Css_data_type.Css_string.t
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Custom.t as t -> Css_data_type.Ident.Custom.to_string_css t
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
  ;;
end

module Keyframes_name_type = struct
  type t =
    [ Css_data_type.Ident.Custom.t
    | Css_data_type.Css_string.t
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Custom.t as t -> Css_data_type.Ident.Custom.to_string_css t
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
  ;;
end

module Opentype_tag_type = struct
  type t = [ | Css_data_type.Css_string.t ]

  let to_string_css = function
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
  ;;
end

module Syntax_type = struct
  type t =
    [ Css_data_type.Ident.t
    | Css_data_type.Css_string.t
    | `Angle_
    | `Color_
    | `Custom_ident_
    | `Image_
    | `Integer_
    | `Length_
    | `Length_percentage_
    | `Number_
    | `Percentage_
    | `Resolution_
    | `String_
    | `Time_
    | `Transform_function_
    | `Transform_list
    | `Url_
    ]

  let to_string_css = function
    | #Css_data_type.Ident.t as t -> Css_data_type.Ident.to_string_css t
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
    | `Angle_ -> "angle"
    | `Color_ -> "color"
    | `Custom_ident_ -> "custom-ident"
    | `Image_ -> "image"
    | `Integer_ -> "integer"
    | `Length_ -> "length"
    | `Length_percentage_ -> "length-percentage"
    | `Number_ -> "number"
    | `Percentage_ -> "percentage"
    | `Resolution_ -> "resolution"
    | `String_ -> "string"
    | `Time_ -> "time"
    | `Transform_function_ -> "transform-function"
    | `Transform_list -> "transform-list"
    | `Url_ -> "url"
  ;;
end

module Text_edge_type = struct
  type t =
    [ `Alphabetic
    | `Cap
    | `Ex
    | `Ideographic
    | `Ideographic_ink
    | `Text
    ]

  let to_string_css = function
    | `Alphabetic -> "alphabetic"
    | `Cap -> "cap"
    | `Ex -> "ex"
    | `Ideographic -> "ideographic"
    | `Ideographic_ink -> "ideographic-ink"
    | `Text -> "text"
  ;;
end

module Top_type = struct
  type t =
    [ Css_data_type.Length.t
    | `Auto
    ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Track_breadth_type = struct
  type t =
    [ Css_data_type.Flex_fr.t
    | Length_percentage_type.t
    | `Auto
    | `Max_content
    | `Min_content
    ]

  let to_string_css = function
    | #Css_data_type.Flex_fr.t as t -> Css_data_type.Flex_fr.to_string_css t
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
    | `Max_content -> "max-content"
    | `Min_content -> "min-content"
  ;;
end

module Auto_track_list_type = struct
  type t =
    [ Inflexible_breadth_type.t
    | Css_data_type.Integer.t
    | Length_percentage_type.t
    | Line_names_type.t
    | Track_breadth_type.t
    | `Auto_fill
    | `Auto_fit
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Inflexible_breadth_type.t as t -> Inflexible_breadth_type.to_string_css t
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | #Line_names_type.t as t -> Line_names_type.to_string_css t
    | #Track_breadth_type.t as t -> Track_breadth_type.to_string_css t
    | `Auto_fill -> "auto-fill"
    | `Auto_fit -> "auto-fit"
  ;;
end

module Track_size_type = struct
  type t =
    [ Inflexible_breadth_type.t
    | Length_percentage_type.t
    | Track_breadth_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Inflexible_breadth_type.t as t -> Inflexible_breadth_type.to_string_css t
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | #Track_breadth_type.t as t -> Track_breadth_type.to_string_css t
  ;;
end

module Track_list_type = struct
  type t =
    [ Css_data_type.Integer.t
    | Line_names_type.t
    | Track_size_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | #Line_names_type.t as t -> Line_names_type.to_string_css t
    | #Track_size_type.t as t -> Track_size_type.to_string_css t
  ;;
end

module Transition_behavior_value_type = struct
  type t =
    [ `Allow_discrete
    | `Normal
    ]

  let to_string_css = function
    | `Allow_discrete -> "allow-discrete"
    | `Normal -> "normal"
  ;;
end

module Shape_command_type = struct
  type t = [ `Close ]

  let to_string_css = function
    | `Close -> "close"
  ;;
end

module Visual_box_type = struct
  type t =
    [ `Border_box
    | `Content_box
    | `Padding_box
    ]

  let to_string_css = function
    | `Border_box -> "border-box"
    | `Content_box -> "content-box"
    | `Padding_box -> "padding-box"
  ;;
end

module Coord_box_type = struct
  type t =
    [ Visual_box_type.t
    | `Fill_box
    | `Stroke_box
    | `View_box
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Visual_box_type.t as t -> Visual_box_type.to_string_css t
    | `Fill_box -> "fill-box"
    | `Stroke_box -> "stroke-box"
    | `View_box -> "view-box"
  ;;
end

module Shape_box_type = struct
  type t =
    [ Visual_box_type.t
    | `Margin_box
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Visual_box_type.t as t -> Visual_box_type.to_string_css t
    | `Margin_box -> "margin-box"
  ;;
end

module Geometry_box_type = struct
  type t =
    [ Shape_box_type.t
    | `Fill_box
    | `Stroke_box
    | `View_box
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Shape_box_type.t as t -> Shape_box_type.to_string_css t
    | `Fill_box -> "fill-box"
    | `Stroke_box -> "stroke-box"
    | `View_box -> "view-box"
  ;;
end

module Zero_type = struct
  type t = [ `Zero ]

  let to_string_css = function
    | `Zero -> "0"
  ;;
end

module Conic_gradient_syntax_type = struct
  type t =
    [ Css_data_type.Angle.t
    | Css_data_type.Color.t
    | Color_interpolation_method_type.t
    | Css_data_type.Percentage.t
    | Position_type.t
    | Zero_type.t
    | `At
    | `From
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Angle.t as t -> Css_data_type.Angle.to_string_css t
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Color_interpolation_method_type.t as t ->
      Color_interpolation_method_type.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | #Position_type.t as t -> Position_type.to_string_css t
    | #Zero_type.t as t -> Zero_type.to_string_css t
    | `At -> "at"
    | `From -> "from"
  ;;
end

module Linear_gradient_syntax_type = struct
  type t =
    [ Css_data_type.Angle.t
    | Color_interpolation_method_type.t
    | Color_stop_list_type.t
    | Zero_type.t
    | `Bottom
    | `Left
    | `Right
    | `To
    | `Top
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Angle.t as t -> Css_data_type.Angle.to_string_css t
    | #Color_interpolation_method_type.t as t ->
      Color_interpolation_method_type.to_string_css t
    | #Color_stop_list_type.t as t -> Color_stop_list_type.to_string_css t
    | #Zero_type.t as t -> Zero_type.to_string_css t
    | `Bottom -> "bottom"
    | `Left -> "left"
    | `Right -> "right"
    | `To -> "to"
    | `Top -> "top"
  ;;
end

module Accent_color = struct
  type t =
    [ Css_data_type.Color.t
    | `Auto
    ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Align_content = struct
  type t =
    [ Baseline_position_type.t
    | Content_distribution_type.t
    | Content_position_type.t
    | Overflow_position_type.t
    | `Normal
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Baseline_position_type.t as t -> Baseline_position_type.to_string_css t
    | #Content_distribution_type.t as t -> Content_distribution_type.to_string_css t
    | #Content_position_type.t as t -> Content_position_type.to_string_css t
    | #Overflow_position_type.t as t -> Overflow_position_type.to_string_css t
    | `Normal -> "normal"
  ;;
end

module Align_items = struct
  type t =
    [ Baseline_position_type.t
    | Overflow_position_type.t
    | Self_position_type.t
    | `Anchor_center
    | `Normal
    | `Stretch
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Baseline_position_type.t as t -> Baseline_position_type.to_string_css t
    | #Overflow_position_type.t as t -> Overflow_position_type.to_string_css t
    | #Self_position_type.t as t -> Self_position_type.to_string_css t
    | `Anchor_center -> "anchor-center"
    | `Normal -> "normal"
    | `Stretch -> "stretch"
  ;;
end

module Align_self = struct
  type t =
    [ Baseline_position_type.t
    | Overflow_position_type.t
    | Self_position_type.t
    | `Anchor_center
    | `Auto
    | `Normal
    | `Stretch
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Baseline_position_type.t as t -> Baseline_position_type.to_string_css t
    | #Overflow_position_type.t as t -> Overflow_position_type.to_string_css t
    | #Self_position_type.t as t -> Self_position_type.to_string_css t
    | `Anchor_center -> "anchor-center"
    | `Auto -> "auto"
    | `Normal -> "normal"
    | `Stretch -> "stretch"
  ;;
end

module Alignment_baseline = struct
  type t =
    [ `Alphabetic
    | `Baseline
    | `Central
    | `Ideographic
    | `Mathematical
    | `Middle
    | `Text_bottom
    | `Text_top
    ]

  let to_string_css = function
    | `Alphabetic -> "alphabetic"
    | `Baseline -> "baseline"
    | `Central -> "central"
    | `Ideographic -> "ideographic"
    | `Mathematical -> "mathematical"
    | `Middle -> "middle"
    | `Text_bottom -> "text-bottom"
    | `Text_top -> "text-top"
  ;;
end

module All = struct
  type t =
    [ `Inherit
    | `Initial
    | `Revert
    | `Revert_layer
    | `Unset
    ]

  let to_string_css = function
    | `Inherit -> "inherit"
    | `Initial -> "initial"
    | `Revert -> "revert"
    | `Revert_layer -> "revert-layer"
    | `Unset -> "unset"
  ;;
end

module Anchor_name = struct
  type t =
    [ Css_data_type.Ident.Dashed.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Dashed.t as t -> Css_data_type.Ident.Dashed.to_string_css t
    | `None -> "none"
  ;;
end

module Anchor_scope = struct
  type t =
    [ Css_data_type.Ident.Dashed.t
    | `All
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Dashed.t as t -> Css_data_type.Ident.Dashed.to_string_css t
    | `All -> "all"
    | `None -> "none"
  ;;
end

module Animation_composition = struct
  type t =
    [ `Accumulate
    | `Add
    | `Replace
    ]

  let to_string_css = function
    | `Accumulate -> "accumulate"
    | `Add -> "add"
    | `Replace -> "replace"
  ;;
end

module Animation_delay = struct
  type t = [ | Css_data_type.Time.t ]

  let to_string_css = function
    | #Css_data_type.Time.t as t -> Css_data_type.Time.to_string_css t
  ;;
end

module Animation_direction = struct
  type t = [ | Single_animation_direction_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Single_animation_direction_type.t as t ->
      Single_animation_direction_type.to_string_css t
  ;;
end

module Animation_duration = struct
  type t =
    [ Css_data_type.Time.t
    | `Auto
    ]

  let to_string_css = function
    | #Css_data_type.Time.t as t -> Css_data_type.Time.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Animation_fill_mode = struct
  type t = [ | Single_animation_fill_mode_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Single_animation_fill_mode_type.t as t ->
      Single_animation_fill_mode_type.to_string_css t
  ;;
end

module Animation_iteration_count = struct
  type t = [ | Single_animation_iteration_count_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Single_animation_iteration_count_type.t as t ->
      Single_animation_iteration_count_type.to_string_css t
  ;;
end

module Animation_name = struct
  type t =
    [ Keyframes_name_type.t
    | `None
    ]

  let to_string_css = function
    | #Keyframes_name_type.t as t -> Keyframes_name_type.to_string_css t
    | `None -> "none"
  ;;
end

module Animation_play_state = struct
  type t = [ | Single_animation_play_state_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Single_animation_play_state_type.t as t ->
      Single_animation_play_state_type.to_string_css t
  ;;
end

module Animation_range_center = struct
  type t =
    [ Length_percentage_type.t
    | `Normal
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Normal -> "normal"
  ;;
end

module Animation_range_end = struct
  type t =
    [ Length_percentage_type.t
    | `Normal
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Normal -> "normal"
  ;;
end

module Animation_range_start = struct
  type t =
    [ Length_percentage_type.t
    | `Normal
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Normal -> "normal"
  ;;
end

module Animation_range = struct
  type t =
    [ Animation_range_end.t
    | Animation_range_start.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Animation_range_end.t as t -> Animation_range_end.to_string_css t
    | #Animation_range_start.t as t -> Animation_range_start.to_string_css t
  ;;
end

module Animation_trigger_behavior = struct
  type t = [ | Single_animation_trigger_behavior_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Single_animation_trigger_behavior_type.t as t ->
      Single_animation_trigger_behavior_type.to_string_css t
  ;;
end

module Animation_trigger_exit_range_end = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    | `Normal
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
    | `Normal -> "normal"
  ;;
end

module Animation_trigger_exit_range_start = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    | `Normal
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
    | `Normal -> "normal"
  ;;
end

module Animation_trigger_exit_range = struct
  type t =
    [ Animation_trigger_exit_range_end.t
    | Animation_trigger_exit_range_start.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Animation_trigger_exit_range_end.t as t ->
      Animation_trigger_exit_range_end.to_string_css t
    | #Animation_trigger_exit_range_start.t as t ->
      Animation_trigger_exit_range_start.to_string_css t
  ;;
end

module Animation_trigger_range_end = struct
  type t =
    [ Length_percentage_type.t
    | `Normal
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Normal -> "normal"
  ;;
end

module Animation_trigger_range_start = struct
  type t =
    [ Length_percentage_type.t
    | `Normal
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Normal -> "normal"
  ;;
end

module Animation_trigger_range = struct
  type t =
    [ Animation_trigger_range_end.t
    | Animation_trigger_range_start.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Animation_trigger_range_end.t as t -> Animation_trigger_range_end.to_string_css t
    | #Animation_trigger_range_start.t as t ->
      Animation_trigger_range_start.to_string_css t
  ;;
end

module Appearance = struct
  module Compat_auto_type = struct
    type t =
      [ `Button
      | `Checkbox
      | `Listbox
      | `Menulist
      | `Meter
      | `Progress_bar
      | `Radio
      | `Searchfield
      | `Textarea
      ]

    let to_string_css = function
      | `Button -> "button"
      | `Checkbox -> "checkbox"
      | `Listbox -> "listbox"
      | `Menulist -> "menulist"
      | `Meter -> "meter"
      | `Progress_bar -> "progress-bar"
      | `Radio -> "radio"
      | `Searchfield -> "searchfield"
      | `Textarea -> "textarea"
    ;;
  end

  module Compat_special_type = struct
    type t =
      [ `Menulist_button
      | `Textfield
      ]

    let to_string_css = function
      | `Menulist_button -> "menulist-button"
      | `Textfield -> "textfield"
    ;;
  end

  type t =
    [ Compat_auto_type.t
    | Compat_special_type.t
    | `Auto
    | `Base
    | `None
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Compat_auto_type.t as t -> Compat_auto_type.to_string_css t
    | #Compat_special_type.t as t -> Compat_special_type.to_string_css t
    | `Auto -> "auto"
    | `Base -> "base"
    | `None -> "none"
  ;;
end

module Aspect_ratio = struct
  type t =
    [ Ratio_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Ratio_type.t as t -> Ratio_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Backface_visibility = struct
  type t =
    [ `Hidden
    | `Visible
    ]

  let to_string_css = function
    | `Hidden -> "hidden"
    | `Visible -> "visible"
  ;;
end

module Background_attachment = struct
  type t = [ | Attachment_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Attachment_type.t as t -> Attachment_type.to_string_css t
  ;;
end

module Background_clip = struct
  type t =
    [ Visual_box_type.t
    | `Border_area
    | `Text
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Visual_box_type.t as t -> Visual_box_type.to_string_css t
    | `Border_area -> "border-area"
    | `Text -> "text"
  ;;
end

module Background_color = struct
  type t = [ | Css_data_type.Color.t ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
  ;;
end

module Background_origin = struct
  type t = [ | Visual_box_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Visual_box_type.t as t -> Visual_box_type.to_string_css t
  ;;
end

module Background_position = struct
  type t = [ | Bg_position_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Bg_position_type.t as t -> Bg_position_type.to_string_css t
  ;;
end

module Background_position_block = struct
  type t =
    [ Length_percentage_type.t
    | `Center
    | `End
    | `Start
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Center -> "center"
    | `End -> "end"
    | `Start -> "start"
  ;;
end

module Background_position_inline = struct
  type t =
    [ Length_percentage_type.t
    | `Center
    | `End
    | `Start
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Center -> "center"
    | `End -> "end"
    | `Start -> "start"
  ;;
end

module Background_position_x = struct
  type t =
    [ Length_percentage_type.t
    | `Center
    | `Left
    | `Right
    | `X_end
    | `X_start
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Center -> "center"
    | `Left -> "left"
    | `Right -> "right"
    | `X_end -> "x-end"
    | `X_start -> "x-start"
  ;;
end

module Background_position_y = struct
  type t =
    [ Length_percentage_type.t
    | `Bottom
    | `Center
    | `Top
    | `Y_end
    | `Y_start
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Bottom -> "bottom"
    | `Center -> "center"
    | `Top -> "top"
    | `Y_end -> "y-end"
    | `Y_start -> "y-start"
  ;;
end

module Background_repeat = struct
  type t = [ | Repeat_style_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Repeat_style_type.t as t -> Repeat_style_type.to_string_css t
  ;;
end

module Background_repeat_block = struct
  type t = [ | Repetition_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Repetition_type.t as t -> Repetition_type.to_string_css t
  ;;
end

module Background_repeat_inline = struct
  type t = [ | Repetition_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Repetition_type.t as t -> Repetition_type.to_string_css t
  ;;
end

module Background_repeat_x = struct
  type t = [ | Repetition_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Repetition_type.t as t -> Repetition_type.to_string_css t
  ;;
end

module Background_repeat_y = struct
  type t = [ | Repetition_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Repetition_type.t as t -> Repetition_type.to_string_css t
  ;;
end

module Background_size = struct
  type t = [ | Bg_size_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Bg_size_type.t as t -> Bg_size_type.to_string_css t
  ;;
end

module Baseline_shift = struct
  type t =
    [ Length_percentage_type.t
    | `Bottom
    | `Center
    | `Sub
    | `Super
    | `Top
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Bottom -> "bottom"
    | `Center -> "center"
    | `Sub -> "sub"
    | `Super -> "super"
    | `Top -> "top"
  ;;
end

module Baseline_source = struct
  type t =
    [ `Auto
    | `First
    | `Last
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `First -> "first"
    | `Last -> "last"
  ;;
end

module Block_ellipsis = struct
  type t =
    [ Css_data_type.Css_string.t
    | `Auto
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
    | `Auto -> "auto"
    | `None -> "none"
  ;;
end

module Block_step_align = struct
  type t =
    [ `Auto
    | `Center
    | `End
    | `Start
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Center -> "center"
    | `End -> "end"
    | `Start -> "start"
  ;;
end

module Block_step_insert = struct
  type t =
    [ `Content_box
    | `Margin_box
    | `Padding_box
    ]

  let to_string_css = function
    | `Content_box -> "content-box"
    | `Margin_box -> "margin-box"
    | `Padding_box -> "padding-box"
  ;;
end

module Block_step_round = struct
  type t =
    [ `Down
    | `Nearest
    | `Up
    ]

  let to_string_css = function
    | `Down -> "down"
    | `Nearest -> "nearest"
    | `Up -> "up"
  ;;
end

module Block_step_size = struct
  type t =
    [ Css_data_type.Length.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | `None -> "none"
  ;;
end

module Block_step = struct
  type t =
    [ Block_step_align.t
    | Block_step_insert.t
    | Block_step_round.t
    | Block_step_size.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Block_step_align.t as t -> Block_step_align.to_string_css t
    | #Block_step_insert.t as t -> Block_step_insert.to_string_css t
    | #Block_step_round.t as t -> Block_step_round.to_string_css t
    | #Block_step_size.t as t -> Block_step_size.to_string_css t
  ;;
end

module Bookmark_level = struct
  type t =
    [ Css_data_type.Integer.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | `None -> "none"
  ;;
end

module Bookmark_state = struct
  type t =
    [ `Closed
    | `Open
    ]

  let to_string_css = function
    | `Closed -> "closed"
    | `Open -> "open"
  ;;
end

module Border = struct
  type t =
    [ Css_data_type.Color.t
    | Line_style_type.t
    | Line_width_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Line_style_type.t as t -> Line_style_type.to_string_css t
    | #Line_width_type.t as t -> Line_width_type.to_string_css t
  ;;
end

module Border_block_end = struct
  type t =
    [ Css_data_type.Color.t
    | Line_style_type.t
    | Line_width_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Line_style_type.t as t -> Line_style_type.to_string_css t
    | #Line_width_type.t as t -> Line_width_type.to_string_css t
  ;;
end

module Border_block_end_radius = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Border_block_end_style = struct
  type t = [ | Line_style_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Line_style_type.t as t -> Line_style_type.to_string_css t
  ;;
end

module Border_block_end_width = struct
  type t = [ | Line_width_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Line_width_type.t as t -> Line_width_type.to_string_css t
  ;;
end

module Border_block_start = struct
  type t =
    [ Css_data_type.Color.t
    | Line_style_type.t
    | Line_width_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Line_style_type.t as t -> Line_style_type.to_string_css t
    | #Line_width_type.t as t -> Line_width_type.to_string_css t
  ;;
end

module Border_block = struct
  type t = [ | Border_block_start.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Border_block_start.t as t -> Border_block_start.to_string_css t
  ;;
end

module Border_block_start_radius = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Border_block_start_style = struct
  type t = [ | Line_style_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Line_style_type.t as t -> Line_style_type.to_string_css t
  ;;
end

module Border_block_start_width = struct
  type t = [ | Line_width_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Line_width_type.t as t -> Line_width_type.to_string_css t
  ;;
end

module Border_bottom = struct
  type t =
    [ Css_data_type.Color.t
    | Line_style_type.t
    | Line_width_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Line_style_type.t as t -> Line_style_type.to_string_css t
    | #Line_width_type.t as t -> Line_width_type.to_string_css t
  ;;
end

module Border_bottom_left_radius = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Border_bottom_radius = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Border_bottom_right_radius = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Border_bottom_style = struct
  type t = [ | Line_style_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Line_style_type.t as t -> Line_style_type.to_string_css t
  ;;
end

module Border_bottom_width = struct
  type t = [ | Line_width_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Line_width_type.t as t -> Line_width_type.to_string_css t
  ;;
end

module Border_boundary = struct
  type t =
    [ `Display
    | `None
    | `Parent
    ]

  let to_string_css = function
    | `Display -> "display"
    | `None -> "none"
    | `Parent -> "parent"
  ;;
end

module Border_clip = struct
  type t =
    [ Css_data_type.Flex_fr.t
    | Length_percentage_type.t
    | `Normal
    ]

  let to_string_css = function
    | #Css_data_type.Flex_fr.t as t -> Css_data_type.Flex_fr.to_string_css t
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Normal -> "normal"
  ;;
end

module Border_clip_bottom = struct
  type t =
    [ Css_data_type.Flex_fr.t
    | Length_percentage_type.t
    | `Normal
    ]

  let to_string_css = function
    | #Css_data_type.Flex_fr.t as t -> Css_data_type.Flex_fr.to_string_css t
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Normal -> "normal"
  ;;
end

module Border_clip_left = struct
  type t =
    [ Css_data_type.Flex_fr.t
    | Length_percentage_type.t
    | `Normal
    ]

  let to_string_css = function
    | #Css_data_type.Flex_fr.t as t -> Css_data_type.Flex_fr.to_string_css t
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Normal -> "normal"
  ;;
end

module Border_clip_right = struct
  type t =
    [ Css_data_type.Flex_fr.t
    | Length_percentage_type.t
    | `Normal
    ]

  let to_string_css = function
    | #Css_data_type.Flex_fr.t as t -> Css_data_type.Flex_fr.to_string_css t
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Normal -> "normal"
  ;;
end

module Border_clip_top = struct
  type t =
    [ Css_data_type.Flex_fr.t
    | Length_percentage_type.t
    | `Normal
    ]

  let to_string_css = function
    | #Css_data_type.Flex_fr.t as t -> Css_data_type.Flex_fr.to_string_css t
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Normal -> "normal"
  ;;
end

module Border_collapse = struct
  type t =
    [ `Collapse
    | `Separate
    ]

  let to_string_css = function
    | `Collapse -> "collapse"
    | `Separate -> "separate"
  ;;
end

module Border_end_end_radius = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Border_end_start_radius = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Border_image_outset = struct
  type t =
    [ Css_data_type.Length.t
    | Css_data_type.Number.t
    ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
  ;;
end

module Border_image_repeat = struct
  type t =
    [ `Repeat
    | `Round
    | `Space
    | `Stretch
    ]

  let to_string_css = function
    | `Repeat -> "repeat"
    | `Round -> "round"
    | `Space -> "space"
    | `Stretch -> "stretch"
  ;;
end

module Border_image_slice = struct
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `Fill
    ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `Fill -> "fill"
  ;;
end

module Border_image_width = struct
  type t =
    [ Length_percentage_type.t
    | Css_data_type.Number.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Border_inline = struct
  type t = [ | Border_block_start.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Border_block_start.t as t -> Border_block_start.to_string_css t
  ;;
end

module Border_inline_end = struct
  type t =
    [ Css_data_type.Color.t
    | Line_style_type.t
    | Line_width_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Line_style_type.t as t -> Line_style_type.to_string_css t
    | #Line_width_type.t as t -> Line_width_type.to_string_css t
  ;;
end

module Border_inline_end_radius = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Border_inline_end_style = struct
  type t = [ | Line_style_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Line_style_type.t as t -> Line_style_type.to_string_css t
  ;;
end

module Border_inline_end_width = struct
  type t = [ | Line_width_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Line_width_type.t as t -> Line_width_type.to_string_css t
  ;;
end

module Border_inline_start = struct
  type t =
    [ Css_data_type.Color.t
    | Line_style_type.t
    | Line_width_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Line_style_type.t as t -> Line_style_type.to_string_css t
    | #Line_width_type.t as t -> Line_width_type.to_string_css t
  ;;
end

module Border_inline_start_radius = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Border_inline_start_style = struct
  type t = [ | Line_style_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Line_style_type.t as t -> Line_style_type.to_string_css t
  ;;
end

module Border_inline_start_width = struct
  type t = [ | Line_width_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Line_width_type.t as t -> Line_width_type.to_string_css t
  ;;
end

module Border_left = struct
  type t =
    [ Css_data_type.Color.t
    | Line_style_type.t
    | Line_width_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Line_style_type.t as t -> Line_style_type.to_string_css t
    | #Line_width_type.t as t -> Line_width_type.to_string_css t
  ;;
end

module Border_left_radius = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Border_left_style = struct
  type t = [ | Line_style_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Line_style_type.t as t -> Line_style_type.to_string_css t
  ;;
end

module Border_left_width = struct
  type t = [ | Line_width_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Line_width_type.t as t -> Line_width_type.to_string_css t
  ;;
end

module Border_limit = struct
  type t =
    [ Length_percentage_type.t
    | `All
    | `Bottom
    | `Corners
    | `Left
    | `Right
    | `Sides
    | `Top
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `All -> "all"
    | `Bottom -> "bottom"
    | `Corners -> "corners"
    | `Left -> "left"
    | `Right -> "right"
    | `Sides -> "sides"
    | `Top -> "top"
  ;;
end

module Border_radius = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Border_right = struct
  type t =
    [ Css_data_type.Color.t
    | Line_style_type.t
    | Line_width_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Line_style_type.t as t -> Line_style_type.to_string_css t
    | #Line_width_type.t as t -> Line_width_type.to_string_css t
  ;;
end

module Border_right_radius = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Border_right_style = struct
  type t = [ | Line_style_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Line_style_type.t as t -> Line_style_type.to_string_css t
  ;;
end

module Border_right_width = struct
  type t = [ | Line_width_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Line_width_type.t as t -> Line_width_type.to_string_css t
  ;;
end

module Border_spacing = struct
  type t = [ | Css_data_type.Length.t ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
  ;;
end

module Border_start_end_radius = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Border_start_start_radius = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Border_style = struct
  type t = [ | Line_style_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Line_style_type.t as t -> Line_style_type.to_string_css t
  ;;
end

module Border_top = struct
  type t =
    [ Css_data_type.Color.t
    | Line_style_type.t
    | Line_width_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Line_style_type.t as t -> Line_style_type.to_string_css t
    | #Line_width_type.t as t -> Line_width_type.to_string_css t
  ;;
end

module Border_top_left_radius = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Border_top_radius = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Border_top_right_radius = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Border_top_style = struct
  type t = [ | Line_style_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Line_style_type.t as t -> Line_style_type.to_string_css t
  ;;
end

module Border_block_style = struct
  type t = [ | Border_top_style.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Border_top_style.t as t -> Border_top_style.to_string_css t
  ;;
end

module Border_inline_style = struct
  type t = [ | Border_top_style.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Border_top_style.t as t -> Border_top_style.to_string_css t
  ;;
end

module Border_top_width = struct
  type t = [ | Line_width_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Line_width_type.t as t -> Line_width_type.to_string_css t
  ;;
end

module Border_block_width = struct
  type t = [ | Border_top_width.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Border_top_width.t as t -> Border_top_width.to_string_css t
  ;;
end

module Border_inline_width = struct
  type t = [ | Border_top_width.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Border_top_width.t as t -> Border_top_width.to_string_css t
  ;;
end

module Border_width = struct
  type t = [ | Line_width_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Line_width_type.t as t -> Line_width_type.to_string_css t
  ;;
end

module Box_decoration_break = struct
  type t =
    [ `Clone
    | `Slice
    ]

  let to_string_css = function
    | `Clone -> "clone"
    | `Slice -> "slice"
  ;;
end

module Box_shadow_blur = struct
  type t = [ | Css_data_type.Length.t ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
  ;;
end

module Box_shadow_color = struct
  type t = [ | Css_data_type.Color.t ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
  ;;
end

module Box_shadow_offset = struct
  type t =
    [ Css_data_type.Length.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | `None -> "none"
  ;;
end

module Box_shadow_position = struct
  type t =
    [ `Inset
    | `Outset
    ]

  let to_string_css = function
    | `Inset -> "inset"
    | `Outset -> "outset"
  ;;
end

module Box_shadow_spread = struct
  type t = [ | Css_data_type.Length.t ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
  ;;
end

module Box_shadow = struct
  type t =
    [ Box_shadow_blur.t
    | Box_shadow_color.t
    | Box_shadow_offset.t
    | Box_shadow_position.t
    | Box_shadow_spread.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Box_shadow_blur.t as t -> Box_shadow_blur.to_string_css t
    | #Box_shadow_color.t as t -> Box_shadow_color.to_string_css t
    | #Box_shadow_offset.t as t -> Box_shadow_offset.to_string_css t
    | #Box_shadow_position.t as t -> Box_shadow_position.to_string_css t
    | #Box_shadow_spread.t as t -> Box_shadow_spread.to_string_css t
  ;;
end

module Box_sizing = struct
  type t =
    [ `Border_box
    | `Content_box
    ]

  let to_string_css = function
    | `Border_box -> "border-box"
    | `Content_box -> "content-box"
  ;;
end

module Box_snap = struct
  type t =
    [ `Baseline
    | `Block_end
    | `Block_start
    | `Center
    | `Last_baseline
    | `None
    ]

  let to_string_css = function
    | `Baseline -> "baseline"
    | `Block_end -> "block-end"
    | `Block_start -> "block-start"
    | `Center -> "center"
    | `Last_baseline -> "last-baseline"
    | `None -> "none"
  ;;
end

module Break_after = struct
  type t =
    [ `All
    | `Always
    | `Auto
    | `Avoid
    | `Avoid_column
    | `Avoid_page
    | `Avoid_region
    | `Column
    | `Left
    | `Page
    | `Recto
    | `Region
    | `Right
    | `Verso
    ]

  let to_string_css = function
    | `All -> "all"
    | `Always -> "always"
    | `Auto -> "auto"
    | `Avoid -> "avoid"
    | `Avoid_column -> "avoid-column"
    | `Avoid_page -> "avoid-page"
    | `Avoid_region -> "avoid-region"
    | `Column -> "column"
    | `Left -> "left"
    | `Page -> "page"
    | `Recto -> "recto"
    | `Region -> "region"
    | `Right -> "right"
    | `Verso -> "verso"
  ;;
end

module Break_before = struct
  type t =
    [ `All
    | `Always
    | `Auto
    | `Avoid
    | `Avoid_column
    | `Avoid_page
    | `Avoid_region
    | `Column
    | `Left
    | `Page
    | `Recto
    | `Region
    | `Right
    | `Verso
    ]

  let to_string_css = function
    | `All -> "all"
    | `Always -> "always"
    | `Auto -> "auto"
    | `Avoid -> "avoid"
    | `Avoid_column -> "avoid-column"
    | `Avoid_page -> "avoid-page"
    | `Avoid_region -> "avoid-region"
    | `Column -> "column"
    | `Left -> "left"
    | `Page -> "page"
    | `Recto -> "recto"
    | `Region -> "region"
    | `Right -> "right"
    | `Verso -> "verso"
  ;;
end

module Break_inside = struct
  type t =
    [ `Auto
    | `Avoid
    | `Avoid_column
    | `Avoid_page
    | `Avoid_region
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Avoid -> "avoid"
    | `Avoid_column -> "avoid-column"
    | `Avoid_page -> "avoid-page"
    | `Avoid_region -> "avoid-region"
  ;;
end

module Caption_side = struct
  type t =
    [ `Bottom
    | `Top
    ]

  let to_string_css = function
    | `Bottom -> "bottom"
    | `Top -> "top"
  ;;
end

module Caret_animation = struct
  type t =
    [ `Auto
    | `Manual
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Manual -> "manual"
  ;;
end

module Caret_color = struct
  type t =
    [ Css_data_type.Color.t
    | `Auto
    ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Caret_shape = struct
  type t =
    [ `Auto
    | `Bar
    | `Block
    | `Underscore
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Bar -> "bar"
    | `Block -> "block"
    | `Underscore -> "underscore"
  ;;
end

module Caret = struct
  type t =
    [ Caret_animation.t
    | Caret_color.t
    | Caret_shape.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Caret_animation.t as t -> Caret_animation.to_string_css t
    | #Caret_color.t as t -> Caret_color.to_string_css t
    | #Caret_shape.t as t -> Caret_shape.to_string_css t
  ;;
end

module Clear = struct
  type t =
    [ `Block_end
    | `Block_start
    | `Both
    | `Both_block
    | `Both_inline
    | `Bottom
    | `Inline_end
    | `Inline_start
    | `Left
    | `None
    | `Right
    | `Top
    ]

  let to_string_css = function
    | `Block_end -> "block-end"
    | `Block_start -> "block-start"
    | `Both -> "both"
    | `Both_block -> "both-block"
    | `Both_inline -> "both-inline"
    | `Bottom -> "bottom"
    | `Inline_end -> "inline-end"
    | `Inline_start -> "inline-start"
    | `Left -> "left"
    | `None -> "none"
    | `Right -> "right"
    | `Top -> "top"
  ;;
end

module Clip_rule = struct
  type t =
    [ `Evenodd
    | `Nonzero
    ]

  let to_string_css = function
    | `Evenodd -> "evenodd"
    | `Nonzero -> "nonzero"
  ;;
end

module Color_interpolation = struct
  type t =
    [ `Auto
    | `LinearRGB
    | `Srgb
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `LinearRGB -> "linearRGB"
    | `Srgb -> "sRGB"
  ;;
end

module Color_interpolation_filters = struct
  type t =
    [ `Auto
    | `LinearRGB
    | `Srgb
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `LinearRGB -> "linearRGB"
    | `Srgb -> "sRGB"
  ;;
end

module Color_scheme = struct
  type t =
    [ Css_data_type.Ident.Custom.t
    | `Dark
    | `Light
    | `Normal
    | `Only
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Custom.t as t -> Css_data_type.Ident.Custom.to_string_css t
    | `Dark -> "dark"
    | `Light -> "light"
    | `Normal -> "normal"
    | `Only -> "only"
  ;;
end

module Column_count = struct
  type t =
    [ Css_data_type.Integer.t
    | `Auto
    ]

  let to_string_css = function
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Column_fill = struct
  type t =
    [ `Auto
    | `Balance
    | `Balance_all
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Balance -> "balance"
    | `Balance_all -> "balance-all"
  ;;
end

module Column_gap = struct
  type t =
    [ Length_percentage_type.t
    | `Normal
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Normal -> "normal"
  ;;
end

module Column_height = struct
  type t =
    [ Css_data_type.Length.t
    | `Auto
    ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Column_rule = struct
  type t =
    [ Gap_auto_rule_list_type.t
    | Gap_rule_list_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Gap_auto_rule_list_type.t as t -> Gap_auto_rule_list_type.to_string_css t
    | #Gap_rule_list_type.t as t -> Gap_rule_list_type.to_string_css t
  ;;
end

module Column_rule_break = struct
  type t =
    [ `Intersection
    | `None
    | `Spanning_item
    ]

  let to_string_css = function
    | `Intersection -> "intersection"
    | `None -> "none"
    | `Spanning_item -> "spanning-item"
  ;;
end

module Column_rule_color = struct
  type t =
    [ Auto_line_color_list_type.t
    | Line_color_list_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Auto_line_color_list_type.t as t -> Auto_line_color_list_type.to_string_css t
    | #Line_color_list_type.t as t -> Line_color_list_type.to_string_css t
  ;;
end

module Column_rule_outset = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Column_rule_style = struct
  type t =
    [ Auto_line_style_list_type.t
    | Line_style_list_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Auto_line_style_list_type.t as t -> Auto_line_style_list_type.to_string_css t
    | #Line_style_list_type.t as t -> Line_style_list_type.to_string_css t
  ;;
end

module Column_rule_width = struct
  type t =
    [ Auto_line_width_list_type.t
    | Line_width_list_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Auto_line_width_list_type.t as t -> Auto_line_width_list_type.to_string_css t
    | #Line_width_list_type.t as t -> Line_width_list_type.to_string_css t
  ;;
end

module Column_span = struct
  type t =
    [ Css_data_type.Integer.t
    | `All
    | `Auto
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | `All -> "all"
    | `Auto -> "auto"
    | `None -> "none"
  ;;
end

module Column_width = struct
  type t =
    [ Css_data_type.Length.t
    | Length_percentage_type.t
    | `Auto
    | `Max_content
    | `Min_content
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
    | `Max_content -> "max-content"
    | `Min_content -> "min-content"
  ;;
end

module Column_wrap = struct
  type t =
    [ `Auto
    | `Nowrap
    | `Wrap
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Nowrap -> "nowrap"
    | `Wrap -> "wrap"
  ;;
end

module Columns = struct
  type t =
    [ Column_count.t
    | Column_height.t
    | Column_width.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Column_count.t as t -> Column_count.to_string_css t
    | #Column_height.t as t -> Column_height.to_string_css t
    | #Column_width.t as t -> Column_width.to_string_css t
  ;;
end

module Contain = struct
  type t =
    [ `Content
    | `Inline_size
    | `Layout
    | `None
    | `Paint
    | `Size
    | `Strict
    | `Style
    ]

  let to_string_css = function
    | `Content -> "content"
    | `Inline_size -> "inline-size"
    | `Layout -> "layout"
    | `None -> "none"
    | `Paint -> "paint"
    | `Size -> "size"
    | `Strict -> "strict"
    | `Style -> "style"
  ;;
end

module Contain_intrinsic_block_size = struct
  type t =
    [ Css_data_type.Length.t
    | `Auto
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | `Auto -> "auto"
    | `None -> "none"
  ;;
end

module Contain_intrinsic_height = struct
  type t =
    [ Css_data_type.Length.t
    | `Auto
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | `Auto -> "auto"
    | `None -> "none"
  ;;
end

module Contain_intrinsic_inline_size = struct
  type t =
    [ Css_data_type.Length.t
    | `Auto
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | `Auto -> "auto"
    | `None -> "none"
  ;;
end

module Contain_intrinsic_size = struct
  type t =
    [ Css_data_type.Length.t
    | `Auto
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | `Auto -> "auto"
    | `None -> "none"
  ;;
end

module Contain_intrinsic_width = struct
  type t =
    [ Css_data_type.Length.t
    | `Auto
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | `Auto -> "auto"
    | `None -> "none"
  ;;
end

module Container_name = struct
  type t =
    [ Css_data_type.Ident.Custom.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Custom.t as t -> Css_data_type.Ident.Custom.to_string_css t
    | `None -> "none"
  ;;
end

module Container_type = struct
  type t =
    [ `Inline_size
    | `Normal
    | `Scroll_state
    | `Size
    ]

  let to_string_css = function
    | `Inline_size -> "inline-size"
    | `Normal -> "normal"
    | `Scroll_state -> "scroll-state"
    | `Size -> "size"
  ;;
end

module Container = struct
  type t =
    [ Container_name.t
    | Container_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Container_name.t as t -> Container_name.to_string_css t
    | #Container_type.t as t -> Container_type.to_string_css t
  ;;
end

module Content_visibility = struct
  type t =
    [ `Auto
    | `Hidden
    | `Visible
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Hidden -> "hidden"
    | `Visible -> "visible"
  ;;
end

module Continue = struct
  type t =
    [ `Auto
    | `Collapse
    | `Discard
    | `Fragments
    | `Overflow
    | `Paginate
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Collapse -> "collapse"
    | `Discard -> "discard"
    | `Fragments -> "fragments"
    | `Overflow -> "overflow"
    | `Paginate -> "paginate"
  ;;
end

module Counter_increment = struct
  type t =
    [ Css_data_type.Integer.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | `None -> "none"
  ;;
end

module Counter_reset = struct
  type t =
    [ Css_data_type.Integer.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | `None -> "none"
  ;;
end

module Counter_set = struct
  type t =
    [ Css_data_type.Integer.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | `None -> "none"
  ;;
end

module Cue_after = struct
  type t =
    [ Css_data_type.Decibel.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Decibel.t as t -> Css_data_type.Decibel.to_string_css t
    | `None -> "none"
  ;;
end

module Cue_before = struct
  type t =
    [ Css_data_type.Decibel.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Decibel.t as t -> Css_data_type.Decibel.to_string_css t
    | `None -> "none"
  ;;
end

module Cue = struct
  type t =
    [ Cue_after.t
    | Cue_before.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Cue_after.t as t -> Cue_after.to_string_css t
    | #Cue_before.t as t -> Cue_before.to_string_css t
  ;;
end

module Cx = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Cy = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module D = struct
  type t =
    [ Css_data_type.Css_string.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
    | `None -> "none"
  ;;
end

module Direction = struct
  type t =
    [ `Ltr
    | `Rtl
    ]

  let to_string_css = function
    | `Ltr -> "ltr"
    | `Rtl -> "rtl"
  ;;
end

module Display = struct
  type t =
    [ `Block
    | `Contents
    | `Flex
    | `Flow
    | `Flow_root
    | `Grid
    | `Inline
    | `Inline_block
    | `Inline_flex
    | `Inline_grid
    | `Inline_table
    | `List_item
    | `Math
    | `None
    | `Ruby
    | `Ruby_base
    | `Ruby_base_container
    | `Ruby_text
    | `Ruby_text_container
    | `Run_in
    | `Table
    | `Table_caption
    | `Table_cell
    | `Table_column
    | `Table_column_group
    | `Table_footer_group
    | `Table_header_group
    | `Table_row
    | `Table_row_group
    ]

  let to_string_css = function
    | `Block -> "block"
    | `Contents -> "contents"
    | `Flex -> "flex"
    | `Flow -> "flow"
    | `Flow_root -> "flow-root"
    | `Grid -> "grid"
    | `Inline -> "inline"
    | `Inline_block -> "inline-block"
    | `Inline_flex -> "inline-flex"
    | `Inline_grid -> "inline-grid"
    | `Inline_table -> "inline-table"
    | `List_item -> "list-item"
    | `Math -> "math"
    | `None -> "none"
    | `Ruby -> "ruby"
    | `Ruby_base -> "ruby-base"
    | `Ruby_base_container -> "ruby-base-container"
    | `Ruby_text -> "ruby-text"
    | `Ruby_text_container -> "ruby-text-container"
    | `Run_in -> "run-in"
    | `Table -> "table"
    | `Table_caption -> "table-caption"
    | `Table_cell -> "table-cell"
    | `Table_column -> "table-column"
    | `Table_column_group -> "table-column-group"
    | `Table_footer_group -> "table-footer-group"
    | `Table_header_group -> "table-header-group"
    | `Table_row -> "table-row"
    | `Table_row_group -> "table-row-group"
  ;;
end

module Dominant_baseline = struct
  type t =
    [ `Alphabetic
    | `Auto
    | `Central
    | `Hanging
    | `Ideographic
    | `Mathematical
    | `Middle
    | `Text_bottom
    | `Text_top
    ]

  let to_string_css = function
    | `Alphabetic -> "alphabetic"
    | `Auto -> "auto"
    | `Central -> "central"
    | `Hanging -> "hanging"
    | `Ideographic -> "ideographic"
    | `Mathematical -> "mathematical"
    | `Middle -> "middle"
    | `Text_bottom -> "text-bottom"
    | `Text_top -> "text-top"
  ;;
end

module Empty_cells = struct
  type t =
    [ `Hide
    | `Show
    ]

  let to_string_css = function
    | `Hide -> "hide"
    | `Show -> "show"
  ;;
end

module Field_sizing = struct
  type t =
    [ `Content
    | `Fixed
    ]

  let to_string_css = function
    | `Content -> "content"
    | `Fixed -> "fixed"
  ;;
end

module Fill_break = struct
  type t =
    [ `Bounding_box
    | `Clone
    | `Slice
    ]

  let to_string_css = function
    | `Bounding_box -> "bounding-box"
    | `Clone -> "clone"
    | `Slice -> "slice"
  ;;
end

module Fill_color = struct
  type t = [ | Css_data_type.Color.t ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
  ;;
end

module Fill_origin = struct
  type t =
    [ `Border_box
    | `Content_box
    | `Fill_box
    | `Match_parent
    | `Padding_box
    | `Stroke_box
    ]

  let to_string_css = function
    | `Border_box -> "border-box"
    | `Content_box -> "content-box"
    | `Fill_box -> "fill-box"
    | `Match_parent -> "match-parent"
    | `Padding_box -> "padding-box"
    | `Stroke_box -> "stroke-box"
  ;;
end

module Fill_position = struct
  type t = [ | Position_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Position_type.t as t -> Position_type.to_string_css t
  ;;
end

module Fill_repeat = struct
  type t = [ | Repeat_style_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Repeat_style_type.t as t -> Repeat_style_type.to_string_css t
  ;;
end

module Fill_rule = struct
  type t =
    [ `Evenodd
    | `Nonzero
    ]

  let to_string_css = function
    | `Evenodd -> "evenodd"
    | `Nonzero -> "nonzero"
  ;;
end

module Fill_size = struct
  type t = [ | Bg_size_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Bg_size_type.t as t -> Bg_size_type.to_string_css t
  ;;
end

module Flex_direction = struct
  type t =
    [ `Column
    | `Column_reverse
    | `Row
    | `Row_reverse
    ]

  let to_string_css = function
    | `Column -> "column"
    | `Column_reverse -> "column-reverse"
    | `Row -> "row"
    | `Row_reverse -> "row-reverse"
  ;;
end

module Flex_grow = struct
  type t = [ | Css_data_type.Number.t ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
  ;;
end

module Flex_shrink = struct
  type t = [ | Css_data_type.Number.t ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
  ;;
end

module Flex_wrap = struct
  type t =
    [ `Nowrap
    | `Wrap
    | `Wrap_reverse
    ]

  let to_string_css = function
    | `Nowrap -> "nowrap"
    | `Wrap -> "wrap"
    | `Wrap_reverse -> "wrap-reverse"
  ;;
end

module Flex_flow = struct
  type t =
    [ Flex_direction.t
    | Flex_wrap.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Flex_direction.t as t -> Flex_direction.to_string_css t
    | #Flex_wrap.t as t -> Flex_wrap.to_string_css t
  ;;
end

module Float_defer = struct
  type t =
    [ Css_data_type.Integer.t
    | `Last
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | `Last -> "last"
    | `None -> "none"
  ;;
end

module Float_offset = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Float_reference = struct
  type t =
    [ `Column
    | `Inline
    | `Page
    | `Region
    ]

  let to_string_css = function
    | `Column -> "column"
    | `Inline -> "inline"
    | `Page -> "page"
    | `Region -> "region"
  ;;
end

module Flood_color = struct
  type t = [ | Css_data_type.Color.t ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
  ;;
end

module Flow_from = struct
  type t =
    [ Css_data_type.Ident.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Ident.t as t -> Css_data_type.Ident.to_string_css t
    | `None -> "none"
  ;;
end

module Flow_into = struct
  type t =
    [ Css_data_type.Ident.t
    | `Content
    | `Element
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Ident.t as t -> Css_data_type.Ident.to_string_css t
    | `Content -> "content"
    | `Element -> "element"
    | `None -> "none"
  ;;
end

module Font_family = struct
  type t =
    [ Family_name_type.t
    | `Cursive
    | `Fangsong
    | `Fantasy
    | `Kai
    | `Khmer_mul
    | `Math
    | `Monospace
    | `Nastaliq
    | `Sans_serif
    | `Serif
    | `System_ui
    | `Ui_monospace
    | `Ui_rounded
    | `Ui_sans_serif
    | `Ui_serif
    ]

  let to_string_css = function
    | #Family_name_type.t as t -> Family_name_type.to_string_css t
    | `Cursive -> "cursive"
    | `Fangsong -> "fangsong"
    | `Fantasy -> "fantasy"
    | `Kai -> "kai"
    | `Khmer_mul -> "khmer-mul"
    | `Math -> "math"
    | `Monospace -> "monospace"
    | `Nastaliq -> "nastaliq"
    | `Sans_serif -> "sans-serif"
    | `Serif -> "serif"
    | `System_ui -> "system-ui"
    | `Ui_monospace -> "ui-monospace"
    | `Ui_rounded -> "ui-rounded"
    | `Ui_sans_serif -> "ui-sans-serif"
    | `Ui_serif -> "ui-serif"
  ;;
end

module Font_feature_settings = struct
  type t =
    [ Css_data_type.Integer.t
    | Opentype_tag_type.t
    | `Normal
    | `Off
    | `On
    ]

  let to_string_css = function
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | #Opentype_tag_type.t as t -> Opentype_tag_type.to_string_css t
    | `Normal -> "normal"
    | `Off -> "off"
    | `On -> "on"
  ;;
end

module Font_kerning = struct
  type t =
    [ `Auto
    | `None
    | `Normal
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `None -> "none"
    | `Normal -> "normal"
  ;;
end

module Font_language_override = struct
  type t =
    [ Css_data_type.Css_string.t
    | `Normal
    ]

  let to_string_css = function
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
    | `Normal -> "normal"
  ;;
end

module Font_optical_sizing = struct
  type t =
    [ `Auto
    | `None
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `None -> "none"
  ;;
end

module Font_size = struct
  type t =
    [ Length_percentage_type.t
    | `Large
    | `Larger
    | `Math
    | `Medium
    | `Small
    | `Smaller
    | `X_large
    | `X_small
    | `Xx_large
    | `Xx_small
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Large -> "large"
    | `Larger -> "larger"
    | `Math -> "math"
    | `Medium -> "medium"
    | `Small -> "small"
    | `Smaller -> "smaller"
    | `X_large -> "x-large"
    | `X_small -> "x-small"
    | `Xx_large -> "xx-large"
    | `Xx_small -> "xx-small"
  ;;
end

module Font_size_adjust = struct
  type t =
    [ Css_data_type.Number.t
    | `Cap_height
    | `Ch_width
    | `Ex_height
    | `From_font
    | `Ic_height
    | `Ic_width
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | `Cap_height -> "cap-height"
    | `Ch_width -> "ch-width"
    | `Ex_height -> "ex-height"
    | `From_font -> "from-font"
    | `Ic_height -> "ic-height"
    | `Ic_width -> "ic-width"
    | `None -> "none"
  ;;
end

module Font_stretch = struct
  type t =
    [ Css_data_type.Percentage.t
    | `Condensed
    | `Expanded
    | `Extra_condensed
    | `Extra_expanded
    | `Normal
    | `Semi_condensed
    | `Semi_expanded
    | `Ultra_condensed
    | `Ultra_expanded
    ]

  let to_string_css = function
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `Condensed -> "condensed"
    | `Expanded -> "expanded"
    | `Extra_condensed -> "extra-condensed"
    | `Extra_expanded -> "extra-expanded"
    | `Normal -> "normal"
    | `Semi_condensed -> "semi-condensed"
    | `Semi_expanded -> "semi-expanded"
    | `Ultra_condensed -> "ultra-condensed"
    | `Ultra_expanded -> "ultra-expanded"
  ;;
end

module Font_style = struct
  type t =
    [ Css_data_type.Angle.t
    | `Italic
    | `Left
    | `Normal
    | `Oblique
    | `Right
    ]

  let to_string_css = function
    | #Css_data_type.Angle.t as t -> Css_data_type.Angle.to_string_css t
    | `Italic -> "italic"
    | `Left -> "left"
    | `Normal -> "normal"
    | `Oblique -> "oblique"
    | `Right -> "right"
  ;;
end

module Font_synthesis = struct
  type t =
    [ `None
    | `Position
    | `Small_caps
    | `Style
    | `Weight
    ]

  let to_string_css = function
    | `None -> "none"
    | `Position -> "position"
    | `Small_caps -> "small-caps"
    | `Style -> "style"
    | `Weight -> "weight"
  ;;
end

module Font_synthesis_position = struct
  type t =
    [ `Auto
    | `None
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `None -> "none"
  ;;
end

module Font_synthesis_small_caps = struct
  type t =
    [ `Auto
    | `None
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `None -> "none"
  ;;
end

module Font_synthesis_style = struct
  type t =
    [ `Auto
    | `None
    | `Oblique_only
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `None -> "none"
    | `Oblique_only -> "oblique-only"
  ;;
end

module Font_synthesis_weight = struct
  type t =
    [ `Auto
    | `None
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `None -> "none"
  ;;
end

module Font_variant = struct
  type t =
    [ Common_lig_values_type.t
    | Contextual_alt_values_type.t
    | Discretionary_lig_values_type.t
    | East_asian_variant_values_type.t
    | East_asian_width_values_type.t
    | Feature_value_name_type.t
    | Historical_lig_values_type.t
    | Numeric_figure_values_type.t
    | Numeric_fraction_values_type.t
    | Numeric_spacing_values_type.t
    | `All_petite_caps
    | `All_small_caps
    | `Emoji
    | `Historical_forms
    | `None
    | `Normal
    | `Ordinal
    | `Petite_caps
    | `Ruby
    | `Slashed_zero
    | `Small_caps
    | `Sub
    | `Super
    | `Text
    | `Titling_caps
    | `Unicase
    | `Unicode
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Common_lig_values_type.t as t -> Common_lig_values_type.to_string_css t
    | #Contextual_alt_values_type.t as t -> Contextual_alt_values_type.to_string_css t
    | #Discretionary_lig_values_type.t as t ->
      Discretionary_lig_values_type.to_string_css t
    | #East_asian_variant_values_type.t as t ->
      East_asian_variant_values_type.to_string_css t
    | #East_asian_width_values_type.t as t -> East_asian_width_values_type.to_string_css t
    | #Feature_value_name_type.t as t -> Feature_value_name_type.to_string_css t
    | #Historical_lig_values_type.t as t -> Historical_lig_values_type.to_string_css t
    | #Numeric_figure_values_type.t as t -> Numeric_figure_values_type.to_string_css t
    | #Numeric_fraction_values_type.t as t -> Numeric_fraction_values_type.to_string_css t
    | #Numeric_spacing_values_type.t as t -> Numeric_spacing_values_type.to_string_css t
    | `All_petite_caps -> "all-petite-caps"
    | `All_small_caps -> "all-small-caps"
    | `Emoji -> "emoji"
    | `Historical_forms -> "historical-forms"
    | `None -> "none"
    | `Normal -> "normal"
    | `Ordinal -> "ordinal"
    | `Petite_caps -> "petite-caps"
    | `Ruby -> "ruby"
    | `Slashed_zero -> "slashed-zero"
    | `Small_caps -> "small-caps"
    | `Sub -> "sub"
    | `Super -> "super"
    | `Text -> "text"
    | `Titling_caps -> "titling-caps"
    | `Unicase -> "unicase"
    | `Unicode -> "unicode"
  ;;
end

module Font_variant_alternates = struct
  type t =
    [ Feature_value_name_type.t
    | `Historical_forms
    | `Normal
    ]

  let to_string_css = function
    | #Feature_value_name_type.t as t -> Feature_value_name_type.to_string_css t
    | `Historical_forms -> "historical-forms"
    | `Normal -> "normal"
  ;;
end

module Font_variant_caps = struct
  type t =
    [ `All_petite_caps
    | `All_small_caps
    | `Normal
    | `Petite_caps
    | `Small_caps
    | `Titling_caps
    | `Unicase
    ]

  let to_string_css = function
    | `All_petite_caps -> "all-petite-caps"
    | `All_small_caps -> "all-small-caps"
    | `Normal -> "normal"
    | `Petite_caps -> "petite-caps"
    | `Small_caps -> "small-caps"
    | `Titling_caps -> "titling-caps"
    | `Unicase -> "unicase"
  ;;
end

module Font_variant_east_asian = struct
  type t =
    [ East_asian_variant_values_type.t
    | East_asian_width_values_type.t
    | `Normal
    | `Ruby
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #East_asian_variant_values_type.t as t ->
      East_asian_variant_values_type.to_string_css t
    | #East_asian_width_values_type.t as t -> East_asian_width_values_type.to_string_css t
    | `Normal -> "normal"
    | `Ruby -> "ruby"
  ;;
end

module Font_variant_emoji = struct
  type t =
    [ `Emoji
    | `Normal
    | `Text
    | `Unicode
    ]

  let to_string_css = function
    | `Emoji -> "emoji"
    | `Normal -> "normal"
    | `Text -> "text"
    | `Unicode -> "unicode"
  ;;
end

module Font_variant_ligatures = struct
  type t =
    [ Common_lig_values_type.t
    | Contextual_alt_values_type.t
    | Discretionary_lig_values_type.t
    | Historical_lig_values_type.t
    | `None
    | `Normal
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Common_lig_values_type.t as t -> Common_lig_values_type.to_string_css t
    | #Contextual_alt_values_type.t as t -> Contextual_alt_values_type.to_string_css t
    | #Discretionary_lig_values_type.t as t ->
      Discretionary_lig_values_type.to_string_css t
    | #Historical_lig_values_type.t as t -> Historical_lig_values_type.to_string_css t
    | `None -> "none"
    | `Normal -> "normal"
  ;;
end

module Font_variant_numeric = struct
  type t =
    [ Numeric_figure_values_type.t
    | Numeric_fraction_values_type.t
    | Numeric_spacing_values_type.t
    | `Normal
    | `Ordinal
    | `Slashed_zero
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Numeric_figure_values_type.t as t -> Numeric_figure_values_type.to_string_css t
    | #Numeric_fraction_values_type.t as t -> Numeric_fraction_values_type.to_string_css t
    | #Numeric_spacing_values_type.t as t -> Numeric_spacing_values_type.to_string_css t
    | `Normal -> "normal"
    | `Ordinal -> "ordinal"
    | `Slashed_zero -> "slashed-zero"
  ;;
end

module Font_variant_position = struct
  type t =
    [ `Normal
    | `Sub
    | `Super
    ]

  let to_string_css = function
    | `Normal -> "normal"
    | `Sub -> "sub"
    | `Super -> "super"
  ;;
end

module Font_variation_settings = struct
  type t =
    [ Css_data_type.Number.t
    | Opentype_tag_type.t
    | `Normal
    ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Opentype_tag_type.t as t -> Opentype_tag_type.to_string_css t
    | `Normal -> "normal"
  ;;
end

module Font_weight = struct
  type t =
    [ Css_data_type.Number.t
    | `Bold
    | `Bolder
    | `Lighter
    | `Normal
    ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | `Bold -> "bold"
    | `Bolder -> "bolder"
    | `Lighter -> "lighter"
    | `Normal -> "normal"
  ;;
end

module Font_width = struct
  type t =
    [ Css_data_type.Percentage.t
    | `Condensed
    | `Expanded
    | `Extra_condensed
    | `Extra_expanded
    | `Normal
    | `Semi_condensed
    | `Semi_expanded
    | `Ultra_condensed
    | `Ultra_expanded
    ]

  let to_string_css = function
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `Condensed -> "condensed"
    | `Expanded -> "expanded"
    | `Extra_condensed -> "extra-condensed"
    | `Extra_expanded -> "extra-expanded"
    | `Normal -> "normal"
    | `Semi_condensed -> "semi-condensed"
    | `Semi_expanded -> "semi-expanded"
    | `Ultra_condensed -> "ultra-condensed"
    | `Ultra_expanded -> "ultra-expanded"
  ;;
end

module Footnote_display = struct
  type t =
    [ `Block
    | `Compact
    | `Inline
    ]

  let to_string_css = function
    | `Block -> "block"
    | `Compact -> "compact"
    | `Inline -> "inline"
  ;;
end

module Footnote_policy = struct
  type t =
    [ `Auto
    | `Block
    | `Line
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Block -> "block"
    | `Line -> "line"
  ;;
end

module Forced_color_adjust = struct
  type t =
    [ `Auto
    | `None
    | `Preserve_parent_color
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `None -> "none"
    | `Preserve_parent_color -> "preserve-parent-color"
  ;;
end

module Glyph_orientation_vertical = struct
  type t =
    [ `Zero
    | `Zero_deg
    | `Ninety
    | `Ninety_deg
    | `Auto
    ]

  let to_string_css = function
    | `Zero -> "0"
    | `Zero_deg -> "0deg"
    | `Ninety -> "90"
    | `Ninety_deg -> "90deg"
    | `Auto -> "auto"
  ;;
end

module Grid_area = struct
  type t = [ | Grid_line_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Grid_line_type.t as t -> Grid_line_type.to_string_css t
  ;;
end

module Grid_auto_columns = struct
  type t = [ | Track_size_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Track_size_type.t as t -> Track_size_type.to_string_css t
  ;;
end

module Grid_auto_flow = struct
  type t =
    [ `Column
    | `Dense
    | `Row
    ]

  let to_string_css = function
    | `Column -> "column"
    | `Dense -> "dense"
    | `Row -> "row"
  ;;
end

module Grid_auto_rows = struct
  type t = [ | Track_size_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Track_size_type.t as t -> Track_size_type.to_string_css t
  ;;
end

module Grid_column = struct
  type t = [ | Grid_line_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Grid_line_type.t as t -> Grid_line_type.to_string_css t
  ;;
end

module Grid_column_end = struct
  type t = [ | Grid_line_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Grid_line_type.t as t -> Grid_line_type.to_string_css t
  ;;
end

module Grid_column_gap = struct
  type t =
    [ Length_percentage_type.t
    | `Normal
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Normal -> "normal"
  ;;
end

module Grid_column_start = struct
  type t = [ | Grid_line_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Grid_line_type.t as t -> Grid_line_type.to_string_css t
  ;;
end

module Grid_row = struct
  type t = [ | Grid_line_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Grid_line_type.t as t -> Grid_line_type.to_string_css t
  ;;
end

module Grid_row_end = struct
  type t = [ | Grid_line_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Grid_line_type.t as t -> Grid_line_type.to_string_css t
  ;;
end

module Grid_row_gap = struct
  type t =
    [ Length_percentage_type.t
    | `Normal
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Normal -> "normal"
  ;;
end

module Grid_row_start = struct
  type t = [ | Grid_line_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Grid_line_type.t as t -> Grid_line_type.to_string_css t
  ;;
end

module Grid_template_areas = struct
  type t =
    [ Css_data_type.Css_string.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
    | `None -> "none"
  ;;
end

module Hanging_punctuation = struct
  type t =
    [ `Allow_end
    | `First
    | `Force_end
    | `Last
    | `None
    ]

  let to_string_css = function
    | `Allow_end -> "allow-end"
    | `First -> "first"
    | `Force_end -> "force-end"
    | `Last -> "last"
    | `None -> "none"
  ;;
end

module Hyphenate_character = struct
  type t =
    [ Css_data_type.Css_string.t
    | `Auto
    ]

  let to_string_css = function
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Hyphenate_limit_chars = struct
  type t =
    [ Css_data_type.Integer.t
    | `Auto
    ]

  let to_string_css = function
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Hyphenate_limit_last = struct
  type t =
    [ `Always
    | `Column
    | `None
    | `Page
    | `Spread
    ]

  let to_string_css = function
    | `Always -> "always"
    | `Column -> "column"
    | `None -> "none"
    | `Page -> "page"
    | `Spread -> "spread"
  ;;
end

module Hyphenate_limit_lines = struct
  type t =
    [ Css_data_type.Integer.t
    | `No_limit
    ]

  let to_string_css = function
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | `No_limit -> "no-limit"
  ;;
end

module Hyphenate_limit_zone = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Hyphens = struct
  type t =
    [ `Auto
    | `Manual
    | `None
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Manual -> "manual"
    | `None -> "none"
  ;;
end

module Image_orientation = struct
  type t =
    [ Css_data_type.Angle.t
    | `Flip
    | `From_image
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Angle.t as t -> Css_data_type.Angle.to_string_css t
    | `Flip -> "flip"
    | `From_image -> "from-image"
    | `None -> "none"
  ;;
end

module Image_rendering = struct
  type t =
    [ `Auto
    | `Crisp_edges
    | `High_quality
    | `Pixelated
    | `Smooth
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Crisp_edges -> "crisp-edges"
    | `High_quality -> "high-quality"
    | `Pixelated -> "pixelated"
    | `Smooth -> "smooth"
  ;;
end

module Image_resolution = struct
  type t =
    [ Css_data_type.Resolution.t
    | `From_image
    | `Snap
    ]

  let to_string_css = function
    | #Css_data_type.Resolution.t as t -> Css_data_type.Resolution.to_string_css t
    | `From_image -> "from-image"
    | `Snap -> "snap"
  ;;
end

module Initial_letter = struct
  type t =
    [ Css_data_type.Integer.t
    | Css_data_type.Number.t
    | `Drop
    | `Normal
    | `Raise
    ]

  let to_string_css = function
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | `Drop -> "drop"
    | `Normal -> "normal"
    | `Raise -> "raise"
  ;;
end

module Initial_letter_align = struct
  type t =
    [ `Alphabetic
    | `Border_box
    | `Hanging
    | `Ideographic
    | `Leading
    ]

  let to_string_css = function
    | `Alphabetic -> "alphabetic"
    | `Border_box -> "border-box"
    | `Hanging -> "hanging"
    | `Ideographic -> "ideographic"
    | `Leading -> "leading"
  ;;
end

module Initial_letter_wrap = struct
  type t =
    [ Length_percentage_type.t
    | `All
    | `First
    | `Grid
    | `None
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `All -> "all"
    | `First -> "first"
    | `Grid -> "grid"
    | `None -> "none"
  ;;
end

module Inline_sizing = struct
  type t =
    [ `Normal
    | `Stretch
    ]

  let to_string_css = function
    | `Normal -> "normal"
    | `Stretch -> "stretch"
  ;;
end

module Input_security = struct
  type t =
    [ `Auto
    | `None
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `None -> "none"
  ;;
end

module Inset_block_end = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Inset_block_start = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Inset_inline_end = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Inset_inline_start = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Interactivity = struct
  type t =
    [ `Auto
    | `Inert
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Inert -> "inert"
  ;;
end

module Interpolate_size = struct
  type t =
    [ `Allow_keywords
    | `Numeric_only
    ]

  let to_string_css = function
    | `Allow_keywords -> "allow-keywords"
    | `Numeric_only -> "numeric-only"
  ;;
end

module Isolation = struct
  type t =
    [ `Auto
    | `Isolate
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Isolate -> "isolate"
  ;;
end

module Item_cross = struct
  type t =
    [ `Auto
    | `Normal
    | `Nowrap
    | `Reverse
    | `Wrap
    | `Wrap_reverse
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Normal -> "normal"
    | `Nowrap -> "nowrap"
    | `Reverse -> "reverse"
    | `Wrap -> "wrap"
    | `Wrap_reverse -> "wrap-reverse"
  ;;
end

module Item_direction = struct
  type t =
    [ `Auto
    | `Column
    | `Column_reverse
    | `Row
    | `Row_reverse
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Column -> "column"
    | `Column_reverse -> "column-reverse"
    | `Row -> "row"
    | `Row_reverse -> "row-reverse"
  ;;
end

module Item_pack = struct
  type t =
    [ `Balance
    | `Dense
    | `Normal
    ]

  let to_string_css = function
    | `Balance -> "balance"
    | `Dense -> "dense"
    | `Normal -> "normal"
  ;;
end

module Item_slack = struct
  type t =
    [ Length_percentage_type.t
    | `Infinite
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Infinite -> "infinite"
  ;;
end

module Item_track = struct
  type t =
    [ `Auto
    | `Column
    | `Column_reverse
    | `Row
    | `Row_reverse
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Column -> "column"
    | `Column_reverse -> "column-reverse"
    | `Row -> "row"
    | `Row_reverse -> "row-reverse"
  ;;
end

module Item_wrap = struct
  type t =
    [ `Auto
    | `Normal
    | `Nowrap
    | `Reverse
    | `Wrap
    | `Wrap_reverse
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Normal -> "normal"
    | `Nowrap -> "nowrap"
    | `Reverse -> "reverse"
    | `Wrap -> "wrap"
    | `Wrap_reverse -> "wrap-reverse"
  ;;
end

module Item_flow = struct
  type t =
    [ Item_direction.t
    | Item_pack.t
    | Item_slack.t
    | Item_wrap.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Item_direction.t as t -> Item_direction.to_string_css t
    | #Item_pack.t as t -> Item_pack.to_string_css t
    | #Item_slack.t as t -> Item_slack.to_string_css t
    | #Item_wrap.t as t -> Item_wrap.to_string_css t
  ;;
end

module Justify_content = struct
  type t =
    [ Content_distribution_type.t
    | Content_position_type.t
    | Overflow_position_type.t
    | `Left
    | `Normal
    | `Right
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Content_distribution_type.t as t -> Content_distribution_type.to_string_css t
    | #Content_position_type.t as t -> Content_position_type.to_string_css t
    | #Overflow_position_type.t as t -> Overflow_position_type.to_string_css t
    | `Left -> "left"
    | `Normal -> "normal"
    | `Right -> "right"
  ;;
end

module Justify_items = struct
  type t =
    [ Baseline_position_type.t
    | Overflow_position_type.t
    | Self_position_type.t
    | `Anchor_center
    | `Center
    | `Left
    | `Legacy
    | `Normal
    | `Right
    | `Stretch
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Baseline_position_type.t as t -> Baseline_position_type.to_string_css t
    | #Overflow_position_type.t as t -> Overflow_position_type.to_string_css t
    | #Self_position_type.t as t -> Self_position_type.to_string_css t
    | `Anchor_center -> "anchor-center"
    | `Center -> "center"
    | `Left -> "left"
    | `Legacy -> "legacy"
    | `Normal -> "normal"
    | `Right -> "right"
    | `Stretch -> "stretch"
  ;;
end

module Justify_self = struct
  type t =
    [ Baseline_position_type.t
    | Overflow_position_type.t
    | Self_position_type.t
    | `Anchor_center
    | `Auto
    | `Left
    | `Normal
    | `Right
    | `Stretch
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Baseline_position_type.t as t -> Baseline_position_type.to_string_css t
    | #Overflow_position_type.t as t -> Overflow_position_type.to_string_css t
    | #Self_position_type.t as t -> Self_position_type.to_string_css t
    | `Anchor_center -> "anchor-center"
    | `Auto -> "auto"
    | `Left -> "left"
    | `Normal -> "normal"
    | `Right -> "right"
    | `Stretch -> "stretch"
  ;;
end

module Letter_spacing = struct
  type t =
    [ Length_percentage_type.t
    | `Normal
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Normal -> "normal"
  ;;
end

module Lighting_color = struct
  type t = [ | Css_data_type.Color.t ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
  ;;
end

module Line_break = struct
  type t =
    [ `Anywhere
    | `Auto
    | `Loose
    | `Normal
    | `Strict
    ]

  let to_string_css = function
    | `Anywhere -> "anywhere"
    | `Auto -> "auto"
    | `Loose -> "loose"
    | `Normal -> "normal"
    | `Strict -> "strict"
  ;;
end

module Line_clamp = struct
  type t =
    [ Css_data_type.Integer.t
    | `None
    | Block_ellipsis.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | `None -> "none"
    | #Block_ellipsis.t as t -> Block_ellipsis.to_string_css t
  ;;
end

module Line_fit_edge = struct
  type t =
    [ Text_edge_type.t
    | `Leading
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Text_edge_type.t as t -> Text_edge_type.to_string_css t
    | `Leading -> "leading"
  ;;
end

module Line_grid = struct
  type t =
    [ `Create
    | `Match_parent
    ]

  let to_string_css = function
    | `Create -> "create"
    | `Match_parent -> "match-parent"
  ;;
end

module Line_height = struct
  type t =
    [ Length_percentage_type.t
    | Css_data_type.Number.t
    | `Normal
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | `Normal -> "normal"
  ;;
end

module Font = struct
  type t =
    [ `Caption
    | `Condensed
    | `Expanded
    | `Extra_condensed
    | `Extra_expanded
    | `Icon
    | `Menu
    | `Message_box
    | `Normal
    | `Semi_condensed
    | `Semi_expanded
    | `Small_caps
    | `Small_caption
    | `Status_bar
    | `Ultra_condensed
    | `Ultra_expanded
    | Font_family.t
    | Font_size.t
    | Font_style.t
    | Font_weight.t
    | Line_height.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | `Caption -> "caption"
    | `Condensed -> "condensed"
    | `Expanded -> "expanded"
    | `Extra_condensed -> "extra-condensed"
    | `Extra_expanded -> "extra-expanded"
    | `Icon -> "icon"
    | `Menu -> "menu"
    | `Message_box -> "message-box"
    | `Normal -> "normal"
    | `Semi_condensed -> "semi-condensed"
    | `Semi_expanded -> "semi-expanded"
    | `Small_caps -> "small-caps"
    | `Small_caption -> "small-caption"
    | `Status_bar -> "status-bar"
    | `Ultra_condensed -> "ultra-condensed"
    | `Ultra_expanded -> "ultra-expanded"
    | #Font_family.t as t -> Font_family.to_string_css t
    | #Font_size.t as t -> Font_size.to_string_css t
    | #Font_style.t as t -> Font_style.to_string_css t
    | #Font_weight.t as t -> Font_weight.to_string_css t
    | #Line_height.t as t -> Line_height.to_string_css t
  ;;
end

module Line_height_step = struct
  type t = [ | Css_data_type.Length.t ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
  ;;
end

module Line_padding = struct
  type t = [ | Css_data_type.Length.t ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
  ;;
end

module Line_snap = struct
  type t =
    [ `Baseline
    | `Contain
    | `None
    ]

  let to_string_css = function
    | `Baseline -> "baseline"
    | `Contain -> "contain"
    | `None -> "none"
  ;;
end

module Link_parameters = struct
  type t =
    [ Css_data_type.Custom_property_name.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Custom_property_name.t as t ->
      Css_data_type.Custom_property_name.to_string_css t
    | `None -> "none"
  ;;
end

module List_style_position = struct
  type t =
    [ `Inside
    | `Outside
    ]

  let to_string_css = function
    | `Inside -> "inside"
    | `Outside -> "outside"
  ;;
end

module Margin_break = struct
  type t =
    [ `Auto
    | `Discard
    | `Keep
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Discard -> "discard"
    | `Keep -> "keep"
  ;;
end

module Margin_trim = struct
  type t =
    [ `Block
    | `Block_end
    | `Block_start
    | `Inline
    | `Inline_end
    | `Inline_start
    | `None
    ]

  let to_string_css = function
    | `Block -> "block"
    | `Block_end -> "block-end"
    | `Block_start -> "block-start"
    | `Inline -> "inline"
    | `Inline_end -> "inline-end"
    | `Inline_start -> "inline-start"
    | `None -> "none"
  ;;
end

module Marker_side = struct
  type t =
    [ `Match_parent
    | `Match_self
    ]

  let to_string_css = function
    | `Match_parent -> "match-parent"
    | `Match_self -> "match-self"
  ;;
end

module Mask_border_mode = struct
  type t =
    [ `Alpha_
    | `Luminance
    ]

  let to_string_css = function
    | `Alpha_ -> "alpha"
    | `Luminance -> "luminance"
  ;;
end

module Mask_border_outset = struct
  type t =
    [ Css_data_type.Length.t
    | Css_data_type.Number.t
    ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
  ;;
end

module Mask_border_repeat = struct
  type t =
    [ `Repeat
    | `Round
    | `Space
    | `Stretch
    ]

  let to_string_css = function
    | `Repeat -> "repeat"
    | `Round -> "round"
    | `Space -> "space"
    | `Stretch -> "stretch"
  ;;
end

module Mask_border_slice = struct
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `Fill
    ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `Fill -> "fill"
  ;;
end

module Mask_border_width = struct
  type t =
    [ Length_percentage_type.t
    | Css_data_type.Number.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Mask_clip = struct
  type t =
    [ Coord_box_type.t
    | `No_clip
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Coord_box_type.t as t -> Coord_box_type.to_string_css t
    | `No_clip -> "no-clip"
  ;;
end

module Mask_composite = struct
  type t = [ | Compositing_operator_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Compositing_operator_type.t as t -> Compositing_operator_type.to_string_css t
  ;;
end

module Mask_mode = struct
  type t = [ | Masking_mode_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Masking_mode_type.t as t -> Masking_mode_type.to_string_css t
  ;;
end

module Mask_origin = struct
  type t = [ | Coord_box_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Coord_box_type.t as t -> Coord_box_type.to_string_css t
  ;;
end

module Mask_position = struct
  type t = [ | Position_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Position_type.t as t -> Position_type.to_string_css t
  ;;
end

module Mask_repeat = struct
  type t = [ | Repeat_style_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Repeat_style_type.t as t -> Repeat_style_type.to_string_css t
  ;;
end

module Mask_size = struct
  type t = [ | Bg_size_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Bg_size_type.t as t -> Bg_size_type.to_string_css t
  ;;
end

module Mask_type = struct
  type t =
    [ `Alpha_
    | `Luminance
    ]

  let to_string_css = function
    | `Alpha_ -> "alpha"
    | `Luminance -> "luminance"
  ;;
end

module Math_depth = struct
  type t =
    [ Css_data_type.Integer.t
    | `Auto_add
    ]

  let to_string_css = function
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | `Auto_add -> "auto-add"
  ;;
end

module Math_shift = struct
  type t =
    [ `Compact
    | `Normal
    ]

  let to_string_css = function
    | `Compact -> "compact"
    | `Normal -> "normal"
  ;;
end

module Math_style = struct
  type t =
    [ `Compact
    | `Normal
    ]

  let to_string_css = function
    | `Compact -> "compact"
    | `Normal -> "normal"
  ;;
end

module Max_lines = struct
  type t =
    [ Css_data_type.Integer.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | `None -> "none"
  ;;
end

module Min_intrinsic_sizing = struct
  type t =
    [ `Legacy
    | `Zero_if_extrinsic
    | `Zero_if_scroll
    ]

  let to_string_css = function
    | `Legacy -> "legacy"
    | `Zero_if_extrinsic -> "zero-if-extrinsic"
    | `Zero_if_scroll -> "zero-if-scroll"
  ;;
end

module Mix_blend_mode = struct
  type t =
    [ Blend_mode_type.t
    | `Plus_darker
    | `Plus_lighter
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Blend_mode_type.t as t -> Blend_mode_type.to_string_css t
    | `Plus_darker -> "plus-darker"
    | `Plus_lighter -> "plus-lighter"
  ;;
end

module Background_blend_mode = struct
  type t = [ | Mix_blend_mode.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Mix_blend_mode.t as t -> Mix_blend_mode.to_string_css t
  ;;
end

module Nav_down = struct
  type t =
    [ `Auto
    | `Current
    | `Root
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Current -> "current"
    | `Root -> "root"
  ;;
end

module Nav_left = struct
  type t =
    [ `Auto
    | `Current
    | `Root
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Current -> "current"
    | `Root -> "root"
  ;;
end

module Nav_right = struct
  type t =
    [ `Auto
    | `Current
    | `Root
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Current -> "current"
    | `Root -> "root"
  ;;
end

module Nav_up = struct
  type t =
    [ `Auto
    | `Current
    | `Root
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Current -> "current"
    | `Root -> "root"
  ;;
end

module Object_fit = struct
  type t =
    [ `Contain
    | `Cover
    | `Fill
    | `None
    | `Scale_down
    ]

  let to_string_css = function
    | `Contain -> "contain"
    | `Cover -> "cover"
    | `Fill -> "fill"
    | `None -> "none"
    | `Scale_down -> "scale-down"
  ;;
end

module Object_position = struct
  type t = [ | Position_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Position_type.t as t -> Position_type.to_string_css t
  ;;
end

module Offset_anchor = struct
  type t =
    [ Position_type.t
    | `Auto
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Position_type.t as t -> Position_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Offset_distance = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Offset_position = struct
  type t =
    [ Position_type.t
    | `Auto
    | `Normal
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Position_type.t as t -> Position_type.to_string_css t
    | `Auto -> "auto"
    | `Normal -> "normal"
  ;;
end

module Offset_rotate = struct
  type t =
    [ Css_data_type.Angle.t
    | `Auto
    | `Reverse
    ]

  let to_string_css = function
    | #Css_data_type.Angle.t as t -> Css_data_type.Angle.to_string_css t
    | `Auto -> "auto"
    | `Reverse -> "reverse"
  ;;
end

module Opacity = struct
  module Opacity_value_type = struct
    type t =
      [ Css_data_type.Number.t
      | Css_data_type.Percentage.t
      ]

    let to_string_css = function
      | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
      | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    ;;
  end

  type t = [ | Opacity_value_type.t ]

  let to_string_css = function
    | #Opacity_value_type.t as t -> Opacity_value_type.to_string_css t
  ;;
end

module Fill_opacity = struct
  type t = [ | Opacity.t ]

  let to_string_css = function
    | #Opacity.t as t -> Opacity.to_string_css t
  ;;
end

module Flood_opacity = struct
  type t = [ | Opacity.t ]

  let to_string_css = function
    | #Opacity.t as t -> Opacity.to_string_css t
  ;;
end

module Order = struct
  type t = [ | Css_data_type.Integer.t ]

  let to_string_css = function
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
  ;;
end

module Orphans = struct
  type t = [ | Css_data_type.Integer.t ]

  let to_string_css = function
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
  ;;
end

module Outline_offset = struct
  type t = [ | Css_data_type.Length.t ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
  ;;
end

module Outline_style = struct
  type t =
    [ `Auto
    | `Dashed
    | `Dotted
    | `Double
    | `Groove
    | `Inset
    | `None
    | `Outset
    | `Ridge
    | `Solid
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Dashed -> "dashed"
    | `Dotted -> "dotted"
    | `Double -> "double"
    | `Groove -> "groove"
    | `Inset -> "inset"
    | `None -> "none"
    | `Outset -> "outset"
    | `Ridge -> "ridge"
    | `Solid -> "solid"
  ;;
end

module Outline_width = struct
  type t = [ | Line_width_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Line_width_type.t as t -> Line_width_type.to_string_css t
  ;;
end

module Overflow_anchor = struct
  type t =
    [ `Auto
    | `None
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `None -> "none"
  ;;
end

module Overflow_block = struct
  type t =
    [ `Auto
    | `Clip
    | `Hidden
    | `Overlay
    | `Scroll
    | `Visible
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Clip -> "clip"
    | `Hidden -> "hidden"
    | `Overlay -> "overlay"
    | `Scroll -> "scroll"
    | `Visible -> "visible"
  ;;
end

module Overflow = struct
  type t = [ | Overflow_block.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Overflow_block.t as t -> Overflow_block.to_string_css t
  ;;
end

module Overflow_clip_margin = struct
  type t =
    [ Css_data_type.Length.t
    | Visual_box_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | #Visual_box_type.t as t -> Visual_box_type.to_string_css t
  ;;
end

module Overflow_clip_margin_block = struct
  type t =
    [ Css_data_type.Length.t
    | Visual_box_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | #Visual_box_type.t as t -> Visual_box_type.to_string_css t
  ;;
end

module Overflow_clip_margin_block_end = struct
  type t =
    [ Css_data_type.Length.t
    | Visual_box_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | #Visual_box_type.t as t -> Visual_box_type.to_string_css t
  ;;
end

module Overflow_clip_margin_block_start = struct
  type t =
    [ Css_data_type.Length.t
    | Visual_box_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | #Visual_box_type.t as t -> Visual_box_type.to_string_css t
  ;;
end

module Overflow_clip_margin_bottom = struct
  type t =
    [ Css_data_type.Length.t
    | Visual_box_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | #Visual_box_type.t as t -> Visual_box_type.to_string_css t
  ;;
end

module Overflow_clip_margin_inline = struct
  type t =
    [ Css_data_type.Length.t
    | Visual_box_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | #Visual_box_type.t as t -> Visual_box_type.to_string_css t
  ;;
end

module Overflow_clip_margin_inline_end = struct
  type t =
    [ Css_data_type.Length.t
    | Visual_box_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | #Visual_box_type.t as t -> Visual_box_type.to_string_css t
  ;;
end

module Overflow_clip_margin_inline_start = struct
  type t =
    [ Css_data_type.Length.t
    | Visual_box_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | #Visual_box_type.t as t -> Visual_box_type.to_string_css t
  ;;
end

module Overflow_clip_margin_left = struct
  type t =
    [ Css_data_type.Length.t
    | Visual_box_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | #Visual_box_type.t as t -> Visual_box_type.to_string_css t
  ;;
end

module Overflow_clip_margin_right = struct
  type t =
    [ Css_data_type.Length.t
    | Visual_box_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | #Visual_box_type.t as t -> Visual_box_type.to_string_css t
  ;;
end

module Overflow_clip_margin_top = struct
  type t =
    [ Css_data_type.Length.t
    | Visual_box_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | #Visual_box_type.t as t -> Visual_box_type.to_string_css t
  ;;
end

module Overflow_inline = struct
  type t =
    [ `Auto
    | `Clip
    | `Hidden
    | `Scroll
    | `Visible
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Clip -> "clip"
    | `Hidden -> "hidden"
    | `Scroll -> "scroll"
    | `Visible -> "visible"
  ;;
end

module Overflow_wrap = struct
  type t =
    [ `Anywhere
    | `Break_word
    | `Normal
    ]

  let to_string_css = function
    | `Anywhere -> "anywhere"
    | `Break_word -> "break-word"
    | `Normal -> "normal"
  ;;
end

module Overflow_x = struct
  type t =
    [ `Auto
    | `Clip
    | `Hidden
    | `Scroll
    | `Visible
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Clip -> "clip"
    | `Hidden -> "hidden"
    | `Scroll -> "scroll"
    | `Visible -> "visible"
  ;;
end

module Overflow_y = struct
  type t =
    [ `Auto
    | `Clip
    | `Hidden
    | `Scroll
    | `Visible
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Clip -> "clip"
    | `Hidden -> "hidden"
    | `Scroll -> "scroll"
    | `Visible -> "visible"
  ;;
end

module Overlay = struct
  type t =
    [ `Auto
    | `None
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `None -> "none"
  ;;
end

module Overscroll_behavior = struct
  type t =
    [ `Auto
    | `Contain
    | `None
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Contain -> "contain"
    | `None -> "none"
  ;;
end

module Overscroll_behavior_block = struct
  type t =
    [ `Auto
    | `Contain
    | `None
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Contain -> "contain"
    | `None -> "none"
  ;;
end

module Overscroll_behavior_inline = struct
  type t =
    [ `Auto
    | `Contain
    | `None
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Contain -> "contain"
    | `None -> "none"
  ;;
end

module Overscroll_behavior_x = struct
  type t =
    [ `Auto
    | `Contain
    | `None
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Contain -> "contain"
    | `None -> "none"
  ;;
end

module Overscroll_behavior_y = struct
  type t =
    [ `Auto
    | `Contain
    | `None
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Contain -> "contain"
    | `None -> "none"
  ;;
end

module Padding_bottom = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Padding_left = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Padding_right = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Padding_top = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Padding = struct
  type t = [ | Padding_top.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Padding_top.t as t -> Padding_top.to_string_css t
  ;;
end

module Padding_block = struct
  type t = [ | Padding_top.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Padding_top.t as t -> Padding_top.to_string_css t
  ;;
end

module Padding_block_end = struct
  type t = [ | Padding_top.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Padding_top.t as t -> Padding_top.to_string_css t
  ;;
end

module Padding_block_start = struct
  type t = [ | Padding_top.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Padding_top.t as t -> Padding_top.to_string_css t
  ;;
end

module Padding_inline = struct
  type t = [ | Padding_top.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Padding_top.t as t -> Padding_top.to_string_css t
  ;;
end

module Padding_inline_end = struct
  type t = [ | Padding_top.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Padding_top.t as t -> Padding_top.to_string_css t
  ;;
end

module Padding_inline_start = struct
  type t = [ | Padding_top.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Padding_top.t as t -> Padding_top.to_string_css t
  ;;
end

module Page = struct
  type t =
    [ Css_data_type.Ident.Custom.t
    | `Auto
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Custom.t as t -> Css_data_type.Ident.Custom.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Page_break_after = struct
  type t =
    [ `Always
    | `Auto
    | `Avoid
    | `Inherit
    | `Left
    | `Right
    ]

  let to_string_css = function
    | `Always -> "always"
    | `Auto -> "auto"
    | `Avoid -> "avoid"
    | `Inherit -> "inherit"
    | `Left -> "left"
    | `Right -> "right"
  ;;
end

module Page_break_before = struct
  type t =
    [ `Always
    | `Auto
    | `Avoid
    | `Inherit
    | `Left
    | `Right
    ]

  let to_string_css = function
    | `Always -> "always"
    | `Auto -> "auto"
    | `Avoid -> "avoid"
    | `Inherit -> "inherit"
    | `Left -> "left"
    | `Right -> "right"
  ;;
end

module Page_break_inside = struct
  type t =
    [ `Auto
    | `Avoid
    | `Inherit
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Avoid -> "avoid"
    | `Inherit -> "inherit"
  ;;
end

module Paint_order = struct
  type t =
    [ `Fill
    | `Markers
    | `Normal
    | `Stroke
    ]

  let to_string_css = function
    | `Fill -> "fill"
    | `Markers -> "markers"
    | `Normal -> "normal"
    | `Stroke -> "stroke"
  ;;
end

module Pause_after = struct
  type t =
    [ Css_data_type.Time.t
    | `Medium
    | `None
    | `Strong
    | `Weak
    | `X_strong
    | `X_weak
    ]

  let to_string_css = function
    | #Css_data_type.Time.t as t -> Css_data_type.Time.to_string_css t
    | `Medium -> "medium"
    | `None -> "none"
    | `Strong -> "strong"
    | `Weak -> "weak"
    | `X_strong -> "x-strong"
    | `X_weak -> "x-weak"
  ;;
end

module Pause_before = struct
  type t =
    [ Css_data_type.Time.t
    | `Medium
    | `None
    | `Strong
    | `Weak
    | `X_strong
    | `X_weak
    ]

  let to_string_css = function
    | #Css_data_type.Time.t as t -> Css_data_type.Time.to_string_css t
    | `Medium -> "medium"
    | `None -> "none"
    | `Strong -> "strong"
    | `Weak -> "weak"
    | `X_strong -> "x-strong"
    | `X_weak -> "x-weak"
  ;;
end

module Pause = struct
  type t =
    [ Pause_after.t
    | Pause_before.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Pause_after.t as t -> Pause_after.to_string_css t
    | #Pause_before.t as t -> Pause_before.to_string_css t
  ;;
end

module Perspective = struct
  type t =
    [ Css_data_type.Length.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | `None -> "none"
  ;;
end

module Perspective_origin = struct
  type t = [ | Position_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Position_type.t as t -> Position_type.to_string_css t
  ;;
end

module Place_content = struct
  type t =
    [ Align_content.t
    | Justify_content.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Align_content.t as t -> Align_content.to_string_css t
    | #Justify_content.t as t -> Justify_content.to_string_css t
  ;;
end

module Place_items = struct
  type t =
    [ Align_items.t
    | Justify_items.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Align_items.t as t -> Align_items.to_string_css t
    | #Justify_items.t as t -> Justify_items.to_string_css t
  ;;
end

module Place_self = struct
  type t =
    [ Align_self.t
    | Justify_self.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Align_self.t as t -> Align_self.to_string_css t
    | #Justify_self.t as t -> Justify_self.to_string_css t
  ;;
end

module Pointer_events = struct
  type t =
    [ `All
    | `Auto
    | `Bounding_box
    | `Fill
    | `None
    | `Painted
    | `Stroke
    | `Visible
    | `VisibleFill
    | `VisiblePainted
    | `VisibleStroke
    ]

  let to_string_css = function
    | `All -> "all"
    | `Auto -> "auto"
    | `Bounding_box -> "bounding-box"
    | `Fill -> "fill"
    | `None -> "none"
    | `Painted -> "painted"
    | `Stroke -> "stroke"
    | `Visible -> "visible"
    | `VisibleFill -> "visibleFill"
    | `VisiblePainted -> "visiblePainted"
    | `VisibleStroke -> "visibleStroke"
  ;;
end

module Pointer_timeline_axis = struct
  type t =
    [ `Block
    | `Inline
    | `X
    | `Y
    ]

  let to_string_css = function
    | `Block -> "block"
    | `Inline -> "inline"
    | `X -> "x"
    | `Y -> "y"
  ;;
end

module Pointer_timeline_name = struct
  type t =
    [ Css_data_type.Ident.Dashed.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Dashed.t as t -> Css_data_type.Ident.Dashed.to_string_css t
    | `None -> "none"
  ;;
end

module Pointer_timeline = struct
  type t =
    [ Pointer_timeline_axis.t
    | Pointer_timeline_name.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Pointer_timeline_axis.t as t -> Pointer_timeline_axis.to_string_css t
    | #Pointer_timeline_name.t as t -> Pointer_timeline_name.to_string_css t
  ;;
end

module Position_anchor = struct
  type t =
    [ Anchor_name_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Anchor_name_type.t as t -> Anchor_name_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Position_area = struct
  type t =
    [ `Block_end
    | `Block_start
    | `Bottom
    | `Center
    | `End
    | `Inline_end
    | `Inline_start
    | `Left
    | `None
    | `Right
    | `Self_block_end
    | `Self_block_start
    | `Self_end
    | `Self_inline_end
    | `Self_inline_start
    | `Self_start
    | `Span_all
    | `Span_block_end
    | `Span_block_start
    | `Span_bottom
    | `Span_end
    | `Span_inline_end
    | `Span_inline_start
    | `Span_left
    | `Span_right
    | `Span_self_block_end
    | `Span_self_block_start
    | `Span_self_end
    | `Span_self_inline_end
    | `Span_self_inline_start
    | `Span_self_start
    | `Span_start
    | `Span_top
    | `Span_x_end
    | `Span_x_self_end
    | `Span_x_self_start
    | `Span_x_start
    | `Span_y_end
    | `Span_y_self_end
    | `Span_y_self_start
    | `Span_y_start
    | `Start
    | `Top
    | `X_end
    | `X_self_end
    | `X_self_start
    | `X_start
    | `Y_end
    | `Y_self_end
    | `Y_self_start
    | `Y_start
    ]

  let to_string_css = function
    | `Block_end -> "block-end"
    | `Block_start -> "block-start"
    | `Bottom -> "bottom"
    | `Center -> "center"
    | `End -> "end"
    | `Inline_end -> "inline-end"
    | `Inline_start -> "inline-start"
    | `Left -> "left"
    | `None -> "none"
    | `Right -> "right"
    | `Self_block_end -> "self-block-end"
    | `Self_block_start -> "self-block-start"
    | `Self_end -> "self-end"
    | `Self_inline_end -> "self-inline-end"
    | `Self_inline_start -> "self-inline-start"
    | `Self_start -> "self-start"
    | `Span_all -> "span-all"
    | `Span_block_end -> "span-block-end"
    | `Span_block_start -> "span-block-start"
    | `Span_bottom -> "span-bottom"
    | `Span_end -> "span-end"
    | `Span_inline_end -> "span-inline-end"
    | `Span_inline_start -> "span-inline-start"
    | `Span_left -> "span-left"
    | `Span_right -> "span-right"
    | `Span_self_block_end -> "span-self-block-end"
    | `Span_self_block_start -> "span-self-block-start"
    | `Span_self_end -> "span-self-end"
    | `Span_self_inline_end -> "span-self-inline-end"
    | `Span_self_inline_start -> "span-self-inline-start"
    | `Span_self_start -> "span-self-start"
    | `Span_start -> "span-start"
    | `Span_top -> "span-top"
    | `Span_x_end -> "span-x-end"
    | `Span_x_self_end -> "span-x-self-end"
    | `Span_x_self_start -> "span-x-self-start"
    | `Span_x_start -> "span-x-start"
    | `Span_y_end -> "span-y-end"
    | `Span_y_self_end -> "span-y-self-end"
    | `Span_y_self_start -> "span-y-self-start"
    | `Span_y_start -> "span-y-start"
    | `Start -> "start"
    | `Top -> "top"
    | `X_end -> "x-end"
    | `X_self_end -> "x-self-end"
    | `X_self_start -> "x-self-start"
    | `X_start -> "x-start"
    | `Y_end -> "y-end"
    | `Y_self_end -> "y-self-end"
    | `Y_self_start -> "y-self-start"
    | `Y_start -> "y-start"
  ;;
end

module Position_try_fallbacks = struct
  module Try_tactic_type = struct
    type t =
      [ `Flip_block
      | `Flip_inline
      | `Flip_start
      ]

    let to_string_css = function
      | `Flip_block -> "flip-block"
      | `Flip_inline -> "flip-inline"
      | `Flip_start -> "flip-start"
    ;;
  end

  type t =
    [ Css_data_type.Ident.Dashed.t
    | Try_tactic_type.t
    | `None
    | Position_area.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Ident.Dashed.t as t -> Css_data_type.Ident.Dashed.to_string_css t
    | #Try_tactic_type.t as t -> Try_tactic_type.to_string_css t
    | `None -> "none"
    | #Position_area.t as t -> Position_area.to_string_css t
  ;;
end

module Position_try_order = struct
  type t =
    [ `Most_block_size
    | `Most_height
    | `Most_inline_size
    | `Most_width
    | `Normal
    ]

  let to_string_css = function
    | `Most_block_size -> "most-block-size"
    | `Most_height -> "most-height"
    | `Most_inline_size -> "most-inline-size"
    | `Most_width -> "most-width"
    | `Normal -> "normal"
  ;;
end

module Position_try = struct
  type t =
    [ Position_try_fallbacks.t
    | Position_try_order.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Position_try_fallbacks.t as t -> Position_try_fallbacks.to_string_css t
    | #Position_try_order.t as t -> Position_try_order.to_string_css t
  ;;
end

module Position_visibility = struct
  type t =
    [ `Always
    | `Anchors_valid
    | `Anchors_visible
    | `No_overflow
    ]

  let to_string_css = function
    | `Always -> "always"
    | `Anchors_valid -> "anchors-valid"
    | `Anchors_visible -> "anchors-visible"
    | `No_overflow -> "no-overflow"
  ;;
end

module Print_color_adjust = struct
  type t =
    [ `Economy
    | `Exact
    ]

  let to_string_css = function
    | `Economy -> "economy"
    | `Exact -> "exact"
  ;;
end

module Color_adjust = struct
  type t = [ | Print_color_adjust.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Print_color_adjust.t as t -> Print_color_adjust.to_string_css t
  ;;
end

module Quotes = struct
  type t =
    [ Css_data_type.Css_string.t
    | `Auto
    | `Match_parent
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
    | `Auto -> "auto"
    | `Match_parent -> "match-parent"
    | `None -> "none"
  ;;
end

module R = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Reading_flow = struct
  type t =
    [ `Flex_flow
    | `Flex_visual
    | `Grid_columns
    | `Grid_order
    | `Grid_rows
    | `Normal
    | `Source_order
    ]

  let to_string_css = function
    | `Flex_flow -> "flex-flow"
    | `Flex_visual -> "flex-visual"
    | `Grid_columns -> "grid-columns"
    | `Grid_order -> "grid-order"
    | `Grid_rows -> "grid-rows"
    | `Normal -> "normal"
    | `Source_order -> "source-order"
  ;;
end

module Reading_order = struct
  type t = [ | Css_data_type.Integer.t ]

  let to_string_css = function
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
  ;;
end

module Region_fragment = struct
  type t =
    [ `Auto
    | `Break
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Break -> "break"
  ;;
end

module Resize = struct
  type t =
    [ `Block
    | `Both
    | `Horizontal
    | `Inline
    | `None
    | `Vertical
    ]

  let to_string_css = function
    | `Block -> "block"
    | `Both -> "both"
    | `Horizontal -> "horizontal"
    | `Inline -> "inline"
    | `None -> "none"
    | `Vertical -> "vertical"
  ;;
end

module Rest_after = struct
  type t =
    [ Css_data_type.Time.t
    | `Medium
    | `None
    | `Strong
    | `Weak
    | `X_strong
    | `X_weak
    ]

  let to_string_css = function
    | #Css_data_type.Time.t as t -> Css_data_type.Time.to_string_css t
    | `Medium -> "medium"
    | `None -> "none"
    | `Strong -> "strong"
    | `Weak -> "weak"
    | `X_strong -> "x-strong"
    | `X_weak -> "x-weak"
  ;;
end

module Rest_before = struct
  type t =
    [ Css_data_type.Time.t
    | `Medium
    | `None
    | `Strong
    | `Weak
    | `X_strong
    | `X_weak
    ]

  let to_string_css = function
    | #Css_data_type.Time.t as t -> Css_data_type.Time.to_string_css t
    | `Medium -> "medium"
    | `None -> "none"
    | `Strong -> "strong"
    | `Weak -> "weak"
    | `X_strong -> "x-strong"
    | `X_weak -> "x-weak"
  ;;
end

module Rest = struct
  type t =
    [ Rest_after.t
    | Rest_before.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Rest_after.t as t -> Rest_after.to_string_css t
    | #Rest_before.t as t -> Rest_before.to_string_css t
  ;;
end

module Rotate = struct
  type t =
    [ Css_data_type.Angle.t
    | Css_data_type.Number.t
    | `None
    | `X
    | `Y
    | `Z
    ]

  let to_string_css = function
    | #Css_data_type.Angle.t as t -> Css_data_type.Angle.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | `None -> "none"
    | `X -> "x"
    | `Y -> "y"
    | `Z -> "z"
  ;;
end

module Row_gap = struct
  type t =
    [ Length_percentage_type.t
    | `Normal
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Normal -> "normal"
  ;;
end

module Gap = struct
  type t =
    [ Column_gap.t
    | Row_gap.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Column_gap.t as t -> Column_gap.to_string_css t
    | #Row_gap.t as t -> Row_gap.to_string_css t
  ;;
end

module Grid_gap = struct
  type t =
    [ Column_gap.t
    | Row_gap.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Column_gap.t as t -> Column_gap.to_string_css t
    | #Row_gap.t as t -> Row_gap.to_string_css t
  ;;
end

module Row_rule = struct
  type t =
    [ Gap_auto_rule_list_type.t
    | Gap_rule_list_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Gap_auto_rule_list_type.t as t -> Gap_auto_rule_list_type.to_string_css t
    | #Gap_rule_list_type.t as t -> Gap_rule_list_type.to_string_css t
  ;;
end

module Row_rule_break = struct
  type t =
    [ `Intersection
    | `None
    | `Spanning_item
    ]

  let to_string_css = function
    | `Intersection -> "intersection"
    | `None -> "none"
    | `Spanning_item -> "spanning-item"
  ;;
end

module Row_rule_color = struct
  type t =
    [ Auto_line_color_list_type.t
    | Line_color_list_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Auto_line_color_list_type.t as t -> Auto_line_color_list_type.to_string_css t
    | #Line_color_list_type.t as t -> Line_color_list_type.to_string_css t
  ;;
end

module Row_rule_outset = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Row_rule_style = struct
  type t =
    [ Auto_line_style_list_type.t
    | Line_style_list_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Auto_line_style_list_type.t as t -> Auto_line_style_list_type.to_string_css t
    | #Line_style_list_type.t as t -> Line_style_list_type.to_string_css t
  ;;
end

module Row_rule_width = struct
  type t =
    [ Auto_line_width_list_type.t
    | Line_width_list_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Auto_line_width_list_type.t as t -> Auto_line_width_list_type.to_string_css t
    | #Line_width_list_type.t as t -> Line_width_list_type.to_string_css t
  ;;
end

module Ruby_align = struct
  type t =
    [ `Center
    | `Space_around
    | `Space_between
    | `Start
    ]

  let to_string_css = function
    | `Center -> "center"
    | `Space_around -> "space-around"
    | `Space_between -> "space-between"
    | `Start -> "start"
  ;;
end

module Ruby_merge = struct
  type t =
    [ `Auto
    | `Merge
    | `Separate
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Merge -> "merge"
    | `Separate -> "separate"
  ;;
end

module Ruby_overhang = struct
  type t =
    [ `Auto
    | `None
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `None -> "none"
  ;;
end

module Ruby_position = struct
  type t =
    [ `Alternate
    | `Inter_character
    | `Over
    | `Under
    ]

  let to_string_css = function
    | `Alternate -> "alternate"
    | `Inter_character -> "inter-character"
    | `Over -> "over"
    | `Under -> "under"
  ;;
end

module Rule = struct
  type t = [ | Column_rule.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Column_rule.t as t -> Column_rule.to_string_css t
  ;;
end

module Rule_break = struct
  type t = [ | Column_rule_break.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Column_rule_break.t as t -> Column_rule_break.to_string_css t
  ;;
end

module Rule_color = struct
  type t = [ | Column_rule_color.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Column_rule_color.t as t -> Column_rule_color.to_string_css t
  ;;
end

module Rule_outset = struct
  type t = [ | Column_rule_outset.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Column_rule_outset.t as t -> Column_rule_outset.to_string_css t
  ;;
end

module Rule_paint_order = struct
  type t =
    [ `Column_over_row
    | `Row_over_column
    ]

  let to_string_css = function
    | `Column_over_row -> "column-over-row"
    | `Row_over_column -> "row-over-column"
  ;;
end

module Rule_style = struct
  type t = [ | Column_rule_style.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Column_rule_style.t as t -> Column_rule_style.to_string_css t
  ;;
end

module Rule_width = struct
  type t = [ | Column_rule_width.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Column_rule_width.t as t -> Column_rule_width.to_string_css t
  ;;
end

module Rx = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Ry = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Scale = struct
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `None -> "none"
  ;;
end

module Scroll_behavior = struct
  type t =
    [ `Auto
    | `Smooth
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Smooth -> "smooth"
  ;;
end

module Scroll_initial_target = struct
  type t =
    [ `Nearest
    | `None
    ]

  let to_string_css = function
    | `Nearest -> "nearest"
    | `None -> "none"
  ;;
end

module Scroll_margin = struct
  type t = [ | Css_data_type.Length.t ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
  ;;
end

module Scroll_margin_block = struct
  type t = [ | Css_data_type.Length.t ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
  ;;
end

module Scroll_margin_block_end = struct
  type t = [ | Css_data_type.Length.t ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
  ;;
end

module Scroll_margin_block_start = struct
  type t = [ | Css_data_type.Length.t ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
  ;;
end

module Scroll_margin_bottom = struct
  type t = [ | Css_data_type.Length.t ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
  ;;
end

module Scroll_margin_inline = struct
  type t = [ | Css_data_type.Length.t ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
  ;;
end

module Scroll_margin_inline_end = struct
  type t = [ | Css_data_type.Length.t ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
  ;;
end

module Scroll_margin_inline_start = struct
  type t = [ | Css_data_type.Length.t ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
  ;;
end

module Scroll_margin_left = struct
  type t = [ | Css_data_type.Length.t ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
  ;;
end

module Scroll_margin_right = struct
  type t = [ | Css_data_type.Length.t ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
  ;;
end

module Scroll_margin_top = struct
  type t = [ | Css_data_type.Length.t ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
  ;;
end

module Scroll_marker_group = struct
  type t =
    [ `After
    | `Before
    | `None
    ]

  let to_string_css = function
    | `After -> "after"
    | `Before -> "before"
    | `None -> "none"
  ;;
end

module Scroll_padding = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Scroll_padding_block = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Scroll_padding_block_end = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Scroll_padding_block_start = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Scroll_padding_bottom = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Scroll_padding_inline = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Scroll_padding_inline_end = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Scroll_padding_inline_start = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Scroll_padding_left = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Scroll_padding_right = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Scroll_padding_top = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Scroll_snap_align = struct
  type t =
    [ `Center
    | `End
    | `None
    | `Start
    ]

  let to_string_css = function
    | `Center -> "center"
    | `End -> "end"
    | `None -> "none"
    | `Start -> "start"
  ;;
end

module Scroll_snap_stop = struct
  type t =
    [ `Always
    | `Normal
    ]

  let to_string_css = function
    | `Always -> "always"
    | `Normal -> "normal"
  ;;
end

module Scroll_snap_type = struct
  type t =
    [ `Block
    | `Both
    | `Inline
    | `Mandatory
    | `None
    | `Proximity
    | `X
    | `Y
    ]

  let to_string_css = function
    | `Block -> "block"
    | `Both -> "both"
    | `Inline -> "inline"
    | `Mandatory -> "mandatory"
    | `None -> "none"
    | `Proximity -> "proximity"
    | `X -> "x"
    | `Y -> "y"
  ;;
end

module Scroll_target_group = struct
  type t =
    [ `Auto
    | `None
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `None -> "none"
  ;;
end

module Scroll_timeline_axis = struct
  type t =
    [ `Block
    | `Inline
    | `X
    | `Y
    ]

  let to_string_css = function
    | `Block -> "block"
    | `Inline -> "inline"
    | `X -> "x"
    | `Y -> "y"
  ;;
end

module Scroll_timeline_name = struct
  type t =
    [ Css_data_type.Ident.Dashed.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Dashed.t as t -> Css_data_type.Ident.Dashed.to_string_css t
    | `None -> "none"
  ;;
end

module Scroll_timeline = struct
  type t =
    [ Scroll_timeline_axis.t
    | Scroll_timeline_name.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Scroll_timeline_axis.t as t -> Scroll_timeline_axis.to_string_css t
    | #Scroll_timeline_name.t as t -> Scroll_timeline_name.to_string_css t
  ;;
end

module Scrollbar_color = struct
  type t =
    [ Css_data_type.Color.t
    | `Auto
    ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Scrollbar_gutter = struct
  type t =
    [ `Auto
    | `Both_edges
    | `Stable
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Both_edges -> "both-edges"
    | `Stable -> "stable"
  ;;
end

module Scrollbar_width = struct
  type t =
    [ `Auto
    | `None
    | `Thin
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `None -> "none"
    | `Thin -> "thin"
  ;;
end

module Shape_image_threshold = struct
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
  ;;
end

module Shape_margin = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Shape_padding = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Shape_rendering = struct
  type t =
    [ `Auto
    | `CrispEdges
    | `GeometricPrecision
    | `OptimizeSpeed
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `CrispEdges -> "crispEdges"
    | `GeometricPrecision -> "geometricPrecision"
    | `OptimizeSpeed -> "optimizeSpeed"
  ;;
end

module Slider_orientation = struct
  type t =
    [ `Auto
    | `Bottom_to_top
    | `Left_to_right
    | `Right_to_left
    | `Top_to_bottom
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Bottom_to_top -> "bottom-to-top"
    | `Left_to_right -> "left-to-right"
    | `Right_to_left -> "right-to-left"
    | `Top_to_bottom -> "top-to-bottom"
  ;;
end

module Spatial_navigation_action = struct
  type t =
    [ `Auto
    | `Focus
    | `Scroll
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Focus -> "focus"
    | `Scroll -> "scroll"
  ;;
end

module Spatial_navigation_contain = struct
  type t =
    [ `Auto
    | `Contain
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Contain -> "contain"
  ;;
end

module Spatial_navigation_function = struct
  type t =
    [ `Grid
    | `Normal
    ]

  let to_string_css = function
    | `Grid -> "grid"
    | `Normal -> "normal"
  ;;
end

module Speak = struct
  type t =
    [ `Always
    | `Auto
    | `Never
    ]

  let to_string_css = function
    | `Always -> "always"
    | `Auto -> "auto"
    | `Never -> "never"
  ;;
end

module Speak_as = struct
  type t =
    [ `Digits
    | `Literal_punctuation
    | `No_punctuation
    | `Normal
    | `Spell_out
    ]

  let to_string_css = function
    | `Digits -> "digits"
    | `Literal_punctuation -> "literal-punctuation"
    | `No_punctuation -> "no-punctuation"
    | `Normal -> "normal"
    | `Spell_out -> "spell-out"
  ;;
end

module String_set = struct
  type t =
    [ Css_data_type.Ident.Custom.t
    | Css_data_type.Css_string.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Custom.t as t -> Css_data_type.Ident.Custom.to_string_css t
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
    | `None -> "none"
  ;;
end

module Stroke_align = struct
  type t =
    [ `Center
    | `Inset
    | `Outset
    ]

  let to_string_css = function
    | `Center -> "center"
    | `Inset -> "inset"
    | `Outset -> "outset"
  ;;
end

module Stroke_alignment = struct
  type t =
    [ `Center
    | `Inner
    | `Outer
    ]

  let to_string_css = function
    | `Center -> "center"
    | `Inner -> "inner"
    | `Outer -> "outer"
  ;;
end

module Stroke_break = struct
  type t =
    [ `Bounding_box
    | `Clone
    | `Slice
    ]

  let to_string_css = function
    | `Bounding_box -> "bounding-box"
    | `Clone -> "clone"
    | `Slice -> "slice"
  ;;
end

module Stroke_color = struct
  type t = [ | Css_data_type.Color.t ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
  ;;
end

module Stroke_dash_corner = struct
  type t =
    [ Css_data_type.Length.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | `None -> "none"
  ;;
end

module Stroke_dash_justify = struct
  type t =
    [ `Compress
    | `Dashes
    | `Gaps
    | `None
    | `Stretch
    ]

  let to_string_css = function
    | `Compress -> "compress"
    | `Dashes -> "dashes"
    | `Gaps -> "gaps"
    | `None -> "none"
    | `Stretch -> "stretch"
  ;;
end

module Stroke_dashadjust = struct
  type t =
    [ `Compress
    | `Dashes
    | `Gaps
    | `None
    | `Stretch
    ]

  let to_string_css = function
    | `Compress -> "compress"
    | `Dashes -> "dashes"
    | `Gaps -> "gaps"
    | `None -> "none"
    | `Stretch -> "stretch"
  ;;
end

module Stroke_dasharray = struct
  type t =
    [ Length_percentage_type.t
    | Css_data_type.Number.t
    | `None
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | `None -> "none"
  ;;
end

module Stroke_dashcorner = struct
  type t =
    [ Css_data_type.Length.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | `None -> "none"
  ;;
end

module Stroke_dashoffset = struct
  type t =
    [ Length_percentage_type.t
    | Css_data_type.Number.t
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
  ;;
end

module Stroke_linecap = struct
  type t =
    [ `Butt
    | `Round
    | `Square
    ]

  let to_string_css = function
    | `Butt -> "butt"
    | `Round -> "round"
    | `Square -> "square"
  ;;
end

module Stroke_linejoin = struct
  type t =
    [ `Arcs
    | `Bevel
    | `Crop
    | `Fallback
    | `Miter
    | `Round
    ]

  let to_string_css = function
    | `Arcs -> "arcs"
    | `Bevel -> "bevel"
    | `Crop -> "crop"
    | `Fallback -> "fallback"
    | `Miter -> "miter"
    | `Round -> "round"
  ;;
end

module Stroke_miterlimit = struct
  type t = [ | Css_data_type.Number.t ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
  ;;
end

module Stroke_opacity = struct
  type t = [ | Opacity.t ]

  let to_string_css = function
    | #Opacity.t as t -> Opacity.to_string_css t
  ;;
end

module Stroke_origin = struct
  type t =
    [ `Border_box
    | `Content_box
    | `Fill_box
    | `Match_parent
    | `Padding_box
    | `Stroke_box
    ]

  let to_string_css = function
    | `Border_box -> "border-box"
    | `Content_box -> "content-box"
    | `Fill_box -> "fill-box"
    | `Match_parent -> "match-parent"
    | `Padding_box -> "padding-box"
    | `Stroke_box -> "stroke-box"
  ;;
end

module Stroke_position = struct
  type t = [ | Position_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Position_type.t as t -> Position_type.to_string_css t
  ;;
end

module Stroke_repeat = struct
  type t = [ | Repeat_style_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Repeat_style_type.t as t -> Repeat_style_type.to_string_css t
  ;;
end

module Stroke_size = struct
  type t = [ | Bg_size_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Bg_size_type.t as t -> Bg_size_type.to_string_css t
  ;;
end

module Stroke_width = struct
  type t =
    [ Length_percentage_type.t
    | Css_data_type.Number.t
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
  ;;
end

module Tab_size = struct
  type t =
    [ Css_data_type.Length.t
    | Css_data_type.Number.t
    ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
  ;;
end

module Table_layout = struct
  type t =
    [ `Auto
    | `Fixed
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Fixed -> "fixed"
  ;;
end

module Text_align = struct
  type t =
    [ Css_data_type.Css_string.t
    | `Center
    | `End
    | `Justify
    | `Justify_all
    | `Left
    | `Match_parent
    | `Right
    | `Start
    ]

  let to_string_css = function
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
    | `Center -> "center"
    | `End -> "end"
    | `Justify -> "justify"
    | `Justify_all -> "justify-all"
    | `Left -> "left"
    | `Match_parent -> "match-parent"
    | `Right -> "right"
    | `Start -> "start"
  ;;
end

module Text_align_all = struct
  type t =
    [ Css_data_type.Css_string.t
    | `Center
    | `End
    | `Justify
    | `Left
    | `Match_parent
    | `Right
    | `Start
    ]

  let to_string_css = function
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
    | `Center -> "center"
    | `End -> "end"
    | `Justify -> "justify"
    | `Left -> "left"
    | `Match_parent -> "match-parent"
    | `Right -> "right"
    | `Start -> "start"
  ;;
end

module Text_align_last = struct
  type t =
    [ `Auto
    | `Center
    | `End
    | `Justify
    | `Left
    | `Match_parent
    | `Right
    | `Start
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Center -> "center"
    | `End -> "end"
    | `Justify -> "justify"
    | `Left -> "left"
    | `Match_parent -> "match-parent"
    | `Right -> "right"
    | `Start -> "start"
  ;;
end

module Text_anchor = struct
  type t =
    [ `End
    | `Middle
    | `Start
    ]

  let to_string_css = function
    | `End -> "end"
    | `Middle -> "middle"
    | `Start -> "start"
  ;;
end

module Text_autospace = struct
  type t =
    [ Autospace_type.t
    | `Auto
    | `Normal
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Autospace_type.t as t -> Autospace_type.to_string_css t
    | `Auto -> "auto"
    | `Normal -> "normal"
  ;;
end

module Text_box_edge = struct
  type t =
    [ Text_edge_type.t
    | `Auto
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Text_edge_type.t as t -> Text_edge_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Text_box_trim = struct
  type t =
    [ `None
    | `Trim_both
    | `Trim_end
    | `Trim_start
    ]

  let to_string_css = function
    | `None -> "none"
    | `Trim_both -> "trim-both"
    | `Trim_end -> "trim-end"
    | `Trim_start -> "trim-start"
  ;;
end

module Text_box = struct
  type t =
    [ `Normal
    | Text_box_edge.t
    | Text_box_trim.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | `Normal -> "normal"
    | #Text_box_edge.t as t -> Text_box_edge.to_string_css t
    | #Text_box_trim.t as t -> Text_box_trim.to_string_css t
  ;;
end

module Text_combine_upright = struct
  type t =
    [ Css_data_type.Integer.t
    | `All
    | `Digits
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | `All -> "all"
    | `Digits -> "digits"
    | `None -> "none"
  ;;
end

module Text_decoration_color = struct
  type t = [ | Css_data_type.Color.t ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
  ;;
end

module Text_decoration_line = struct
  type t =
    [ `Blink
    | `Grammar_error
    | `Line_through
    | `None
    | `Overline
    | `Spelling_error
    | `Underline
    ]

  let to_string_css = function
    | `Blink -> "blink"
    | `Grammar_error -> "grammar-error"
    | `Line_through -> "line-through"
    | `None -> "none"
    | `Overline -> "overline"
    | `Spelling_error -> "spelling-error"
    | `Underline -> "underline"
  ;;
end

module Text_decoration_skip = struct
  type t =
    [ `Auto
    | `None
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `None -> "none"
  ;;
end

module Text_decoration_skip_box = struct
  type t =
    [ `All
    | `None
    ]

  let to_string_css = function
    | `All -> "all"
    | `None -> "none"
  ;;
end

module Text_decoration_skip_ink = struct
  type t =
    [ `All
    | `Auto
    | `None
    ]

  let to_string_css = function
    | `All -> "all"
    | `Auto -> "auto"
    | `None -> "none"
  ;;
end

module Text_decoration_skip_self = struct
  type t =
    [ `Auto
    | `No_skip
    | `Skip_all
    | `Skip_line_through
    | `Skip_overline
    | `Skip_underline
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `No_skip -> "no-skip"
    | `Skip_all -> "skip-all"
    | `Skip_line_through -> "skip-line-through"
    | `Skip_overline -> "skip-overline"
    | `Skip_underline -> "skip-underline"
  ;;
end

module Text_decoration_skip_spaces = struct
  type t =
    [ `All
    | `End
    | `None
    | `Start
    ]

  let to_string_css = function
    | `All -> "all"
    | `End -> "end"
    | `None -> "none"
    | `Start -> "start"
  ;;
end

module Text_decoration_style = struct
  type t =
    [ `Dashed
    | `Dotted
    | `Double
    | `Solid
    | `Wavy
    ]

  let to_string_css = function
    | `Dashed -> "dashed"
    | `Dotted -> "dotted"
    | `Double -> "double"
    | `Solid -> "solid"
    | `Wavy -> "wavy"
  ;;
end

module Text_decoration_thickness = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    | `From_font
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
    | `From_font -> "from-font"
  ;;
end

module Text_decoration = struct
  type t =
    [ Text_decoration_color.t
    | Text_decoration_line.t
    | Text_decoration_style.t
    | Text_decoration_thickness.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Text_decoration_color.t as t -> Text_decoration_color.to_string_css t
    | #Text_decoration_line.t as t -> Text_decoration_line.to_string_css t
    | #Text_decoration_style.t as t -> Text_decoration_style.to_string_css t
    | #Text_decoration_thickness.t as t -> Text_decoration_thickness.to_string_css t
  ;;
end

module Text_decoration_trim = struct
  type t =
    [ Css_data_type.Length.t
    | `Auto
    ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Text_emphasis_color = struct
  type t = [ | Css_data_type.Color.t ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
  ;;
end

module Text_emphasis_position = struct
  type t =
    [ `Left
    | `Over
    | `Right
    | `Under
    ]

  let to_string_css = function
    | `Left -> "left"
    | `Over -> "over"
    | `Right -> "right"
    | `Under -> "under"
  ;;
end

module Text_emphasis_skip = struct
  type t =
    [ `Narrow
    | `Punctuation
    | `Spaces
    | `Symbols
    ]

  let to_string_css = function
    | `Narrow -> "narrow"
    | `Punctuation -> "punctuation"
    | `Spaces -> "spaces"
    | `Symbols -> "symbols"
  ;;
end

module Text_emphasis_style = struct
  type t =
    [ Css_data_type.Css_string.t
    | `Circle
    | `Dot
    | `Double_circle
    | `Filled
    | `None
    | `Open
    | `Sesame
    | `Triangle
    ]

  let to_string_css = function
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
    | `Circle -> "circle"
    | `Dot -> "dot"
    | `Double_circle -> "double-circle"
    | `Filled -> "filled"
    | `None -> "none"
    | `Open -> "open"
    | `Sesame -> "sesame"
    | `Triangle -> "triangle"
  ;;
end

module Text_emphasis = struct
  type t =
    [ Text_emphasis_color.t
    | Text_emphasis_style.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Text_emphasis_color.t as t -> Text_emphasis_color.to_string_css t
    | #Text_emphasis_style.t as t -> Text_emphasis_style.to_string_css t
  ;;
end

module Text_group_align = struct
  type t =
    [ `Center
    | `End
    | `Left
    | `None
    | `Right
    | `Start
    ]

  let to_string_css = function
    | `Center -> "center"
    | `End -> "end"
    | `Left -> "left"
    | `None -> "none"
    | `Right -> "right"
    | `Start -> "start"
  ;;
end

module Text_indent = struct
  type t =
    [ Length_percentage_type.t
    | `Each_line
    | `Hanging
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Each_line -> "each-line"
    | `Hanging -> "hanging"
  ;;
end

module Text_justify = struct
  type t =
    [ `Auto
    | `Inter_character
    | `Inter_word
    | `No_compress
    | `None
    | `Ruby
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Inter_character -> "inter-character"
    | `Inter_word -> "inter-word"
    | `No_compress -> "no-compress"
    | `None -> "none"
    | `Ruby -> "ruby"
  ;;
end

module Text_orientation = struct
  type t =
    [ `Mixed
    | `Sideways
    | `Upright
    ]

  let to_string_css = function
    | `Mixed -> "mixed"
    | `Sideways -> "sideways"
    | `Upright -> "upright"
  ;;
end

module Text_rendering = struct
  type t =
    [ `Auto
    | `GeometricPrecision
    | `OptimizeLegibility
    | `OptimizeSpeed
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `GeometricPrecision -> "geometricPrecision"
    | `OptimizeLegibility -> "optimizeLegibility"
    | `OptimizeSpeed -> "optimizeSpeed"
  ;;
end

module Text_shadow = struct
  type t =
    [ Css_data_type.Color.t
    | Css_data_type.Length.t
    | `Inset
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | `Inset -> "inset"
    | `None -> "none"
  ;;
end

module Text_size_adjust = struct
  type t =
    [ Css_data_type.Percentage.t
    | `Auto
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `Auto -> "auto"
    | `None -> "none"
  ;;
end

module Text_spacing = struct
  type t =
    [ Autospace_type.t
    | Spacing_trim_type.t
    | `Auto
    | `None
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Autospace_type.t as t -> Autospace_type.to_string_css t
    | #Spacing_trim_type.t as t -> Spacing_trim_type.to_string_css t
    | `Auto -> "auto"
    | `None -> "none"
  ;;
end

module Text_spacing_trim = struct
  type t =
    [ Spacing_trim_type.t
    | `Auto
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Spacing_trim_type.t as t -> Spacing_trim_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Text_transform = struct
  type t =
    [ `Capitalize
    | `Full_size_kana
    | `Full_width
    | `Lowercase
    | `Math_auto
    | `None
    | `Uppercase
    ]

  let to_string_css = function
    | `Capitalize -> "capitalize"
    | `Full_size_kana -> "full-size-kana"
    | `Full_width -> "full-width"
    | `Lowercase -> "lowercase"
    | `Math_auto -> "math-auto"
    | `None -> "none"
    | `Uppercase -> "uppercase"
  ;;
end

module Text_underline_offset = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Text_underline_position = struct
  type t =
    [ `Auto
    | `From_font
    | `Left
    | `Right
    | `Under
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `From_font -> "from-font"
    | `Left -> "left"
    | `Right -> "right"
    | `Under -> "under"
  ;;
end

module Text_wrap_mode = struct
  type t =
    [ `Nowrap
    | `Wrap
    ]

  let to_string_css = function
    | `Nowrap -> "nowrap"
    | `Wrap -> "wrap"
  ;;
end

module Text_wrap_style = struct
  type t =
    [ `Auto
    | `Avoid_orphans
    | `Balance
    | `Pretty
    | `Stable
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Avoid_orphans -> "avoid-orphans"
    | `Balance -> "balance"
    | `Pretty -> "pretty"
    | `Stable -> "stable"
  ;;
end

module Text_wrap = struct
  type t =
    [ Text_wrap_mode.t
    | Text_wrap_style.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Text_wrap_mode.t as t -> Text_wrap_mode.to_string_css t
    | #Text_wrap_style.t as t -> Text_wrap_style.to_string_css t
  ;;
end

module Timeline_scope = struct
  type t =
    [ Css_data_type.Ident.Dashed.t
    | `All
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Dashed.t as t -> Css_data_type.Ident.Dashed.to_string_css t
    | `All -> "all"
    | `None -> "none"
  ;;
end

module Touch_action = struct
  type t =
    [ `Auto
    | `Manipulation
    | `None
    | `Pan_down
    | `Pan_left
    | `Pan_right
    | `Pan_up
    | `Pan_x
    | `Pan_y
    | `Pinch_zoom
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Manipulation -> "manipulation"
    | `None -> "none"
    | `Pan_down -> "pan-down"
    | `Pan_left -> "pan-left"
    | `Pan_right -> "pan-right"
    | `Pan_up -> "pan-up"
    | `Pan_x -> "pan-x"
    | `Pan_y -> "pan-y"
    | `Pinch_zoom -> "pinch-zoom"
  ;;
end

module Transform_box = struct
  type t =
    [ `Border_box
    | `Content_box
    | `Fill_box
    | `Stroke_box
    | `View_box
    ]

  let to_string_css = function
    | `Border_box -> "border-box"
    | `Content_box -> "content-box"
    | `Fill_box -> "fill-box"
    | `Stroke_box -> "stroke-box"
    | `View_box -> "view-box"
  ;;
end

module Transform_origin = struct
  type t =
    [ Css_data_type.Length.t
    | Length_percentage_type.t
    | `Bottom
    | `Center
    | `Left
    | `Right
    | `Top
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Bottom -> "bottom"
    | `Center -> "center"
    | `Left -> "left"
    | `Right -> "right"
    | `Top -> "top"
  ;;
end

module Transform_style = struct
  type t =
    [ `Flat
    | `Preserve_3d
    ]

  let to_string_css = function
    | `Flat -> "flat"
    | `Preserve_3d -> "preserve-3d"
  ;;
end

module Transition_behavior = struct
  type t = [ | Transition_behavior_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Transition_behavior_value_type.t as t ->
      Transition_behavior_value_type.to_string_css t
  ;;
end

module Transition_delay = struct
  type t = [ | Css_data_type.Time.t ]

  let to_string_css = function
    | #Css_data_type.Time.t as t -> Css_data_type.Time.to_string_css t
  ;;
end

module Transition_duration = struct
  type t = [ | Css_data_type.Time.t ]

  let to_string_css = function
    | #Css_data_type.Time.t as t -> Css_data_type.Time.to_string_css t
  ;;
end

module Transition_property = struct
  type t =
    [ Single_transition_property_type.t
    | `None
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Single_transition_property_type.t as t ->
      Single_transition_property_type.to_string_css t
    | `None -> "none"
  ;;
end

module Translate = struct
  type t =
    [ Css_data_type.Length.t
    | Length_percentage_type.t
    | `None
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `None -> "none"
  ;;
end

module Unicode_bidi = struct
  type t =
    [ `Bidi_override
    | `Embed
    | `Isolate
    | `Isolate_override
    | `Normal
    | `Plaintext
    ]

  let to_string_css = function
    | `Bidi_override -> "bidi-override"
    | `Embed -> "embed"
    | `Isolate -> "isolate"
    | `Isolate_override -> "isolate-override"
    | `Normal -> "normal"
    | `Plaintext -> "plaintext"
  ;;
end

module User_select = struct
  type t =
    [ `All
    | `Auto
    | `Contain
    | `None
    | `Text
    ]

  let to_string_css = function
    | `All -> "all"
    | `Auto -> "auto"
    | `Contain -> "contain"
    | `None -> "none"
    | `Text -> "text"
  ;;
end

module Vector_effect = struct
  type t =
    [ `Fixed_position
    | `Non_rotation
    | `Non_scaling_size
    | `Non_scaling_stroke
    | `None
    ]

  let to_string_css = function
    | `Fixed_position -> "fixed-position"
    | `Non_rotation -> "non-rotation"
    | `Non_scaling_size -> "non-scaling-size"
    | `Non_scaling_stroke -> "non-scaling-stroke"
    | `None -> "none"
  ;;
end

module Vertical_align = struct
  type t =
    [ `First
    | `Last
    | Alignment_baseline.t
    | Baseline_shift.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | `First -> "first"
    | `Last -> "last"
    | #Alignment_baseline.t as t -> Alignment_baseline.to_string_css t
    | #Baseline_shift.t as t -> Baseline_shift.to_string_css t
  ;;
end

module View_timeline_axis = struct
  type t =
    [ `Block
    | `Inline
    | `X
    | `Y
    ]

  let to_string_css = function
    | `Block -> "block"
    | `Inline -> "inline"
    | `X -> "x"
    | `Y -> "y"
  ;;
end

module View_timeline_inset = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module View_timeline_name = struct
  type t =
    [ Css_data_type.Ident.Dashed.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Dashed.t as t -> Css_data_type.Ident.Dashed.to_string_css t
    | `None -> "none"
  ;;
end

module View_timeline = struct
  type t =
    [ View_timeline_axis.t
    | View_timeline_inset.t
    | View_timeline_name.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #View_timeline_axis.t as t -> View_timeline_axis.to_string_css t
    | #View_timeline_inset.t as t -> View_timeline_inset.to_string_css t
    | #View_timeline_name.t as t -> View_timeline_name.to_string_css t
  ;;
end

module View_transition_class = struct
  type t =
    [ Css_data_type.Ident.Custom.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Custom.t as t -> Css_data_type.Ident.Custom.to_string_css t
    | `None -> "none"
  ;;
end

module View_transition_group = struct
  type t =
    [ Css_data_type.Ident.Custom.t
    | `Contain
    | `Nearest
    | `Normal
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Custom.t as t -> Css_data_type.Ident.Custom.to_string_css t
    | `Contain -> "contain"
    | `Nearest -> "nearest"
    | `Normal -> "normal"
  ;;
end

module View_transition_name = struct
  type t =
    [ Css_data_type.Ident.Custom.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Custom.t as t -> Css_data_type.Ident.Custom.to_string_css t
    | `None -> "none"
  ;;
end

module Visibility = struct
  type t =
    [ `Collapse
    | `Force_hidden
    | `Hidden
    | `Visible
    ]

  let to_string_css = function
    | `Collapse -> "collapse"
    | `Force_hidden -> "force-hidden"
    | `Hidden -> "hidden"
    | `Visible -> "visible"
  ;;
end

module Voice_balance = struct
  type t =
    [ Css_data_type.Number.t
    | `Center
    | `Left
    | `Leftwards
    | `Right
    | `Rightwards
    ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | `Center -> "center"
    | `Left -> "left"
    | `Leftwards -> "leftwards"
    | `Right -> "right"
    | `Rightwards -> "rightwards"
  ;;
end

module Voice_duration = struct
  type t =
    [ Css_data_type.Time.t
    | `Auto
    ]

  let to_string_css = function
    | #Css_data_type.Time.t as t -> Css_data_type.Time.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Voice_family = struct
  type t =
    [ Family_name_type.t
    | Css_data_type.Integer.t
    | `Preserve
    ]

  let to_string_css = function
    | #Family_name_type.t as t -> Family_name_type.to_string_css t
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | `Preserve -> "preserve"
  ;;
end

module Voice_pitch = struct
  type t =
    [ Css_data_type.Frequency.t
    | Css_data_type.Percentage.t
    | `Absolute
    | `High
    | `Low
    | `Medium
    | `X_high
    | `X_low
    ]

  let to_string_css = function
    | #Css_data_type.Frequency.t as t -> Css_data_type.Frequency.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `Absolute -> "absolute"
    | `High -> "high"
    | `Low -> "low"
    | `Medium -> "medium"
    | `X_high -> "x-high"
    | `X_low -> "x-low"
  ;;
end

module Voice_range = struct
  type t =
    [ Css_data_type.Frequency.t
    | Css_data_type.Percentage.t
    | `Absolute
    | `High
    | `Low
    | `Medium
    | `X_high
    | `X_low
    ]

  let to_string_css = function
    | #Css_data_type.Frequency.t as t -> Css_data_type.Frequency.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `Absolute -> "absolute"
    | `High -> "high"
    | `Low -> "low"
    | `Medium -> "medium"
    | `X_high -> "x-high"
    | `X_low -> "x-low"
  ;;
end

module Voice_rate = struct
  type t =
    [ Css_data_type.Percentage.t
    | `Fast
    | `Medium
    | `Normal
    | `Slow
    | `X_fast
    | `X_slow
    ]

  let to_string_css = function
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `Fast -> "fast"
    | `Medium -> "medium"
    | `Normal -> "normal"
    | `Slow -> "slow"
    | `X_fast -> "x-fast"
    | `X_slow -> "x-slow"
  ;;
end

module Voice_stress = struct
  type t =
    [ `Moderate
    | `None
    | `Normal
    | `Reduced
    | `Strong
    ]

  let to_string_css = function
    | `Moderate -> "moderate"
    | `None -> "none"
    | `Normal -> "normal"
    | `Reduced -> "reduced"
    | `Strong -> "strong"
  ;;
end

module Voice_volume = struct
  type t =
    [ Css_data_type.Decibel.t
    | `Loud
    | `Medium
    | `Silent
    | `Soft
    | `X_loud
    | `X_soft
    ]

  let to_string_css = function
    | #Css_data_type.Decibel.t as t -> Css_data_type.Decibel.to_string_css t
    | `Loud -> "loud"
    | `Medium -> "medium"
    | `Silent -> "silent"
    | `Soft -> "soft"
    | `X_loud -> "x-loud"
    | `X_soft -> "x-soft"
  ;;
end

module White_space_collapse = struct
  type t =
    [ `Break_spaces
    | `Collapse
    | `Discard
    | `Preserve
    | `Preserve_breaks
    | `Preserve_spaces
    ]

  let to_string_css = function
    | `Break_spaces -> "break-spaces"
    | `Collapse -> "collapse"
    | `Discard -> "discard"
    | `Preserve -> "preserve"
    | `Preserve_breaks -> "preserve-breaks"
    | `Preserve_spaces -> "preserve-spaces"
  ;;
end

module White_space_trim = struct
  type t =
    [ `Discard_after
    | `Discard_before
    | `Discard_inner
    | `None
    ]

  let to_string_css = function
    | `Discard_after -> "discard-after"
    | `Discard_before -> "discard-before"
    | `Discard_inner -> "discard-inner"
    | `None -> "none"
  ;;
end

module White_space = struct
  type t =
    [ `Normal
    | `Pre
    | `Pre_line
    | `Pre_wrap
    | Text_wrap_mode.t
    | White_space_collapse.t
    | White_space_trim.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | `Normal -> "normal"
    | `Pre -> "pre"
    | `Pre_line -> "pre-line"
    | `Pre_wrap -> "pre-wrap"
    | #Text_wrap_mode.t as t -> Text_wrap_mode.to_string_css t
    | #White_space_collapse.t as t -> White_space_collapse.to_string_css t
    | #White_space_trim.t as t -> White_space_trim.to_string_css t
  ;;
end

module Widows = struct
  type t = [ | Css_data_type.Integer.t ]

  let to_string_css = function
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
  ;;
end

module Will_change = struct
  type t =
    [ Css_data_type.Ident.Custom.t
    | `Auto
    | `Contents
    | `Scroll_position
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Custom.t as t -> Css_data_type.Ident.Custom.to_string_css t
    | `Auto -> "auto"
    | `Contents -> "contents"
    | `Scroll_position -> "scroll-position"
  ;;
end

module Word_break = struct
  type t =
    [ `Auto_phrase
    | `Break_all
    | `Break_word
    | `Keep_all
    | `Manual
    | `Normal
    ]

  let to_string_css = function
    | `Auto_phrase -> "auto-phrase"
    | `Break_all -> "break-all"
    | `Break_word -> "break-word"
    | `Keep_all -> "keep-all"
    | `Manual -> "manual"
    | `Normal -> "normal"
  ;;
end

module Word_space_transform = struct
  type t =
    [ `Auto_phrase
    | `Ideographic_space
    | `None
    | `Space
    ]

  let to_string_css = function
    | `Auto_phrase -> "auto-phrase"
    | `Ideographic_space -> "ideographic-space"
    | `None -> "none"
    | `Space -> "space"
  ;;
end

module Word_spacing = struct
  type t =
    [ Length_percentage_type.t
    | `Normal
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Normal -> "normal"
  ;;
end

module Word_wrap = struct
  type t =
    [ `Anywhere
    | `Break_word
    | `Normal
    ]

  let to_string_css = function
    | `Anywhere -> "anywhere"
    | `Break_word -> "break-word"
    | `Normal -> "normal"
  ;;
end

module Wrap_after = struct
  type t =
    [ `Auto
    | `Avoid
    | `Avoid_flex
    | `Avoid_line
    | `Flex
    | `Line
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Avoid -> "avoid"
    | `Avoid_flex -> "avoid-flex"
    | `Avoid_line -> "avoid-line"
    | `Flex -> "flex"
    | `Line -> "line"
  ;;
end

module Wrap_before = struct
  type t =
    [ `Auto
    | `Avoid
    | `Avoid_flex
    | `Avoid_line
    | `Flex
    | `Line
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Avoid -> "avoid"
    | `Avoid_flex -> "avoid-flex"
    | `Avoid_line -> "avoid-line"
    | `Flex -> "flex"
    | `Line -> "line"
  ;;
end

module Wrap_flow = struct
  type t =
    [ `Auto
    | `Both
    | `Clear
    | `End
    | `Maximum
    | `Minimum
    | `Start
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Both -> "both"
    | `Clear -> "clear"
    | `End -> "end"
    | `Maximum -> "maximum"
    | `Minimum -> "minimum"
    | `Start -> "start"
  ;;
end

module Wrap_inside = struct
  type t =
    [ `Auto
    | `Avoid
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Avoid -> "avoid"
  ;;
end

module Wrap_through = struct
  type t =
    [ `None
    | `Wrap
    ]

  let to_string_css = function
    | `None -> "none"
    | `Wrap -> "wrap"
  ;;
end

module Writing_mode = struct
  type t =
    [ `Horizontal_tb
    | `Sideways_lr
    | `Sideways_rl
    | `Vertical_lr
    | `Vertical_rl
    ]

  let to_string_css = function
    | `Horizontal_tb -> "horizontal-tb"
    | `Sideways_lr -> "sideways-lr"
    | `Sideways_rl -> "sideways-rl"
    | `Vertical_lr -> "vertical-lr"
    | `Vertical_rl -> "vertical-rl"
  ;;
end

module X = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Y = struct
  type t = [ | Length_percentage_type.t ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Z_index = struct
  type t =
    [ Css_data_type.Integer.t
    | `Auto
    | `Inherit
    ]

  let to_string_css = function
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | `Auto -> "auto"
    | `Inherit -> "inherit"
  ;;
end

module Zoom = struct
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
  ;;
end

module Abs_fn = struct
  type t = [ | Calc_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
  ;;
end

module Acos_fn = struct
  type t = [ | Calc_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
  ;;
end

module Anchor_fn = struct
  type t =
    [ Anchor_name_type.t
    | Length_percentage_type.t
    | Css_data_type.Percentage.t
    | `Bottom
    | `Center
    | `End
    | `Inside
    | `Left
    | `Outside
    | `Right
    | `Self_end
    | `Self_start
    | `Start
    | `Top
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Anchor_name_type.t as t -> Anchor_name_type.to_string_css t
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `Bottom -> "bottom"
    | `Center -> "center"
    | `End -> "end"
    | `Inside -> "inside"
    | `Left -> "left"
    | `Outside -> "outside"
    | `Right -> "right"
    | `Self_end -> "self-end"
    | `Self_start -> "self-start"
    | `Start -> "start"
    | `Top -> "top"
  ;;
end

module Anchor_size_fn = struct
  type t =
    [ Anchor_name_type.t
    | Length_percentage_type.t
    | `Block
    | `Height
    | `Inline
    | `Self_block
    | `Self_inline
    | `Width
    ]

  let to_string_css = function
    | #Anchor_name_type.t as t -> Anchor_name_type.to_string_css t
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Block -> "block"
    | `Height -> "height"
    | `Inline -> "inline"
    | `Self_block -> "self-block"
    | `Self_inline -> "self-inline"
    | `Width -> "width"
  ;;
end

module Bottom = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Left = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Margin_bottom = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Margin_left = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Margin_right = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Margin_top = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Margin = struct
  type t = [ | Margin_top.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Margin_top.t as t -> Margin_top.to_string_css t
  ;;
end

module Margin_block = struct
  type t = [ | Margin_top.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Margin_top.t as t -> Margin_top.to_string_css t
  ;;
end

module Margin_block_end = struct
  type t = [ | Margin_top.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Margin_top.t as t -> Margin_top.to_string_css t
  ;;
end

module Margin_block_start = struct
  type t = [ | Margin_top.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Margin_top.t as t -> Margin_top.to_string_css t
  ;;
end

module Margin_inline = struct
  type t = [ | Margin_top.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Margin_top.t as t -> Margin_top.to_string_css t
  ;;
end

module Margin_inline_end = struct
  type t = [ | Margin_top.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Margin_top.t as t -> Margin_top.to_string_css t
  ;;
end

module Margin_inline_start = struct
  type t = [ | Margin_top.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Margin_top.t as t -> Margin_top.to_string_css t
  ;;
end

module Right = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Top = struct
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Inset = struct
  type t = [ | Top.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Top.t as t -> Top.to_string_css t
  ;;
end

module Inset_block = struct
  type t = [ | Top.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Top.t as t -> Top.to_string_css t
  ;;
end

module Inset_inline = struct
  type t = [ | Top.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Top.t as t -> Top.to_string_css t
  ;;
end

module Asin_fn = struct
  type t = [ | Calc_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
  ;;
end

module Atan_fn = struct
  type t = [ | Calc_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
  ;;
end

module Atan2_fn = struct
  type t = [ | Calc_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
  ;;
end

module Calc_fn = struct
  type t = [ | Calc_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
  ;;
end

module Calc_mix_fn = struct
  type t =
    [ Calc_value_type.t
    | Css_data_type.Percentage.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
  ;;
end

module Calc_size_fn = struct
  type t =
    [ Calc_value_type.t
    | `Any
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
    | `Any -> "any"
  ;;
end

module Calc_size_basis_type = struct
  type t =
    [ Calc_value_type.t
    | `Any
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
    | `Any -> "any"
  ;;
end

module Clamp_fn = struct
  type t =
    [ Calc_value_type.t
    | `None
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
    | `None -> "none"
  ;;
end

module Color_fn = struct
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Ident.Dashed.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `A98_rgb
    | `Display_p3
    | `From
    | `None
    | `Prophoto_rgb
    | `Rec2020
    | `Rec2100_hlg
    | `Rec2100_linear
    | `Rec2100_pq
    | `Srgb
    | `Srgb_linear
    | `Xyz
    | `Xyz_d50
    | `Xyz_d65
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Alpha_value_type.t as t -> Alpha_value_type.to_string_css t
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Css_data_type.Ident.Dashed.t as t -> Css_data_type.Ident.Dashed.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `A98_rgb -> "a98-rgb"
    | `Display_p3 -> "display-p3"
    | `From -> "from"
    | `None -> "none"
    | `Prophoto_rgb -> "prophoto-rgb"
    | `Rec2020 -> "rec2020"
    | `Rec2100_hlg -> "rec2100-hlg"
    | `Rec2100_linear -> "rec2100-linear"
    | `Rec2100_pq -> "rec2100-pq"
    | `Srgb -> "srgb"
    | `Srgb_linear -> "srgb-linear"
    | `Xyz -> "xyz"
    | `Xyz_d50 -> "xyz-d50"
    | `Xyz_d65 -> "xyz-d65"
  ;;
end

module Color_layers_fn = struct
  type t =
    [ Blend_mode_type.t
    | Css_data_type.Color.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Blend_mode_type.t as t -> Blend_mode_type.to_string_css t
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
  ;;
end

module Color_mix_fn = struct
  type t =
    [ Css_data_type.Color.t
    | Color_interpolation_method_type.t
    | Css_data_type.Percentage.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Color_interpolation_method_type.t as t ->
      Color_interpolation_method_type.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
  ;;
end

module Conic_gradient_fn = struct
  type t = [ | Conic_gradient_syntax_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Conic_gradient_syntax_type.t as t -> Conic_gradient_syntax_type.to_string_css t
  ;;
end

module Content_fn = struct
  type t =
    [ `After
    | `Before
    | `First_letter
    | `Marker
    | `Text
    ]

  let to_string_css = function
    | `After -> "after"
    | `Before -> "before"
    | `First_letter -> "first-letter"
    | `Marker -> "marker"
    | `Text -> "text"
  ;;
end

module Contrast_color_fn = struct
  type t =
    [ Css_data_type.Color.t
    | Css_data_type.Number.t
    | `Aa
    | `Aaa
    | `Large
    | `Tbd_bg
    | `Tbd_fg
    | `Wcag2
    ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | `Aa -> "aa"
    | `Aaa -> "aaa"
    | `Large -> "large"
    | `Tbd_bg -> "tbd-bg"
    | `Tbd_fg -> "tbd-fg"
    | `Wcag2 -> "wcag2"
  ;;
end

module Control_value_fn = struct
  type t =
    [ `Number_
    | `String_
    ]

  let to_string_css = function
    | `Number_ -> "number"
    | `String_ -> "string"
  ;;
end

module Cos_fn = struct
  type t = [ | Calc_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
  ;;
end

module Cubic_bezier_fn = struct
  type t = [ | Css_data_type.Number.t ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
  ;;
end

module Device_cmyk_fn = struct
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `None
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Alpha_value_type.t as t -> Alpha_value_type.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `None -> "none"
  ;;
end

module Dynamic_range_limit_mix_fn = struct
  type t =
    [ Css_data_type.Percentage.t
    | `Constrained
    | `No_limit
    | `Standard
    ]

  let to_string_css = function
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `Constrained -> "constrained"
    | `No_limit -> "no-limit"
    | `Standard -> "standard"
  ;;
end

module Dynamic_range_limit = struct
  type t =
    [ Css_data_type.Percentage.t
    | `Constrained
    | `No_limit
    | `Standard
    ]

  let to_string_css = function
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `Constrained -> "constrained"
    | `No_limit -> "no-limit"
    | `Standard -> "standard"
  ;;
end

module Env_fn = struct
  type t =
    [ Css_data_type.Ident.Custom.t
    | Css_data_type.Integer.t
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Custom.t as t -> Css_data_type.Ident.Custom.to_string_css t
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
  ;;
end

module Exp_fn = struct
  type t = [ | Calc_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
  ;;
end

module Text_overflow = struct
  module Fade_fn = struct
    type t = [ | Length_percentage_type.t ]

    let to_string_css = function
      | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    ;;
  end

  type t =
    [ Css_data_type.Css_string.t
    | `Clip
    | `Ellipsis
    | `Fade
    ]

  let to_string_css = function
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
    | `Clip -> "clip"
    | `Ellipsis -> "ellipsis"
    | `Fade -> "fade"
  ;;
end

module Height = struct
  module Fit_content_fn = struct
    type t = [ | Length_percentage_type.t ]

    let to_string_css = function
      | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    ;;
  end

  type t =
    [ Length_percentage_type.t
    | `Auto
    | `Contain
    | `Fit_content
    | `Max_content
    | `Min_content
    | `Stretch
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
    | `Contain -> "contain"
    | `Fit_content -> "fit-content"
    | `Max_content -> "max-content"
    | `Min_content -> "min-content"
    | `Stretch -> "stretch"
  ;;
end

module Max_height = struct
  module Fit_content_fn = struct
    type t = [ | Length_percentage_type.t ]

    let to_string_css = function
      | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    ;;
  end

  type t =
    [ Length_percentage_type.t
    | `Contain
    | `Fit_content
    | `Max_content
    | `Min_content
    | `None
    | `Stretch
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Contain -> "contain"
    | `Fit_content -> "fit-content"
    | `Max_content -> "max-content"
    | `Min_content -> "min-content"
    | `None -> "none"
    | `Stretch -> "stretch"
  ;;
end

module Max_width = struct
  module Fit_content_fn = struct
    type t = [ | Length_percentage_type.t ]

    let to_string_css = function
      | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    ;;
  end

  type t =
    [ Length_percentage_type.t
    | `Contain
    | `Fit_content
    | `Max_content
    | `Min_content
    | `None
    | `Stretch
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Contain -> "contain"
    | `Fit_content -> "fit-content"
    | `Max_content -> "max-content"
    | `Min_content -> "min-content"
    | `None -> "none"
    | `Stretch -> "stretch"
  ;;
end

module Max_block_size = struct
  type t = [ | Max_width.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Max_width.t as t -> Max_width.to_string_css t
  ;;
end

module Max_inline_size = struct
  type t = [ | Max_width.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Max_width.t as t -> Max_width.to_string_css t
  ;;
end

module Min_height = struct
  module Fit_content_fn = struct
    type t = [ | Length_percentage_type.t ]

    let to_string_css = function
      | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    ;;
  end

  type t =
    [ Length_percentage_type.t
    | `Auto
    | `Contain
    | `Fit_content
    | `Max_content
    | `Min_content
    | `Stretch
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
    | `Contain -> "contain"
    | `Fit_content -> "fit-content"
    | `Max_content -> "max-content"
    | `Min_content -> "min-content"
    | `Stretch -> "stretch"
  ;;
end

module Min_width = struct
  module Fit_content_fn = struct
    type t = [ | Length_percentage_type.t ]

    let to_string_css = function
      | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    ;;
  end

  type t =
    [ Length_percentage_type.t
    | `Auto
    | `Contain
    | `Fit_content
    | `Max_content
    | `Min_content
    | `Stretch
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
    | `Contain -> "contain"
    | `Fit_content -> "fit-content"
    | `Max_content -> "max-content"
    | `Min_content -> "min-content"
    | `Stretch -> "stretch"
  ;;
end

module Min_block_size = struct
  type t = [ | Min_width.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Min_width.t as t -> Min_width.to_string_css t
  ;;
end

module Min_inline_size = struct
  type t = [ | Min_width.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Min_width.t as t -> Min_width.to_string_css t
  ;;
end

module Width = struct
  module Fit_content_fn = struct
    type t = [ | Length_percentage_type.t ]

    let to_string_css = function
      | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    ;;
  end

  type t =
    [ Length_percentage_type.t
    | `Auto
    | `Contain
    | `Fit_content
    | `Max_content
    | `Min_content
    | `Stretch
    ]

  let to_string_css = function
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | `Auto -> "auto"
    | `Contain -> "contain"
    | `Fit_content -> "fit-content"
    | `Max_content -> "max-content"
    | `Min_content -> "min-content"
    | `Stretch -> "stretch"
  ;;
end

module Block_size = struct
  type t = [ | Width.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Width.t as t -> Width.to_string_css t
  ;;
end

module Flex_basis = struct
  type t =
    [ `Content
    | Width.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | `Content -> "content"
    | #Width.t as t -> Width.to_string_css t
  ;;
end

module Flex = struct
  type t =
    [ `None
    | Flex_basis.t
    | Flex_grow.t
    | Flex_shrink.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | `None -> "none"
    | #Flex_basis.t as t -> Flex_basis.to_string_css t
    | #Flex_grow.t as t -> Flex_grow.to_string_css t
    | #Flex_shrink.t as t -> Flex_shrink.to_string_css t
  ;;
end

module Inline_size = struct
  type t = [ | Width.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Width.t as t -> Width.to_string_css t
  ;;
end

module Hdr_color_fn = struct
  type t =
    [ Css_data_type.Color.t
    | Css_data_type.Number.t
    ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
  ;;
end

module Hsl_fn = struct
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Color.Hue.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `From
    | `None
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Alpha_value_type.t as t -> Alpha_value_type.to_string_css t
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Css_data_type.Color.Hue.t as t -> Css_data_type.Color.Hue.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `From -> "from"
    | `None -> "none"
  ;;
end

module Hsla_fn = struct
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Color.Hue.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `From
    | `None
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Alpha_value_type.t as t -> Alpha_value_type.to_string_css t
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Css_data_type.Color.Hue.t as t -> Css_data_type.Color.Hue.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `From -> "from"
    | `None -> "none"
  ;;
end

module Hwb_fn = struct
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Color.Hue.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `From
    | `None
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Alpha_value_type.t as t -> Alpha_value_type.to_string_css t
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Css_data_type.Color.Hue.t as t -> Css_data_type.Color.Hue.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `From -> "from"
    | `None -> "none"
  ;;
end

module Hypot_fn = struct
  type t = [ | Calc_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
  ;;
end

module Ictcp_fn = struct
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `From
    | `None
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Alpha_value_type.t as t -> Alpha_value_type.to_string_css t
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `From -> "from"
    | `None -> "none"
  ;;
end

module Ident_fn = struct
  type t =
    [ Css_data_type.Ident.t
    | Css_data_type.Integer.t
    | Css_data_type.Css_string.t
    ]

  let to_string_css = function
    | #Css_data_type.Ident.t as t -> Css_data_type.Ident.to_string_css t
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
  ;;
end

module If_fn = struct
  type t = [ `Else ]

  let to_string_css = function
    | `Else -> "else"
  ;;
end

module Inherit_fn = struct
  type t = [ | Css_data_type.Custom_property_name.t ]

  let to_string_css = function
    | #Css_data_type.Custom_property_name.t as t ->
      Css_data_type.Custom_property_name.to_string_css t
  ;;
end

module Jzazbz_fn = struct
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `From
    | `None
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Alpha_value_type.t as t -> Alpha_value_type.to_string_css t
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `From -> "from"
    | `None -> "none"
  ;;
end

module Jzczhz_fn = struct
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Color.Hue.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `From
    | `None
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Alpha_value_type.t as t -> Alpha_value_type.to_string_css t
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Css_data_type.Color.Hue.t as t -> Css_data_type.Color.Hue.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `From -> "from"
    | `None -> "none"
  ;;
end

module Lab_fn = struct
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `From
    | `None
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Alpha_value_type.t as t -> Alpha_value_type.to_string_css t
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `From -> "from"
    | `None -> "none"
  ;;
end

module Lch_fn = struct
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Color.Hue.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `From
    | `None
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Alpha_value_type.t as t -> Alpha_value_type.to_string_css t
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Css_data_type.Color.Hue.t as t -> Css_data_type.Color.Hue.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `From -> "from"
    | `None -> "none"
  ;;
end

module Leader_fn = struct
  type t =
    [ Css_data_type.Css_string.t
    | `Dotted
    | `Solid
    | `Space
    ]

  let to_string_css = function
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
    | `Dotted -> "dotted"
    | `Solid -> "solid"
    | `Space -> "space"
  ;;
end

module Light_dark_fn = struct
  type t = [ | Css_data_type.Color.t ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
  ;;
end

module Linear_fn = struct
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
  ;;
end

module Linear_gradient_fn = struct
  type t = [ | Linear_gradient_syntax_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Linear_gradient_syntax_type.t as t -> Linear_gradient_syntax_type.to_string_css t
  ;;
end

module Log_fn = struct
  type t = [ | Calc_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
  ;;
end

module Matrix3d_fn = struct
  type t = [ | Css_data_type.Number.t ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
  ;;
end

module Max_fn = struct
  type t = [ | Calc_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
  ;;
end

module Media_fn = struct
  type t =
    [ Css_data_type.Ident.t
    | Css_data_type.Number.t
    | Ratio_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Ident.t as t -> Css_data_type.Ident.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Ratio_type.t as t -> Ratio_type.to_string_css t
  ;;
end

module Min_fn = struct
  type t = [ | Calc_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
  ;;
end

module Grid_template_columns = struct
  module Fit_content_fn = struct
    type t = [ | Length_percentage_type.t ]

    let to_string_css = function
      | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    ;;
  end

  module Minmax_fn = struct
    type t =
      [ `Max
      | `Min
      ]

    let to_string_css = function
      | `Max -> "max"
      | `Min -> "min"
    ;;
  end

  type t =
    [ Auto_track_list_type.t
    | Line_name_list_type.t
    | Track_list_type.t
    | `None
    | `Subgrid
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Auto_track_list_type.t as t -> Auto_track_list_type.to_string_css t
    | #Line_name_list_type.t as t -> Line_name_list_type.to_string_css t
    | #Track_list_type.t as t -> Track_list_type.to_string_css t
    | `None -> "none"
    | `Subgrid -> "subgrid"
  ;;
end

module Grid_template_rows = struct
  module Fit_content_fn = struct
    type t = [ | Length_percentage_type.t ]

    let to_string_css = function
      | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    ;;
  end

  module Minmax_fn = struct
    type t =
      [ `Max
      | `Min
      ]

    let to_string_css = function
      | `Max -> "max"
      | `Min -> "min"
    ;;
  end

  type t =
    [ Auto_track_list_type.t
    | Line_name_list_type.t
    | Track_list_type.t
    | `None
    | `Subgrid
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Auto_track_list_type.t as t -> Auto_track_list_type.to_string_css t
    | #Line_name_list_type.t as t -> Line_name_list_type.to_string_css t
    | #Track_list_type.t as t -> Track_list_type.to_string_css t
    | `None -> "none"
    | `Subgrid -> "subgrid"
  ;;
end

module Grid_template = struct
  type t =
    [ Line_names_type.t
    | Css_data_type.Css_string.t
    | Track_size_type.t
    | `None
    | Grid_template_columns.t
    | Grid_template_rows.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Line_names_type.t as t -> Line_names_type.to_string_css t
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
    | #Track_size_type.t as t -> Track_size_type.to_string_css t
    | `None -> "none"
    | #Grid_template_columns.t as t -> Grid_template_columns.to_string_css t
    | #Grid_template_rows.t as t -> Grid_template_rows.to_string_css t
  ;;
end

module Grid = struct
  type t =
    [ `Auto_flow
    | `Dense
    | Grid_auto_columns.t
    | Grid_auto_rows.t
    | Grid_template.t
    | Grid_template_columns.t
    | Grid_template_rows.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | `Auto_flow -> "auto-flow"
    | `Dense -> "dense"
    | #Grid_auto_columns.t as t -> Grid_auto_columns.to_string_css t
    | #Grid_auto_rows.t as t -> Grid_auto_rows.to_string_css t
    | #Grid_template.t as t -> Grid_template.to_string_css t
    | #Grid_template_columns.t as t -> Grid_template_columns.to_string_css t
    | #Grid_template_rows.t as t -> Grid_template_rows.to_string_css t
  ;;
end

module Mod_fn = struct
  type t = [ | Calc_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
  ;;
end

module Oklab_fn = struct
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `From
    | `None
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Alpha_value_type.t as t -> Alpha_value_type.to_string_css t
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `From -> "from"
    | `None -> "none"
  ;;
end

module Oklch_fn = struct
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Color.Hue.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `From
    | `None
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Alpha_value_type.t as t -> Alpha_value_type.to_string_css t
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Css_data_type.Color.Hue.t as t -> Css_data_type.Color.Hue.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `From -> "from"
    | `None -> "none"
  ;;
end

module Paint_fn = struct
  type t = [ | Css_data_type.Ident.t ]

  let to_string_css = function
    | #Css_data_type.Ident.t as t -> Css_data_type.Ident.to_string_css t
  ;;
end

module Palette_mix_fn = struct
  type t =
    [ Color_interpolation_method_type.t
    | Css_data_type.Percentage.t
    | `Dark
    | `Light
    | `Normal
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Color_interpolation_method_type.t as t ->
      Color_interpolation_method_type.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `Dark -> "dark"
    | `Light -> "light"
    | `Normal -> "normal"
  ;;
end

module Font_palette = struct
  type t =
    [ `Dark
    | `Light
    | `Normal
    ]

  let to_string_css = function
    | `Dark -> "dark"
    | `Light -> "light"
    | `Normal -> "normal"
  ;;
end

module Perspective_fn = struct
  type t =
    [ Css_data_type.Length.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | `None -> "none"
  ;;
end

module Pointer_fn = struct
  type t =
    [ `Block
    | `Inline
    | `Nearest
    | `Root
    | `Self
    | `X
    | `Y
    ]

  let to_string_css = function
    | `Block -> "block"
    | `Inline -> "inline"
    | `Nearest -> "nearest"
    | `Root -> "root"
    | `Self -> "self"
    | `X -> "x"
    | `Y -> "y"
  ;;
end

module Pow_fn = struct
  type t = [ | Calc_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
  ;;
end

module Progress_fn = struct
  type t = [ | Calc_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
  ;;
end

module Radial_gradient_fn = struct
  type t = [ | Radial_gradient_syntax_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Radial_gradient_syntax_type.t as t -> Radial_gradient_syntax_type.to_string_css t
  ;;
end

module Random_fn = struct
  type t =
    [ Calc_value_type.t
    | Random_value_sharing_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
    | #Random_value_sharing_type.t as t -> Random_value_sharing_type.to_string_css t
  ;;
end

module Random_item_fn = struct
  type t = [ | Random_value_sharing_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Random_value_sharing_type.t as t -> Random_value_sharing_type.to_string_css t
  ;;
end

module Ray_fn = struct
  type t =
    [ Css_data_type.Angle.t
    | Position_type.t
    | `At
    | `Closest_corner
    | `Closest_side
    | `Contain
    | `Farthest_corner
    | `Farthest_side
    | `Sides
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Angle.t as t -> Css_data_type.Angle.to_string_css t
    | #Position_type.t as t -> Position_type.to_string_css t
    | `At -> "at"
    | `Closest_corner -> "closest-corner"
    | `Closest_side -> "closest-side"
    | `Contain -> "contain"
    | `Farthest_corner -> "farthest-corner"
    | `Farthest_side -> "farthest-side"
    | `Sides -> "sides"
  ;;
end

module Clip = struct
  module Rect_fn = struct
    type t =
      [ Bottom_type.t
      | Left_type.t
      | Right_type.t
      | Top_type.t
      ]

    let to_string_css
      =
      function[@ocaml.warning "-11"]
              (* Silence unused arm error. The type is illustrative and the functionality
                 is the same whether or not the arm exists *)
      | #Bottom_type.t as t -> Bottom_type.to_string_css t
      | #Left_type.t as t -> Left_type.to_string_css t
      | #Right_type.t as t -> Right_type.to_string_css t
      | #Top_type.t as t -> Top_type.to_string_css t
    ;;
  end

  type t = [ `Auto ]

  let to_string_css = function
    | `Auto -> "auto"
  ;;
end

module Rem_fn = struct
  type t = [ | Calc_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
  ;;
end

module Repeating_conic_gradient_fn = struct
  type t = [ | Conic_gradient_syntax_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Conic_gradient_syntax_type.t as t -> Conic_gradient_syntax_type.to_string_css t
  ;;
end

module Repeating_linear_gradient_fn = struct
  type t = [ | Linear_gradient_syntax_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Linear_gradient_syntax_type.t as t -> Linear_gradient_syntax_type.to_string_css t
  ;;
end

module Repeating_radial_gradient_fn = struct
  type t = [ | Radial_gradient_syntax_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Radial_gradient_syntax_type.t as t -> Radial_gradient_syntax_type.to_string_css t
  ;;
end

module Rgb_fn = struct
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `From
    | `None
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Alpha_value_type.t as t -> Alpha_value_type.to_string_css t
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `From -> "from"
    | `None -> "none"
  ;;
end

module Rgba_fn = struct
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `From
    | `None
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Alpha_value_type.t as t -> Alpha_value_type.to_string_css t
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | `From -> "from"
    | `None -> "none"
  ;;
end

module Color_base_type = struct
  type t =
    [ Css_data_type.Color.Hex.t
    | Css_data_type.Color.Named.t
    | `Transparent
    ]

  let to_string_css = function
    | #Css_data_type.Color.Hex.t as t -> Css_data_type.Color.Hex.to_string_css t
    | #Css_data_type.Color.Named.t as t -> Css_data_type.Color.Named.to_string_css t
    | `Transparent -> "transparent"
  ;;
end

module Rotate3d_fn = struct
  type t =
    [ Css_data_type.Angle.t
    | Css_data_type.Number.t
    | Zero_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Angle.t as t -> Css_data_type.Angle.to_string_css t
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Zero_type.t as t -> Zero_type.to_string_css t
  ;;
end

module RotateX_fn = struct
  type t =
    [ Css_data_type.Angle.t
    | Zero_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Angle.t as t -> Css_data_type.Angle.to_string_css t
    | #Zero_type.t as t -> Zero_type.to_string_css t
  ;;
end

module RotateY_fn = struct
  type t =
    [ Css_data_type.Angle.t
    | Zero_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Angle.t as t -> Css_data_type.Angle.to_string_css t
    | #Zero_type.t as t -> Zero_type.to_string_css t
  ;;
end

module RotateZ_fn = struct
  type t =
    [ Css_data_type.Angle.t
    | Zero_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Angle.t as t -> Css_data_type.Angle.to_string_css t
    | #Zero_type.t as t -> Zero_type.to_string_css t
  ;;
end

module Round_fn = struct
  type t =
    [ Calc_value_type.t
    | `Down
    | `Nearest
    | `To_zero
    | `Up
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
    | `Down -> "down"
    | `Nearest -> "nearest"
    | `To_zero -> "to-zero"
    | `Up -> "up"
  ;;
end

module Running_fn = struct
  type t = [ | Css_data_type.Ident.Custom.t ]

  let to_string_css = function
    | #Css_data_type.Ident.Custom.t as t -> Css_data_type.Ident.Custom.to_string_css t
  ;;
end

module Position = struct
  type t =
    [ `Absolute
    | `Fixed
    | `Relative
    | `Static
    | `Sticky
    ]

  let to_string_css = function
    | `Absolute -> "absolute"
    | `Fixed -> "fixed"
    | `Relative -> "relative"
    | `Static -> "static"
    | `Sticky -> "sticky"
  ;;
end

module Scale_fn = struct
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
  ;;
end

module Scale3d_fn = struct
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
  ;;
end

module ScaleX_fn = struct
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
  ;;
end

module ScaleY_fn = struct
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
  ;;
end

module ScaleZ_fn = struct
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
  ;;
end

module Scroll_fn = struct
  type t =
    [ Axis_type.t
    | `Nearest
    | `Root
    | `Self
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Axis_type.t as t -> Axis_type.to_string_css t
    | `Nearest -> "nearest"
    | `Root -> "root"
    | `Self -> "self"
  ;;
end

module Sign_fn = struct
  type t = [ | Calc_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
  ;;
end

module Sin_fn = struct
  type t = [ | Calc_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
  ;;
end

module Float = struct
  module Snap_block_fn = struct
    type t =
      [ Css_data_type.Length.t
      | `End
      | `Near
      | `Start
      ]

    let to_string_css = function
      | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
      | `End -> "end"
      | `Near -> "near"
      | `Start -> "start"
    ;;
  end

  module Snap_inline_fn = struct
    type t =
      [ Css_data_type.Length.t
      | `Left
      | `Near
      | `Right
      ]

    let to_string_css = function
      | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
      | `Left -> "left"
      | `Near -> "near"
      | `Right -> "right"
    ;;
  end

  type t =
    [ `Block_end
    | `Block_start
    | `Bottom
    | `Footnote
    | `Inline_end
    | `Inline_start
    | `Left
    | `None
    | `Right
    | `Snap_block
    | `Snap_inline
    | `Top
    ]

  let to_string_css = function
    | `Block_end -> "block-end"
    | `Block_start -> "block-start"
    | `Bottom -> "bottom"
    | `Footnote -> "footnote"
    | `Inline_end -> "inline-end"
    | `Inline_start -> "inline-start"
    | `Left -> "left"
    | `None -> "none"
    | `Right -> "right"
    | `Snap_block -> "snap-block"
    | `Snap_inline -> "snap-inline"
    | `Top -> "top"
  ;;
end

module Sqrt_fn = struct
  type t = [ | Calc_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
  ;;
end

module Src_fn = struct
  type t = [ | Css_data_type.Css_string.t ]

  let to_string_css = function
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
  ;;
end

module Steps_fn = struct
  type t =
    [ Css_data_type.Integer.t
    | `End
    | `Jump_both
    | `Jump_end
    | `Jump_none
    | `Jump_start
    | `Start
    ]

  let to_string_css = function
    | #Css_data_type.Integer.t as t -> Css_data_type.Integer.to_string_css t
    | `End -> "end"
    | `Jump_both -> "jump-both"
    | `Jump_end -> "jump-end"
    | `Jump_none -> "jump-none"
    | `Jump_start -> "jump-start"
    | `Start -> "start"
  ;;
end

module Easing_function_type = struct
  type t =
    [ `Ease
    | `Ease_in
    | `Ease_in_out
    | `Ease_out
    | `Linear
    | `Step_end
    | `Step_start
    ]

  let to_string_css = function
    | `Ease -> "ease"
    | `Ease_in -> "ease-in"
    | `Ease_in_out -> "ease-in-out"
    | `Ease_out -> "ease-out"
    | `Linear -> "linear"
    | `Step_end -> "step-end"
    | `Step_start -> "step-start"
  ;;
end

module Animation_timing_function = struct
  type t = [ | Easing_function_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Easing_function_type.t as t -> Easing_function_type.to_string_css t
  ;;
end

module Transition = struct
  type t =
    [ Easing_function_type.t
    | Single_transition_property_type.t
    | Css_data_type.Time.t
    | Transition_behavior_value_type.t
    | `None
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Easing_function_type.t as t -> Easing_function_type.to_string_css t
    | #Single_transition_property_type.t as t ->
      Single_transition_property_type.to_string_css t
    | #Css_data_type.Time.t as t -> Css_data_type.Time.to_string_css t
    | #Transition_behavior_value_type.t as t ->
      Transition_behavior_value_type.to_string_css t
    | `None -> "none"
  ;;
end

module Transition_timing_function = struct
  type t = [ | Easing_function_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Easing_function_type.t as t -> Easing_function_type.to_string_css t
  ;;
end

module String_fn = struct
  type t =
    [ Css_data_type.Ident.Custom.t
    | `First
    | `First_except
    | `Last
    | `Start
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Custom.t as t -> Css_data_type.Ident.Custom.to_string_css t
    | `First -> "first"
    | `First_except -> "first-except"
    | `Last -> "last"
    | `Start -> "start"
  ;;
end

module Stripes_fn = struct
  type t =
    [ Css_data_type.Color.t
    | Css_data_type.Flex_fr.t
    | Length_percentage_type.t
    ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Css_data_type.Flex_fr.t as t -> Css_data_type.Flex_fr.to_string_css t
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Border_block_end_color = struct
  type t = [ | Css_data_type.Color.t ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
  ;;
end

module Border_block_start_color = struct
  type t = [ | Css_data_type.Color.t ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
  ;;
end

module Border_bottom_color = struct
  type t = [ | Css_data_type.Color.t ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
  ;;
end

module Border_color = struct
  type t = [ | Css_data_type.Color.t ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
  ;;
end

module Border_inline_end_color = struct
  type t = [ | Css_data_type.Color.t ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
  ;;
end

module Border_inline_start_color = struct
  type t = [ | Css_data_type.Color.t ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
  ;;
end

module Border_left_color = struct
  type t = [ | Css_data_type.Color.t ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
  ;;
end

module Border_right_color = struct
  type t = [ | Css_data_type.Color.t ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
  ;;
end

module Border_top_color = struct
  type t = [ | Css_data_type.Color.t ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
  ;;
end

module Border_block_color = struct
  type t = [ | Border_top_color.t ]

  let to_string_css = function
    | #Border_top_color.t as t -> Border_top_color.to_string_css t
  ;;
end

module Border_inline_color = struct
  type t = [ | Border_top_color.t ]

  let to_string_css = function
    | #Border_top_color.t as t -> Border_top_color.to_string_css t
  ;;
end

module Outline_color = struct
  type t =
    [ Css_data_type.Color.t
    | `Auto
    ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | `Auto -> "auto"
  ;;
end

module Outline = struct
  type t =
    [ Outline_color.t
    | Outline_style.t
    | Outline_width.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Outline_color.t as t -> Outline_color.to_string_css t
    | #Outline_style.t as t -> Outline_style.to_string_css t
    | #Outline_width.t as t -> Outline_width.to_string_css t
  ;;
end

module Superellipse_fn = struct
  type t =
    [ Css_data_type.Number.t
    | `Negative_infinity
    | `Infinity
    ]

  let to_string_css = function
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | `Negative_infinity -> "-infinity"
    | `Infinity -> "infinity"
  ;;
end

module Corner_shape_value_type = struct
  type t =
    [ `Bevel
    | `Notch
    | `Round
    | `Scoop
    | `Square
    | `Squircle
    ]

  let to_string_css = function
    | `Bevel -> "bevel"
    | `Notch -> "notch"
    | `Round -> "round"
    | `Scoop -> "scoop"
    | `Square -> "square"
    | `Squircle -> "squircle"
  ;;
end

module Corner_block_end_shape = struct
  type t = [ | Corner_shape_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Corner_shape_value_type.t as t -> Corner_shape_value_type.to_string_css t
  ;;
end

module Corner_block_start_shape = struct
  type t = [ | Corner_shape_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Corner_shape_value_type.t as t -> Corner_shape_value_type.to_string_css t
  ;;
end

module Corner_bottom_left_shape = struct
  type t = [ | Corner_shape_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Corner_shape_value_type.t as t -> Corner_shape_value_type.to_string_css t
  ;;
end

module Corner_bottom_right_shape = struct
  type t = [ | Corner_shape_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Corner_shape_value_type.t as t -> Corner_shape_value_type.to_string_css t
  ;;
end

module Corner_bottom_shape = struct
  type t = [ | Corner_shape_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Corner_shape_value_type.t as t -> Corner_shape_value_type.to_string_css t
  ;;
end

module Corner_end_end_shape = struct
  type t = [ | Corner_shape_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Corner_shape_value_type.t as t -> Corner_shape_value_type.to_string_css t
  ;;
end

module Corner_end_start_shape = struct
  type t = [ | Corner_shape_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Corner_shape_value_type.t as t -> Corner_shape_value_type.to_string_css t
  ;;
end

module Corner_inline_end_shape = struct
  type t = [ | Corner_shape_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Corner_shape_value_type.t as t -> Corner_shape_value_type.to_string_css t
  ;;
end

module Corner_inline_start_shape = struct
  type t = [ | Corner_shape_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Corner_shape_value_type.t as t -> Corner_shape_value_type.to_string_css t
  ;;
end

module Corner_left_shape = struct
  type t = [ | Corner_shape_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Corner_shape_value_type.t as t -> Corner_shape_value_type.to_string_css t
  ;;
end

module Corner_right_shape = struct
  type t = [ | Corner_shape_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Corner_shape_value_type.t as t -> Corner_shape_value_type.to_string_css t
  ;;
end

module Corner_shape = struct
  type t = [ | Corner_shape_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Corner_shape_value_type.t as t -> Corner_shape_value_type.to_string_css t
  ;;
end

module Corner_start_end_shape = struct
  type t = [ | Corner_shape_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Corner_shape_value_type.t as t -> Corner_shape_value_type.to_string_css t
  ;;
end

module Corner_start_start_shape = struct
  type t = [ | Corner_shape_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Corner_shape_value_type.t as t -> Corner_shape_value_type.to_string_css t
  ;;
end

module Corner_top_left_shape = struct
  type t = [ | Corner_shape_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Corner_shape_value_type.t as t -> Corner_shape_value_type.to_string_css t
  ;;
end

module Corner_top_right_shape = struct
  type t = [ | Corner_shape_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Corner_shape_value_type.t as t -> Corner_shape_value_type.to_string_css t
  ;;
end

module Corner_top_shape = struct
  type t = [ | Corner_shape_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Corner_shape_value_type.t as t -> Corner_shape_value_type.to_string_css t
  ;;
end

module Tan_fn = struct
  type t = [ | Calc_value_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
  ;;
end

module Transform_mix_fn = struct
  type t = [ | Css_data_type.Percentage.t ]

  let to_string_css = function
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
  ;;
end

module Translate3d_fn = struct
  type t =
    [ Css_data_type.Length.t
    | Length_percentage_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
  ;;
end

module Transform = struct
  module Matrix_fn = struct
    type t = [ | Css_data_type.Number.t ]

    let to_string_css = function
      | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    ;;
  end

  module Rotate_fn = struct
    type t =
      [ Css_data_type.Angle.t
      | Zero_type.t
      ]

    let to_string_css
      =
      function[@ocaml.warning "-11"]
              (* Silence unused arm error. The type is illustrative and the functionality
                 is the same whether or not the arm exists *)
      | #Css_data_type.Angle.t as t -> Css_data_type.Angle.to_string_css t
      | #Zero_type.t as t -> Zero_type.to_string_css t
    ;;
  end

  module Scale_fn = struct
    type t = [ | Css_data_type.Number.t ]

    let to_string_css = function
      | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    ;;
  end

  module ScaleX_fn = struct
    type t = [ | Css_data_type.Number.t ]

    let to_string_css = function
      | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    ;;
  end

  module ScaleY_fn = struct
    type t = [ | Css_data_type.Number.t ]

    let to_string_css = function
      | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    ;;
  end

  module Skew_fn = struct
    type t =
      [ Css_data_type.Angle.t
      | Zero_type.t
      ]

    let to_string_css
      =
      function[@ocaml.warning "-11"]
              (* Silence unused arm error. The type is illustrative and the functionality
                 is the same whether or not the arm exists *)
      | #Css_data_type.Angle.t as t -> Css_data_type.Angle.to_string_css t
      | #Zero_type.t as t -> Zero_type.to_string_css t
    ;;
  end

  module SkewX_fn = struct
    type t =
      [ Css_data_type.Angle.t
      | Zero_type.t
      ]

    let to_string_css
      =
      function[@ocaml.warning "-11"]
              (* Silence unused arm error. The type is illustrative and the functionality
                 is the same whether or not the arm exists *)
      | #Css_data_type.Angle.t as t -> Css_data_type.Angle.to_string_css t
      | #Zero_type.t as t -> Zero_type.to_string_css t
    ;;
  end

  module SkewY_fn = struct
    type t =
      [ Css_data_type.Angle.t
      | Zero_type.t
      ]

    let to_string_css
      =
      function[@ocaml.warning "-11"]
              (* Silence unused arm error. The type is illustrative and the functionality
                 is the same whether or not the arm exists *)
      | #Css_data_type.Angle.t as t -> Css_data_type.Angle.to_string_css t
      | #Zero_type.t as t -> Zero_type.to_string_css t
    ;;
  end

  module Translate_fn = struct
    type t = [ | Length_percentage_type.t ]

    let to_string_css = function
      | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    ;;
  end

  module TranslateX_fn = struct
    type t = [ | Length_percentage_type.t ]

    let to_string_css = function
      | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    ;;
  end

  module TranslateY_fn = struct
    type t = [ | Length_percentage_type.t ]

    let to_string_css = function
      | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    ;;
  end

  type t = [ `None ]

  let to_string_css = function
    | `None -> "none"
  ;;
end

module TranslateZ_fn = struct
  type t = [ | Css_data_type.Length.t ]

  let to_string_css = function
    | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
  ;;
end

module Url_fn = struct
  type t = [ | Css_data_type.Css_string.t ]

  let to_string_css = function
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
  ;;
end

module Paint_type = struct
  type t =
    [ Css_data_type.Color.t
    | `Context_fill
    | `Context_stroke
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | `Context_fill -> "context-fill"
    | `Context_stroke -> "context-stroke"
    | `None -> "none"
  ;;
end

module Backdrop_filter = struct
  type t = [ `None ]

  let to_string_css = function
    | `None -> "none"
  ;;
end

module Cursor = struct
  type t =
    [ `Alias
    | `All_scroll
    | `Auto
    | `Cell
    | `Col_resize
    | `Context_menu
    | `Copy
    | `Crosshair
    | `Default
    | `E_resize
    | `Ew_resize
    | `Grab
    | `Grabbing
    | `Help
    | `Move
    | `N_resize
    | `Ne_resize
    | `Nesw_resize
    | `No_drop
    | `None
    | `Not_allowed
    | `Ns_resize
    | `Nw_resize
    | `Nwse_resize
    | `Pointer
    | `Progress
    | `Row_resize
    | `S_resize
    | `Se_resize
    | `Sw_resize
    | `Text
    | `Vertical_text
    | `W_resize
    | `Wait
    | `Zoom_in
    | `Zoom_out
    | X.t
    | Y.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | `Alias -> "alias"
    | `All_scroll -> "all-scroll"
    | `Auto -> "auto"
    | `Cell -> "cell"
    | `Col_resize -> "col-resize"
    | `Context_menu -> "context-menu"
    | `Copy -> "copy"
    | `Crosshair -> "crosshair"
    | `Default -> "default"
    | `E_resize -> "e-resize"
    | `Ew_resize -> "ew-resize"
    | `Grab -> "grab"
    | `Grabbing -> "grabbing"
    | `Help -> "help"
    | `Move -> "move"
    | `N_resize -> "n-resize"
    | `Ne_resize -> "ne-resize"
    | `Nesw_resize -> "nesw-resize"
    | `No_drop -> "no-drop"
    | `None -> "none"
    | `Not_allowed -> "not-allowed"
    | `Ns_resize -> "ns-resize"
    | `Nw_resize -> "nw-resize"
    | `Nwse_resize -> "nwse-resize"
    | `Pointer -> "pointer"
    | `Progress -> "progress"
    | `Row_resize -> "row-resize"
    | `S_resize -> "s-resize"
    | `Se_resize -> "se-resize"
    | `Sw_resize -> "sw-resize"
    | `Text -> "text"
    | `Vertical_text -> "vertical-text"
    | `W_resize -> "w-resize"
    | `Wait -> "wait"
    | `Zoom_in -> "zoom-in"
    | `Zoom_out -> "zoom-out"
    | #X.t as t -> X.to_string_css t
    | #Y.t as t -> Y.to_string_css t
  ;;
end

module Fill = struct
  type t = [ | Paint_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Paint_type.t as t -> Paint_type.to_string_css t
  ;;
end

module Fill_image = struct
  type t = [ | Paint_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Paint_type.t as t -> Paint_type.to_string_css t
  ;;
end

module Filter = struct
  module Blur_fn = struct
    type t = [ | Css_data_type.Length.t ]

    let to_string_css = function
      | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    ;;
  end

  module Brightness_fn = struct
    type t =
      [ Css_data_type.Number.t
      | Css_data_type.Percentage.t
      ]

    let to_string_css = function
      | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
      | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    ;;
  end

  module Contrast_fn = struct
    type t =
      [ Css_data_type.Number.t
      | Css_data_type.Percentage.t
      ]

    let to_string_css = function
      | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
      | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    ;;
  end

  module Drop_shadow_fn = struct
    type t =
      [ Css_data_type.Color.t
      | Css_data_type.Length.t
      ]

    let to_string_css = function
      | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
      | #Css_data_type.Length.t as t -> Css_data_type.Length.to_string_css t
    ;;
  end

  module Grayscale_fn = struct
    type t =
      [ Css_data_type.Number.t
      | Css_data_type.Percentage.t
      ]

    let to_string_css = function
      | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
      | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    ;;
  end

  module Hue_rotate_fn = struct
    type t =
      [ Css_data_type.Angle.t
      | Zero_type.t
      ]

    let to_string_css
      =
      function[@ocaml.warning "-11"]
              (* Silence unused arm error. The type is illustrative and the functionality
                 is the same whether or not the arm exists *)
      | #Css_data_type.Angle.t as t -> Css_data_type.Angle.to_string_css t
      | #Zero_type.t as t -> Zero_type.to_string_css t
    ;;
  end

  module Invert_fn = struct
    type t =
      [ Css_data_type.Number.t
      | Css_data_type.Percentage.t
      ]

    let to_string_css = function
      | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
      | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    ;;
  end

  module Opacity_fn = struct
    type t =
      [ Css_data_type.Number.t
      | Css_data_type.Percentage.t
      ]

    let to_string_css = function
      | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
      | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    ;;
  end

  module Saturate_fn = struct
    type t =
      [ Css_data_type.Number.t
      | Css_data_type.Percentage.t
      ]

    let to_string_css = function
      | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
      | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    ;;
  end

  module Sepia_fn = struct
    type t =
      [ Css_data_type.Number.t
      | Css_data_type.Percentage.t
      ]

    let to_string_css = function
      | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
      | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    ;;
  end

  type t = [ `None ]

  let to_string_css = function
    | `None -> "none"
  ;;
end

module Marker = struct
  type t = [ `None ]

  let to_string_css = function
    | `None -> "none"
  ;;
end

module Marker_end = struct
  type t = [ `None ]

  let to_string_css = function
    | `None -> "none"
  ;;
end

module Marker_mid = struct
  type t = [ `None ]

  let to_string_css = function
    | `None -> "none"
  ;;
end

module Marker_start = struct
  type t = [ `None ]

  let to_string_css = function
    | `None -> "none"
  ;;
end

module Stroke = struct
  type t = [ | Paint_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Paint_type.t as t -> Paint_type.to_string_css t
  ;;
end

module Stroke_image = struct
  type t = [ | Paint_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Paint_type.t as t -> Paint_type.to_string_css t
  ;;
end

module Image_fn = struct
  type t =
    [ Css_data_type.Color.t
    | Css_data_type.Css_string.t
    | `Ltr
    | `Rtl
    ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
    | `Ltr -> "ltr"
    | `Rtl -> "rtl"
  ;;
end

module Bg_image_type = struct
  type t = [ `None ]

  let to_string_css = function
    | `None -> "none"
  ;;
end

module Bg_layer_type = struct
  type t =
    [ Attachment_type.t
    | Bg_image_type.t
    | Bg_position_type.t
    | Bg_size_type.t
    | Repeat_style_type.t
    | Visual_box_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Attachment_type.t as t -> Attachment_type.to_string_css t
    | #Bg_image_type.t as t -> Bg_image_type.to_string_css t
    | #Bg_position_type.t as t -> Bg_position_type.to_string_css t
    | #Bg_size_type.t as t -> Bg_size_type.to_string_css t
    | #Repeat_style_type.t as t -> Repeat_style_type.to_string_css t
    | #Visual_box_type.t as t -> Visual_box_type.to_string_css t
  ;;
end

module Cf_image_type = struct
  type t =
    [ Css_data_type.Color.t
    | Css_data_type.Percentage.t
    ]

  let to_string_css = function
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
  ;;
end

module Image_set_option_type = struct
  type t =
    [ Css_data_type.Resolution.t
    | Css_data_type.Css_string.t
    ]

  let to_string_css = function
    | #Css_data_type.Resolution.t as t -> Css_data_type.Resolution.to_string_css t
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
  ;;
end

module Mask_reference_type = struct
  type t = [ `None ]

  let to_string_css = function
    | `None -> "none"
  ;;
end

module Background = struct
  type t =
    [ Attachment_type.t
    | Bg_image_type.t
    | Bg_layer_type.t
    | Bg_position_type.t
    | Bg_size_type.t
    | Repeat_style_type.t
    | Visual_box_type.t
    | Background_color.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Attachment_type.t as t -> Attachment_type.to_string_css t
    | #Bg_image_type.t as t -> Bg_image_type.to_string_css t
    | #Bg_layer_type.t as t -> Bg_layer_type.to_string_css t
    | #Bg_position_type.t as t -> Bg_position_type.to_string_css t
    | #Bg_size_type.t as t -> Bg_size_type.to_string_css t
    | #Repeat_style_type.t as t -> Repeat_style_type.to_string_css t
    | #Visual_box_type.t as t -> Visual_box_type.to_string_css t
    | #Background_color.t as t -> Background_color.to_string_css t
  ;;
end

module Background_image = struct
  type t = [ | Bg_image_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Bg_image_type.t as t -> Bg_image_type.to_string_css t
  ;;
end

module Background_tbd = struct
  type t = [ | Bg_layer_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Bg_layer_type.t as t -> Bg_layer_type.to_string_css t
  ;;
end

module Border_image_source = struct
  type t = [ `None ]

  let to_string_css = function
    | `None -> "none"
  ;;
end

module Border_image = struct
  type t =
    [ Border_image_outset.t
    | Border_image_repeat.t
    | Border_image_slice.t
    | Border_image_source.t
    | Border_image_width.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Border_image_outset.t as t -> Border_image_outset.to_string_css t
    | #Border_image_repeat.t as t -> Border_image_repeat.to_string_css t
    | #Border_image_slice.t as t -> Border_image_slice.to_string_css t
    | #Border_image_source.t as t -> Border_image_source.to_string_css t
    | #Border_image_width.t as t -> Border_image_width.to_string_css t
  ;;
end

module List_style_image = struct
  type t = [ `None ]

  let to_string_css = function
    | `None -> "none"
  ;;
end

module Mask = struct
  type t =
    [ Bg_size_type.t
    | Compositing_operator_type.t
    | Geometry_box_type.t
    | Mask_reference_type.t
    | Masking_mode_type.t
    | Position_type.t
    | Repeat_style_type.t
    | `No_clip
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Bg_size_type.t as t -> Bg_size_type.to_string_css t
    | #Compositing_operator_type.t as t -> Compositing_operator_type.to_string_css t
    | #Geometry_box_type.t as t -> Geometry_box_type.to_string_css t
    | #Mask_reference_type.t as t -> Mask_reference_type.to_string_css t
    | #Masking_mode_type.t as t -> Masking_mode_type.to_string_css t
    | #Position_type.t as t -> Position_type.to_string_css t
    | #Repeat_style_type.t as t -> Repeat_style_type.to_string_css t
    | `No_clip -> "no-clip"
  ;;
end

module Mask_border_source = struct
  type t = [ `None ]

  let to_string_css = function
    | `None -> "none"
  ;;
end

module Mask_border = struct
  type t =
    [ Mask_border_mode.t
    | Mask_border_outset.t
    | Mask_border_repeat.t
    | Mask_border_slice.t
    | Mask_border_source.t
    | Mask_border_width.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Mask_border_mode.t as t -> Mask_border_mode.to_string_css t
    | #Mask_border_outset.t as t -> Mask_border_outset.to_string_css t
    | #Mask_border_repeat.t as t -> Mask_border_repeat.to_string_css t
    | #Mask_border_slice.t as t -> Mask_border_slice.to_string_css t
    | #Mask_border_source.t as t -> Mask_border_source.to_string_css t
    | #Mask_border_width.t as t -> Mask_border_width.to_string_css t
  ;;
end

module Mask_image = struct
  type t = [ | Mask_reference_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Mask_reference_type.t as t -> Mask_reference_type.to_string_css t
  ;;
end

module Filter_fn = struct
  type t = [ | Css_data_type.Css_string.t ]

  let to_string_css = function
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
  ;;
end

module Symbols_fn = struct
  type t =
    [ Css_data_type.Css_string.t
    | `Alphabetic
    | `Cyclic
    | `Fixed
    | `Numeric
    | `Symbolic
    ]

  let to_string_css = function
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
    | `Alphabetic -> "alphabetic"
    | `Cyclic -> "cyclic"
    | `Fixed -> "fixed"
    | `Numeric -> "numeric"
    | `Symbolic -> "symbolic"
  ;;
end

module List_style_type = struct
  type t =
    [ Css_data_type.Css_string.t
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
    | `None -> "none"
  ;;
end

module List_style = struct
  type t =
    [ List_style_image.t
    | List_style_position.t
    | List_style_type.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #List_style_image.t as t -> List_style_image.to_string_css t
    | #List_style_position.t as t -> List_style_position.to_string_css t
    | #List_style_type.t as t -> List_style_type.to_string_css t
  ;;
end

module Counters_fn = struct
  type t = [ | Css_data_type.Css_string.t ]

  let to_string_css = function
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
  ;;
end

module Content_list_type = struct
  type t = [ | Css_data_type.Css_string.t ]

  let to_string_css = function
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
  ;;
end

module Bookmark_label = struct
  type t = [ | Content_list_type.t ]

  let to_string_css = function
    | #Content_list_type.t as t -> Content_list_type.to_string_css t
  ;;
end

module Content = struct
  type t =
    [ Content_list_type.t
    | Css_data_type.Css_string.t
    | `None
    | `Normal
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Content_list_type.t as t -> Content_list_type.to_string_css t
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
    | `None -> "none"
    | `Normal -> "normal"
  ;;
end

module Copy_into = struct
  type t =
    [ Css_data_type.Ident.Custom.t
    | `Content
    | `Element
    | `None
    | `Text
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Custom.t as t -> Css_data_type.Ident.Custom.to_string_css t
    | `Content -> "content"
    | `Element -> "element"
    | `None -> "none"
    | `Text -> "text"
  ;;
end

module Target_counter_fn = struct
  type t =
    [ Css_data_type.Ident.Custom.t
    | Css_data_type.Css_string.t
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Custom.t as t -> Css_data_type.Ident.Custom.to_string_css t
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
  ;;
end

module Target_counters_fn = struct
  type t =
    [ Css_data_type.Ident.Custom.t
    | Css_data_type.Css_string.t
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Custom.t as t -> Css_data_type.Ident.Custom.to_string_css t
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
  ;;
end

module Target_text_fn = struct
  type t =
    [ Css_data_type.Css_string.t
    | `After
    | `Before
    | `Content
    | `First_letter
    ]

  let to_string_css = function
    | #Css_data_type.Css_string.t as t -> Css_data_type.Css_string.to_string_css t
    | `After -> "after"
    | `Before -> "before"
    | `Content -> "content"
    | `First_letter -> "first-letter"
  ;;
end

module Var_fn = struct
  type t = [ | Css_data_type.Custom_property_name.t ]

  let to_string_css = function
    | #Css_data_type.Custom_property_name.t as t ->
      Css_data_type.Custom_property_name.to_string_css t
  ;;
end

module View_fn = struct
  type t =
    [ Axis_type.t
    | View_timeline_inset.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Axis_type.t as t -> Axis_type.to_string_css t
    | #View_timeline_inset.t as t -> View_timeline_inset.to_string_css t
  ;;
end

module Single_animation_timeline_type = struct
  type t =
    [ Css_data_type.Ident.Dashed.t
    | `Auto
    | `None
    ]

  let to_string_css = function
    | #Css_data_type.Ident.Dashed.t as t -> Css_data_type.Ident.Dashed.to_string_css t
    | `Auto -> "auto"
    | `None -> "none"
  ;;
end

module Animation = struct
  type t =
    [ Easing_function_type.t
    | Keyframes_name_type.t
    | Single_animation_direction_type.t
    | Single_animation_fill_mode_type.t
    | Single_animation_iteration_count_type.t
    | Single_animation_play_state_type.t
    | Single_animation_timeline_type.t
    | `None
    | Animation_delay.t
    | Animation_duration.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Easing_function_type.t as t -> Easing_function_type.to_string_css t
    | #Keyframes_name_type.t as t -> Keyframes_name_type.to_string_css t
    | #Single_animation_direction_type.t as t ->
      Single_animation_direction_type.to_string_css t
    | #Single_animation_fill_mode_type.t as t ->
      Single_animation_fill_mode_type.to_string_css t
    | #Single_animation_iteration_count_type.t as t ->
      Single_animation_iteration_count_type.to_string_css t
    | #Single_animation_play_state_type.t as t ->
      Single_animation_play_state_type.to_string_css t
    | #Single_animation_timeline_type.t as t ->
      Single_animation_timeline_type.to_string_css t
    | `None -> "none"
    | #Animation_delay.t as t -> Animation_delay.to_string_css t
    | #Animation_duration.t as t -> Animation_duration.to_string_css t
  ;;
end

module Animation_timeline = struct
  type t = [ | Single_animation_timeline_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Single_animation_timeline_type.t as t ->
      Single_animation_timeline_type.to_string_css t
  ;;
end

module Progress_source_type = struct
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | Animation_timeline.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
    | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    | #Animation_timeline.t as t -> Animation_timeline.to_string_css t
  ;;
end

module Animation_trigger = struct
  type t =
    [ Css_data_type.Ident.Dashed.t
    | Length_percentage_type.t
    | Single_animation_trigger_behavior_type.t
    | `Auto
    | `None
    | `Normal
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Ident.Dashed.t as t -> Css_data_type.Ident.Dashed.to_string_css t
    | #Length_percentage_type.t as t -> Length_percentage_type.to_string_css t
    | #Single_animation_trigger_behavior_type.t as t ->
      Single_animation_trigger_behavior_type.to_string_css t
    | `Auto -> "auto"
    | `None -> "none"
    | `Normal -> "normal"
  ;;
end

module Animation_trigger_timeline = struct
  type t = [ | Single_animation_timeline_type.t ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Single_animation_timeline_type.t as t ->
      Single_animation_timeline_type.to_string_css t
  ;;
end

module Calc_interpolate_fn = struct
  module Input_position_type = struct
    type t =
      [ Css_data_type.Number.t
      | Css_data_type.Percentage.t
      ]

    let to_string_css = function
      | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
      | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    ;;
  end

  type t =
    [ Calc_value_type.t
    | Easing_function_type.t
    | Input_position_type.t
    | Progress_source_type.t
    | `By
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Calc_value_type.t as t -> Calc_value_type.to_string_css t
    | #Easing_function_type.t as t -> Easing_function_type.to_string_css t
    | #Input_position_type.t as t -> Input_position_type.to_string_css t
    | #Progress_source_type.t as t -> Progress_source_type.to_string_css t
    | `By -> "by"
  ;;
end

module Color_interpolate_fn = struct
  module Input_position_type = struct
    type t =
      [ Css_data_type.Number.t
      | Css_data_type.Percentage.t
      ]

    let to_string_css = function
      | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
      | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    ;;
  end

  type t =
    [ Css_data_type.Color.t
    | Color_interpolation_method_type.t
    | Easing_function_type.t
    | Input_position_type.t
    | Progress_source_type.t
    | `By
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Css_data_type.Color.t as t -> Css_data_type.Color.to_string_css t
    | #Color_interpolation_method_type.t as t ->
      Color_interpolation_method_type.to_string_css t
    | #Easing_function_type.t as t -> Easing_function_type.to_string_css t
    | #Input_position_type.t as t -> Input_position_type.to_string_css t
    | #Progress_source_type.t as t -> Progress_source_type.to_string_css t
    | `By -> "by"
  ;;
end

module Interpolate_fn = struct
  module Input_position_type = struct
    type t =
      [ Css_data_type.Number.t
      | Css_data_type.Percentage.t
      ]

    let to_string_css = function
      | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
      | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    ;;
  end

  type t =
    [ Easing_function_type.t
    | Input_position_type.t
    | Keyframes_name_type.t
    | Progress_source_type.t
    | `By
    | `Of
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Easing_function_type.t as t -> Easing_function_type.to_string_css t
    | #Input_position_type.t as t -> Input_position_type.to_string_css t
    | #Keyframes_name_type.t as t -> Keyframes_name_type.to_string_css t
    | #Progress_source_type.t as t -> Progress_source_type.to_string_css t
    | `By -> "by"
    | `Of -> "of"
  ;;
end

module Transform_interpolate_fn = struct
  module Input_position_type = struct
    type t =
      [ Css_data_type.Number.t
      | Css_data_type.Percentage.t
      ]

    let to_string_css = function
      | #Css_data_type.Number.t as t -> Css_data_type.Number.to_string_css t
      | #Css_data_type.Percentage.t as t -> Css_data_type.Percentage.to_string_css t
    ;;
  end

  type t =
    [ Easing_function_type.t
    | Input_position_type.t
    | Progress_source_type.t
    | `By
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Easing_function_type.t as t -> Easing_function_type.to_string_css t
    | #Input_position_type.t as t -> Input_position_type.to_string_css t
    | #Progress_source_type.t as t -> Progress_source_type.to_string_css t
    | `By -> "by"
  ;;
end

module Border_shape = struct
  type t =
    [ Geometry_box_type.t
    | `None
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Geometry_box_type.t as t -> Geometry_box_type.to_string_css t
    | `None -> "none"
  ;;
end

module Clip_path = struct
  type t =
    [ Geometry_box_type.t
    | `None
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Geometry_box_type.t as t -> Geometry_box_type.to_string_css t
    | `None -> "none"
  ;;
end

module Object_view_box = struct
  type t = [ `None ]

  let to_string_css = function
    | `None -> "none"
  ;;
end

module Offset_path = struct
  type t =
    [ Coord_box_type.t
    | `None
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Coord_box_type.t as t -> Coord_box_type.to_string_css t
    | `None -> "none"
  ;;
end

module Offset = struct
  type t =
    [ Offset_anchor.t
    | Offset_distance.t
    | Offset_path.t
    | Offset_position.t
    | Offset_rotate.t
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Offset_anchor.t as t -> Offset_anchor.to_string_css t
    | #Offset_distance.t as t -> Offset_distance.to_string_css t
    | #Offset_path.t as t -> Offset_path.to_string_css t
    | #Offset_position.t as t -> Offset_position.to_string_css t
    | #Offset_rotate.t as t -> Offset_rotate.to_string_css t
  ;;
end

module Shape_inside = struct
  type t =
    [ `Auto
    | `Display
    | `Outside_shape
    | `Shape_box
    ]

  let to_string_css = function
    | `Auto -> "auto"
    | `Display -> "display"
    | `Outside_shape -> "outside-shape"
    | `Shape_box -> "shape-box"
  ;;
end

module Shape_outside = struct
  type t =
    [ Shape_box_type.t
    | `None
    ]

  let to_string_css
    =
    function[@ocaml.warning "-11"]
            (* Silence unused arm error. The type is illustrative and the functionality is
               the same whether or not the arm exists *)
    | #Shape_box_type.t as t -> Shape_box_type.to_string_css t
    | `None -> "none"
  ;;
end

module Shape_subtract = struct
  type t = [ `None ]

  let to_string_css = function
    | `None -> "none"
  ;;
end
