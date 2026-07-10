module Attachment_type : sig
  type t =
    [ `Fixed
    | `Local
    | `Scroll
    ]

  val to_string_css : [< t ] -> string
end

module Autospace_type : sig
  type t =
    [ `Ideograph_alpha
    | `Ideograph_numeric
    | `Insert
    | `No_autospace
    | `Punctuation
    | `Replace
    ]

  val to_string_css : [< t ] -> string
end

module Axis_type : sig
  type t =
    [ `Block
    | `Inline
    | `X
    | `Y
    ]

  val to_string_css : [< t ] -> string
end

module Baseline_position_type : sig
  type t =
    [ `Baseline
    | `First
    | `Last
    ]

  val to_string_css : [< t ] -> string
end

module Blend_mode_type : sig
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

  val to_string_css : [< t ] -> string
end

module Common_lig_values_type : sig
  type t =
    [ `Common_ligatures
    | `No_common_ligatures
    ]

  val to_string_css : [< t ] -> string
end

module Compositing_operator_type : sig
  type t =
    [ `Add
    | `Exclude
    | `Intersect
    | `Subtract
    ]

  val to_string_css : [< t ] -> string
end

module Content_distribution_type : sig
  type t =
    [ `Space_around
    | `Space_between
    | `Space_evenly
    | `Stretch
    ]

  val to_string_css : [< t ] -> string
end

module Content_position_type : sig
  type t =
    [ `Center
    | `End
    | `Flex_end
    | `Flex_start
    | `Start
    ]

  val to_string_css : [< t ] -> string
end

module Contextual_alt_values_type : sig
  type t =
    [ `Contextual
    | `No_contextual
    ]

  val to_string_css : [< t ] -> string
end

module Anchor_name_type : sig
  type t = [ | Css_data_type.Ident.Dashed.t ]

  val to_string_css : [< t ] -> string
end

module Discretionary_lig_values_type : sig
  type t =
    [ `Discretionary_ligatures
    | `No_discretionary_ligatures
    ]

  val to_string_css : [< t ] -> string
end

module East_asian_variant_values_type : sig
  type t =
    [ `Jis04
    | `Jis78
    | `Jis83
    | `Jis90
    | `Simplified
    | `Traditional
    ]

  val to_string_css : [< t ] -> string
end

module East_asian_width_values_type : sig
  type t =
    [ `Full_width
    | `Proportional_width
    ]

  val to_string_css : [< t ] -> string
end

module Historical_lig_values_type : sig
  type t =
    [ `Historical_ligatures
    | `No_historical_ligatures
    ]

  val to_string_css : [< t ] -> string
end

module Feature_value_name_type : sig
  type t = [ | Css_data_type.Ident.t ]

  val to_string_css : [< t ] -> string
end

module Grid_line_type : sig
  type t =
    [ Css_data_type.Ident.Custom.t
    | Css_data_type.Integer.t
    | `Auto
    | `Span
    ]

  val to_string_css : [< t ] -> string
end

module Bottom_type : sig
  type t =
    [ Css_data_type.Length.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Left_type : sig
  type t =
    [ Css_data_type.Length.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Line_names_type : sig
  type t = [ | Css_data_type.Ident.Custom.t ]

  val to_string_css : [< t ] -> string
end

module Line_style_type : sig
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

  val to_string_css : [< t ] -> string
end

module Line_width_type : sig
  type t =
    [ Css_data_type.Length.t
    | `Medium
    | `Thick
    | `Thin
    ]

  val to_string_css : [< t ] -> string
end

module Gap_rule_type : sig
  type t =
    [ Css_data_type.Color.t
    | Line_style_type.t
    | Line_width_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Gap_rule_or_repeat_type : sig
  type t =
    [ Gap_rule_type.t
    | Css_data_type.Integer.t
    ]

  val to_string_css : [< t ] -> string
end

module Gap_auto_rule_list_type : sig
  type t =
    [ Gap_rule_type.t
    | Gap_rule_or_repeat_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Gap_rule_list_type : sig
  type t = [ | Gap_rule_or_repeat_type.t ]

  val to_string_css : [< t ] -> string
end

module Masking_mode_type : sig
  type t =
    [ `Alpha_
    | `Luminance
    | `Match_source
    ]

  val to_string_css : [< t ] -> string
end

module Line_name_list_type : sig
  type t =
    [ Css_data_type.Integer.t
    | Line_names_type.t
    | `Auto_fill
    ]

  val to_string_css : [< t ] -> string
end

module Numeric_figure_values_type : sig
  type t =
    [ `Lining_nums
    | `Oldstyle_nums
    ]

  val to_string_css : [< t ] -> string
end

module Numeric_fraction_values_type : sig
  type t =
    [ `Diagonal_fractions
    | `Stacked_fractions
    ]

  val to_string_css : [< t ] -> string
end

module Numeric_spacing_values_type : sig
  type t =
    [ `Proportional_nums
    | `Tabular_nums
    ]

  val to_string_css : [< t ] -> string
end

module Overflow_position_type : sig
  type t =
    [ `Safe
    | `Unsafe
    ]

  val to_string_css : [< t ] -> string
end

module Alpha_value_type : sig
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    ]

  val to_string_css : [< t ] -> string
end

module Calc_value_type : sig
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `Negative_infinity
    | `NaN
    | `E
    | `Infinity
    | `Pi
    ]

  val to_string_css : [< t ] -> string
end

module Length_percentage_type : sig
  type t =
    [ Css_data_type.Length.t
    | Css_data_type.Percentage.t
    ]

  val to_string_css : [< t ] -> string
end

module Bg_size_type : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    | `Contain
    | `Cover
    ]

  val to_string_css : [< t ] -> string
end

module Inflexible_breadth_type : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    | `Max_content
    | `Min_content
    ]

  val to_string_css : [< t ] -> string
end

module Color_stop_list_type : sig
  type t =
    [ Css_data_type.Color.t
    | Length_percentage_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Position_type : sig
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

  val to_string_css : [< t ] -> string
end

module Bg_position_type : sig
  type t =
    [ Length_percentage_type.t
    | Position_type.t
    | `Bottom
    | `Center
    | `Left
    | `Right
    | `Top
    ]

  val to_string_css : [< t ] -> string
end

module Random_value_sharing_type : sig
  type t =
    [ Css_data_type.Ident.Dashed.t
    | Css_data_type.Number.t
    | `Auto
    | `Element_shared
    | `Fixed
    ]

  val to_string_css : [< t ] -> string
end

module Ratio_type : sig
  type t = [ | Css_data_type.Number.t ]

  val to_string_css : [< t ] -> string
end

module Color_interpolation_method_type : sig
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

  val to_string_css : [< t ] -> string
end

module Radial_gradient_syntax_type : sig
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

  val to_string_css : [< t ] -> string
end

module Line_color_or_repeat_type : sig
  type t =
    [ Css_data_type.Color.t
    | Css_data_type.Integer.t
    ]

  val to_string_css : [< t ] -> string
end

module Auto_line_color_list_type : sig
  type t =
    [ Css_data_type.Color.t
    | Line_color_or_repeat_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Line_color_list_type : sig
  type t = [ | Line_color_or_repeat_type.t ]

  val to_string_css : [< t ] -> string
end

module Line_style_or_repeat_type : sig
  type t =
    [ Css_data_type.Integer.t
    | Line_style_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Auto_line_style_list_type : sig
  type t =
    [ Line_style_type.t
    | Line_style_or_repeat_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Line_style_list_type : sig
  type t = [ | Line_style_or_repeat_type.t ]

  val to_string_css : [< t ] -> string
end

module Line_width_or_repeat_type : sig
  type t =
    [ Css_data_type.Integer.t
    | Line_width_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Auto_line_width_list_type : sig
  type t =
    [ Line_width_type.t
    | Line_width_or_repeat_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Line_width_list_type : sig
  type t = [ | Line_width_or_repeat_type.t ]

  val to_string_css : [< t ] -> string
end

module Repetition_type : sig
  type t =
    [ `No_repeat
    | `Repeat
    | `Round
    | `Space
    ]

  val to_string_css : [< t ] -> string
end

module Repeat_style_type : sig
  type t =
    [ Repetition_type.t
    | `Repeat_x
    | `Repeat_y
    ]

  val to_string_css : [< t ] -> string
end

module Right_type : sig
  type t =
    [ Css_data_type.Length.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Self_position_type : sig
  type t =
    [ `Center
    | `End
    | `Flex_end
    | `Flex_start
    | `Self_end
    | `Self_start
    | `Start
    ]

  val to_string_css : [< t ] -> string
end

module Single_animation_direction_type : sig
  type t =
    [ `Alternate
    | `Alternate_reverse
    | `Normal
    | `Reverse
    ]

  val to_string_css : [< t ] -> string
end

module Single_animation_fill_mode_type : sig
  type t =
    [ `Backwards
    | `Both
    | `Forwards
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Single_animation_iteration_count_type : sig
  type t =
    [ Css_data_type.Number.t
    | `Infinite
    ]

  val to_string_css : [< t ] -> string
end

module Single_animation_play_state_type : sig
  type t =
    [ `Paused
    | `Running
    ]

  val to_string_css : [< t ] -> string
end

module Single_animation_trigger_behavior_type : sig
  type t =
    [ `Alternate
    | `Once
    | `Repeat
    | `State
    ]

  val to_string_css : [< t ] -> string
end

module Single_transition_property_type : sig
  type t =
    [ Css_data_type.Ident.Custom.t
    | `All
    ]

  val to_string_css : [< t ] -> string
end

module Spacing_trim_type : sig
  type t =
    [ `Normal
    | `Space_all
    | `Space_first
    | `Trim_all
    | `Trim_both
    | `Trim_start
    ]

  val to_string_css : [< t ] -> string
end

module Family_name_type : sig
  type t =
    [ Css_data_type.Ident.Custom.t
    | Css_data_type.Css_string.t
    ]

  val to_string_css : [< t ] -> string
end

module Keyframes_name_type : sig
  type t =
    [ Css_data_type.Ident.Custom.t
    | Css_data_type.Css_string.t
    ]

  val to_string_css : [< t ] -> string
end

module Opentype_tag_type : sig
  type t = [ | Css_data_type.Css_string.t ]

  val to_string_css : [< t ] -> string
end

module Syntax_type : sig
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

  val to_string_css : [< t ] -> string
end

module Text_edge_type : sig
  type t =
    [ `Alphabetic
    | `Cap
    | `Ex
    | `Ideographic
    | `Ideographic_ink
    | `Text
    ]

  val to_string_css : [< t ] -> string
end

module Top_type : sig
  type t =
    [ Css_data_type.Length.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Track_breadth_type : sig
  type t =
    [ Css_data_type.Flex_fr.t
    | Length_percentage_type.t
    | `Auto
    | `Max_content
    | `Min_content
    ]

  val to_string_css : [< t ] -> string
end

module Auto_track_list_type : sig
  type t =
    [ Inflexible_breadth_type.t
    | Css_data_type.Integer.t
    | Length_percentage_type.t
    | Line_names_type.t
    | Track_breadth_type.t
    | `Auto_fill
    | `Auto_fit
    ]

  val to_string_css : [< t ] -> string
end

module Track_size_type : sig
  type t =
    [ Inflexible_breadth_type.t
    | Length_percentage_type.t
    | Track_breadth_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Track_list_type : sig
  type t =
    [ Css_data_type.Integer.t
    | Line_names_type.t
    | Track_size_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Transition_behavior_value_type : sig
  type t =
    [ `Allow_discrete
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Shape_command_type : sig
  type t = [ `Close ]

  val to_string_css : [< t ] -> string
end

module Visual_box_type : sig
  type t =
    [ `Border_box
    | `Content_box
    | `Padding_box
    ]

  val to_string_css : [< t ] -> string
end

module Coord_box_type : sig
  type t =
    [ Visual_box_type.t
    | `Fill_box
    | `Stroke_box
    | `View_box
    ]

  val to_string_css : [< t ] -> string
end

module Shape_box_type : sig
  type t =
    [ Visual_box_type.t
    | `Margin_box
    ]

  val to_string_css : [< t ] -> string
end

module Geometry_box_type : sig
  type t =
    [ Shape_box_type.t
    | `Fill_box
    | `Stroke_box
    | `View_box
    ]

  val to_string_css : [< t ] -> string
end

module Zero_type : sig
  type t = [ `Zero ]

  val to_string_css : [< t ] -> string
end

module Conic_gradient_syntax_type : sig
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

  val to_string_css : [< t ] -> string
end

module Linear_gradient_syntax_type : sig
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

  val to_string_css : [< t ] -> string
end

module Accent_color : sig
  type t =
    [ Css_data_type.Color.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Align_content : sig
  type t =
    [ Baseline_position_type.t
    | Content_distribution_type.t
    | Content_position_type.t
    | Overflow_position_type.t
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Align_items : sig
  type t =
    [ Baseline_position_type.t
    | Overflow_position_type.t
    | Self_position_type.t
    | `Anchor_center
    | `Normal
    | `Stretch
    ]

  val to_string_css : [< t ] -> string
end

module Align_self : sig
  type t =
    [ Baseline_position_type.t
    | Overflow_position_type.t
    | Self_position_type.t
    | `Anchor_center
    | `Auto
    | `Normal
    | `Stretch
    ]

  val to_string_css : [< t ] -> string
end

module Alignment_baseline : sig
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

  val to_string_css : [< t ] -> string
end

module All : sig
  type t =
    [ `Inherit
    | `Initial
    | `Revert
    | `Revert_layer
    | `Unset
    ]

  val to_string_css : [< t ] -> string
end

module Anchor_name : sig
  type t =
    [ Css_data_type.Ident.Dashed.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Anchor_scope : sig
  type t =
    [ Css_data_type.Ident.Dashed.t
    | `All
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Animation_composition : sig
  type t =
    [ `Accumulate
    | `Add
    | `Replace
    ]

  val to_string_css : [< t ] -> string
end

module Animation_delay : sig
  type t = [ | Css_data_type.Time.t ]

  val to_string_css : [< t ] -> string
end

module Animation_direction : sig
  type t = [ | Single_animation_direction_type.t ]

  val to_string_css : [< t ] -> string
end

module Animation_duration : sig
  type t =
    [ Css_data_type.Time.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Animation_fill_mode : sig
  type t = [ | Single_animation_fill_mode_type.t ]

  val to_string_css : [< t ] -> string
end

module Animation_iteration_count : sig
  type t = [ | Single_animation_iteration_count_type.t ]

  val to_string_css : [< t ] -> string
end

module Animation_name : sig
  type t =
    [ Keyframes_name_type.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Animation_play_state : sig
  type t = [ | Single_animation_play_state_type.t ]

  val to_string_css : [< t ] -> string
end

module Animation_range_center : sig
  type t =
    [ Length_percentage_type.t
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Animation_range_end : sig
  type t =
    [ Length_percentage_type.t
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Animation_range_start : sig
  type t =
    [ Length_percentage_type.t
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Animation_range : sig
  type t =
    [ Animation_range_end.t
    | Animation_range_start.t
    ]

  val to_string_css : [< t ] -> string
end

module Animation_trigger_behavior : sig
  type t = [ | Single_animation_trigger_behavior_type.t ]

  val to_string_css : [< t ] -> string
end

module Animation_trigger_exit_range_end : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Animation_trigger_exit_range_start : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Animation_trigger_exit_range : sig
  type t =
    [ Animation_trigger_exit_range_end.t
    | Animation_trigger_exit_range_start.t
    ]

  val to_string_css : [< t ] -> string
end

module Animation_trigger_range_end : sig
  type t =
    [ Length_percentage_type.t
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Animation_trigger_range_start : sig
  type t =
    [ Length_percentage_type.t
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Animation_trigger_range : sig
  type t =
    [ Animation_trigger_range_end.t
    | Animation_trigger_range_start.t
    ]

  val to_string_css : [< t ] -> string
end

module Appearance : sig
  module Compat_auto_type : sig
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

    val to_string_css : [< t ] -> string
  end

  module Compat_special_type : sig
    type t =
      [ `Menulist_button
      | `Textfield
      ]

    val to_string_css : [< t ] -> string
  end

  type t =
    [ Compat_auto_type.t
    | Compat_special_type.t
    | `Auto
    | `Base
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Aspect_ratio : sig
  type t =
    [ Ratio_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Backface_visibility : sig
  type t =
    [ `Hidden
    | `Visible
    ]

  val to_string_css : [< t ] -> string
end

module Background_attachment : sig
  type t = [ | Attachment_type.t ]

  val to_string_css : [< t ] -> string
end

module Background_clip : sig
  type t =
    [ Visual_box_type.t
    | `Border_area
    | `Text
    ]

  val to_string_css : [< t ] -> string
end

module Background_color : sig
  type t = [ | Css_data_type.Color.t ]

  val to_string_css : [< t ] -> string
end

module Background_origin : sig
  type t = [ | Visual_box_type.t ]

  val to_string_css : [< t ] -> string
end

module Background_position : sig
  type t = [ | Bg_position_type.t ]

  val to_string_css : [< t ] -> string
end

module Background_position_block : sig
  type t =
    [ Length_percentage_type.t
    | `Center
    | `End
    | `Start
    ]

  val to_string_css : [< t ] -> string
end

module Background_position_inline : sig
  type t =
    [ Length_percentage_type.t
    | `Center
    | `End
    | `Start
    ]

  val to_string_css : [< t ] -> string
end

module Background_position_x : sig
  type t =
    [ Length_percentage_type.t
    | `Center
    | `Left
    | `Right
    | `X_end
    | `X_start
    ]

  val to_string_css : [< t ] -> string
end

module Background_position_y : sig
  type t =
    [ Length_percentage_type.t
    | `Bottom
    | `Center
    | `Top
    | `Y_end
    | `Y_start
    ]

  val to_string_css : [< t ] -> string
end

module Background_repeat : sig
  type t = [ | Repeat_style_type.t ]

  val to_string_css : [< t ] -> string
end

module Background_repeat_block : sig
  type t = [ | Repetition_type.t ]

  val to_string_css : [< t ] -> string
end

module Background_repeat_inline : sig
  type t = [ | Repetition_type.t ]

  val to_string_css : [< t ] -> string
end

module Background_repeat_x : sig
  type t = [ | Repetition_type.t ]

  val to_string_css : [< t ] -> string
end

module Background_repeat_y : sig
  type t = [ | Repetition_type.t ]

  val to_string_css : [< t ] -> string
end

module Background_size : sig
  type t = [ | Bg_size_type.t ]

  val to_string_css : [< t ] -> string
end

module Baseline_shift : sig
  type t =
    [ Length_percentage_type.t
    | `Bottom
    | `Center
    | `Sub
    | `Super
    | `Top
    ]

  val to_string_css : [< t ] -> string
end

module Baseline_source : sig
  type t =
    [ `Auto
    | `First
    | `Last
    ]

  val to_string_css : [< t ] -> string
end

module Block_ellipsis : sig
  type t =
    [ Css_data_type.Css_string.t
    | `Auto
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Block_step_align : sig
  type t =
    [ `Auto
    | `Center
    | `End
    | `Start
    ]

  val to_string_css : [< t ] -> string
end

module Block_step_insert : sig
  type t =
    [ `Content_box
    | `Margin_box
    | `Padding_box
    ]

  val to_string_css : [< t ] -> string
end

module Block_step_round : sig
  type t =
    [ `Down
    | `Nearest
    | `Up
    ]

  val to_string_css : [< t ] -> string
end

module Block_step_size : sig
  type t =
    [ Css_data_type.Length.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Block_step : sig
  type t =
    [ Block_step_align.t
    | Block_step_insert.t
    | Block_step_round.t
    | Block_step_size.t
    ]

  val to_string_css : [< t ] -> string
end

module Bookmark_level : sig
  type t =
    [ Css_data_type.Integer.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Bookmark_state : sig
  type t =
    [ `Closed
    | `Open
    ]

  val to_string_css : [< t ] -> string
end

module Border : sig
  type t =
    [ Css_data_type.Color.t
    | Line_style_type.t
    | Line_width_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Border_block_end : sig
  type t =
    [ Css_data_type.Color.t
    | Line_style_type.t
    | Line_width_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Border_block_end_radius : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_block_end_style : sig
  type t = [ | Line_style_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_block_end_width : sig
  type t = [ | Line_width_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_block_start : sig
  type t =
    [ Css_data_type.Color.t
    | Line_style_type.t
    | Line_width_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Border_block : sig
  type t = [ | Border_block_start.t ]

  val to_string_css : [< t ] -> string
end

module Border_block_start_radius : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_block_start_style : sig
  type t = [ | Line_style_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_block_start_width : sig
  type t = [ | Line_width_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_bottom : sig
  type t =
    [ Css_data_type.Color.t
    | Line_style_type.t
    | Line_width_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Border_bottom_left_radius : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_bottom_radius : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_bottom_right_radius : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_bottom_style : sig
  type t = [ | Line_style_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_bottom_width : sig
  type t = [ | Line_width_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_boundary : sig
  type t =
    [ `Display
    | `None
    | `Parent
    ]

  val to_string_css : [< t ] -> string
end

module Border_clip : sig
  type t =
    [ Css_data_type.Flex_fr.t
    | Length_percentage_type.t
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Border_clip_bottom : sig
  type t =
    [ Css_data_type.Flex_fr.t
    | Length_percentage_type.t
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Border_clip_left : sig
  type t =
    [ Css_data_type.Flex_fr.t
    | Length_percentage_type.t
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Border_clip_right : sig
  type t =
    [ Css_data_type.Flex_fr.t
    | Length_percentage_type.t
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Border_clip_top : sig
  type t =
    [ Css_data_type.Flex_fr.t
    | Length_percentage_type.t
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Border_collapse : sig
  type t =
    [ `Collapse
    | `Separate
    ]

  val to_string_css : [< t ] -> string
end

module Border_end_end_radius : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_end_start_radius : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_image_outset : sig
  type t =
    [ Css_data_type.Length.t
    | Css_data_type.Number.t
    ]

  val to_string_css : [< t ] -> string
end

module Border_image_repeat : sig
  type t =
    [ `Repeat
    | `Round
    | `Space
    | `Stretch
    ]

  val to_string_css : [< t ] -> string
end

module Border_image_slice : sig
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `Fill
    ]

  val to_string_css : [< t ] -> string
end

module Border_image_width : sig
  type t =
    [ Length_percentage_type.t
    | Css_data_type.Number.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Border_inline : sig
  type t = [ | Border_block_start.t ]

  val to_string_css : [< t ] -> string
end

module Border_inline_end : sig
  type t =
    [ Css_data_type.Color.t
    | Line_style_type.t
    | Line_width_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Border_inline_end_radius : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_inline_end_style : sig
  type t = [ | Line_style_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_inline_end_width : sig
  type t = [ | Line_width_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_inline_start : sig
  type t =
    [ Css_data_type.Color.t
    | Line_style_type.t
    | Line_width_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Border_inline_start_radius : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_inline_start_style : sig
  type t = [ | Line_style_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_inline_start_width : sig
  type t = [ | Line_width_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_left : sig
  type t =
    [ Css_data_type.Color.t
    | Line_style_type.t
    | Line_width_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Border_left_radius : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_left_style : sig
  type t = [ | Line_style_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_left_width : sig
  type t = [ | Line_width_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_limit : sig
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

  val to_string_css : [< t ] -> string
end

module Border_radius : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_right : sig
  type t =
    [ Css_data_type.Color.t
    | Line_style_type.t
    | Line_width_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Border_right_radius : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_right_style : sig
  type t = [ | Line_style_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_right_width : sig
  type t = [ | Line_width_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_spacing : sig
  type t = [ | Css_data_type.Length.t ]

  val to_string_css : [< t ] -> string
end

module Border_start_end_radius : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_start_start_radius : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_style : sig
  type t = [ | Line_style_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_top : sig
  type t =
    [ Css_data_type.Color.t
    | Line_style_type.t
    | Line_width_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Border_top_left_radius : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_top_radius : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_top_right_radius : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_top_style : sig
  type t = [ | Line_style_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_block_style : sig
  type t = [ | Border_top_style.t ]

  val to_string_css : [< t ] -> string
end

module Border_inline_style : sig
  type t = [ | Border_top_style.t ]

  val to_string_css : [< t ] -> string
end

module Border_top_width : sig
  type t = [ | Line_width_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_block_width : sig
  type t = [ | Border_top_width.t ]

  val to_string_css : [< t ] -> string
end

module Border_inline_width : sig
  type t = [ | Border_top_width.t ]

  val to_string_css : [< t ] -> string
end

module Border_width : sig
  type t = [ | Line_width_type.t ]

  val to_string_css : [< t ] -> string
end

module Box_decoration_break : sig
  type t =
    [ `Clone
    | `Slice
    ]

  val to_string_css : [< t ] -> string
end

module Box_shadow_blur : sig
  type t = [ | Css_data_type.Length.t ]

  val to_string_css : [< t ] -> string
end

module Box_shadow_color : sig
  type t = [ | Css_data_type.Color.t ]

  val to_string_css : [< t ] -> string
end

module Box_shadow_offset : sig
  type t =
    [ Css_data_type.Length.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Box_shadow_position : sig
  type t =
    [ `Inset
    | `Outset
    ]

  val to_string_css : [< t ] -> string
end

module Box_shadow_spread : sig
  type t = [ | Css_data_type.Length.t ]

  val to_string_css : [< t ] -> string
end

module Box_shadow : sig
  type t =
    [ Box_shadow_blur.t
    | Box_shadow_color.t
    | Box_shadow_offset.t
    | Box_shadow_position.t
    | Box_shadow_spread.t
    ]

  val to_string_css : [< t ] -> string
end

module Box_sizing : sig
  type t =
    [ `Border_box
    | `Content_box
    ]

  val to_string_css : [< t ] -> string
end

module Box_snap : sig
  type t =
    [ `Baseline
    | `Block_end
    | `Block_start
    | `Center
    | `Last_baseline
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Break_after : sig
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

  val to_string_css : [< t ] -> string
end

module Break_before : sig
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

  val to_string_css : [< t ] -> string
end

module Break_inside : sig
  type t =
    [ `Auto
    | `Avoid
    | `Avoid_column
    | `Avoid_page
    | `Avoid_region
    ]

  val to_string_css : [< t ] -> string
end

module Caption_side : sig
  type t =
    [ `Bottom
    | `Top
    ]

  val to_string_css : [< t ] -> string
end

module Caret_animation : sig
  type t =
    [ `Auto
    | `Manual
    ]

  val to_string_css : [< t ] -> string
end

module Caret_color : sig
  type t =
    [ Css_data_type.Color.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Caret_shape : sig
  type t =
    [ `Auto
    | `Bar
    | `Block
    | `Underscore
    ]

  val to_string_css : [< t ] -> string
end

module Caret : sig
  type t =
    [ Caret_animation.t
    | Caret_color.t
    | Caret_shape.t
    ]

  val to_string_css : [< t ] -> string
end

module Clear : sig
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

  val to_string_css : [< t ] -> string
end

module Clip_rule : sig
  type t =
    [ `Evenodd
    | `Nonzero
    ]

  val to_string_css : [< t ] -> string
end

module Color_interpolation : sig
  type t =
    [ `Auto
    | `LinearRGB
    | `Srgb
    ]

  val to_string_css : [< t ] -> string
end

module Color_interpolation_filters : sig
  type t =
    [ `Auto
    | `LinearRGB
    | `Srgb
    ]

  val to_string_css : [< t ] -> string
end

module Color_scheme : sig
  type t =
    [ Css_data_type.Ident.Custom.t
    | `Dark
    | `Light
    | `Normal
    | `Only
    ]

  val to_string_css : [< t ] -> string
end

module Column_count : sig
  type t =
    [ Css_data_type.Integer.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Column_fill : sig
  type t =
    [ `Auto
    | `Balance
    | `Balance_all
    ]

  val to_string_css : [< t ] -> string
end

module Column_gap : sig
  type t =
    [ Length_percentage_type.t
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Column_height : sig
  type t =
    [ Css_data_type.Length.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Column_rule : sig
  type t =
    [ Gap_auto_rule_list_type.t
    | Gap_rule_list_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Column_rule_break : sig
  type t =
    [ `Intersection
    | `None
    | `Spanning_item
    ]

  val to_string_css : [< t ] -> string
end

module Column_rule_color : sig
  type t =
    [ Auto_line_color_list_type.t
    | Line_color_list_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Column_rule_outset : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Column_rule_style : sig
  type t =
    [ Auto_line_style_list_type.t
    | Line_style_list_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Column_rule_width : sig
  type t =
    [ Auto_line_width_list_type.t
    | Line_width_list_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Column_span : sig
  type t =
    [ Css_data_type.Integer.t
    | `All
    | `Auto
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Column_width : sig
  type t =
    [ Css_data_type.Length.t
    | Length_percentage_type.t
    | `Auto
    | `Max_content
    | `Min_content
    ]

  val to_string_css : [< t ] -> string
end

module Column_wrap : sig
  type t =
    [ `Auto
    | `Nowrap
    | `Wrap
    ]

  val to_string_css : [< t ] -> string
end

module Columns : sig
  type t =
    [ Column_count.t
    | Column_height.t
    | Column_width.t
    ]

  val to_string_css : [< t ] -> string
end

module Contain : sig
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

  val to_string_css : [< t ] -> string
end

module Contain_intrinsic_block_size : sig
  type t =
    [ Css_data_type.Length.t
    | `Auto
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Contain_intrinsic_height : sig
  type t =
    [ Css_data_type.Length.t
    | `Auto
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Contain_intrinsic_inline_size : sig
  type t =
    [ Css_data_type.Length.t
    | `Auto
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Contain_intrinsic_size : sig
  type t =
    [ Css_data_type.Length.t
    | `Auto
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Contain_intrinsic_width : sig
  type t =
    [ Css_data_type.Length.t
    | `Auto
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Container_name : sig
  type t =
    [ Css_data_type.Ident.Custom.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Container_type : sig
  type t =
    [ `Inline_size
    | `Normal
    | `Scroll_state
    | `Size
    ]

  val to_string_css : [< t ] -> string
end

module Container : sig
  type t =
    [ Container_name.t
    | Container_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Content_visibility : sig
  type t =
    [ `Auto
    | `Hidden
    | `Visible
    ]

  val to_string_css : [< t ] -> string
end

module Continue : sig
  type t =
    [ `Auto
    | `Collapse
    | `Discard
    | `Fragments
    | `Overflow
    | `Paginate
    ]

  val to_string_css : [< t ] -> string
end

module Counter_increment : sig
  type t =
    [ Css_data_type.Integer.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Counter_reset : sig
  type t =
    [ Css_data_type.Integer.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Counter_set : sig
  type t =
    [ Css_data_type.Integer.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Cue_after : sig
  type t =
    [ Css_data_type.Decibel.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Cue_before : sig
  type t =
    [ Css_data_type.Decibel.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Cue : sig
  type t =
    [ Cue_after.t
    | Cue_before.t
    ]

  val to_string_css : [< t ] -> string
end

module Cx : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Cy : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module D : sig
  type t =
    [ Css_data_type.Css_string.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Direction : sig
  type t =
    [ `Ltr
    | `Rtl
    ]

  val to_string_css : [< t ] -> string
end

module Display : sig
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

  val to_string_css : [< t ] -> string
end

module Dominant_baseline : sig
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

  val to_string_css : [< t ] -> string
end

module Empty_cells : sig
  type t =
    [ `Hide
    | `Show
    ]

  val to_string_css : [< t ] -> string
end

module Field_sizing : sig
  type t =
    [ `Content
    | `Fixed
    ]

  val to_string_css : [< t ] -> string
end

module Fill_break : sig
  type t =
    [ `Bounding_box
    | `Clone
    | `Slice
    ]

  val to_string_css : [< t ] -> string
end

module Fill_color : sig
  type t = [ | Css_data_type.Color.t ]

  val to_string_css : [< t ] -> string
end

module Fill_origin : sig
  type t =
    [ `Border_box
    | `Content_box
    | `Fill_box
    | `Match_parent
    | `Padding_box
    | `Stroke_box
    ]

  val to_string_css : [< t ] -> string
end

module Fill_position : sig
  type t = [ | Position_type.t ]

  val to_string_css : [< t ] -> string
end

module Fill_repeat : sig
  type t = [ | Repeat_style_type.t ]

  val to_string_css : [< t ] -> string
end

module Fill_rule : sig
  type t =
    [ `Evenodd
    | `Nonzero
    ]

  val to_string_css : [< t ] -> string
end

module Fill_size : sig
  type t = [ | Bg_size_type.t ]

  val to_string_css : [< t ] -> string
end

module Flex_direction : sig
  type t =
    [ `Column
    | `Column_reverse
    | `Row
    | `Row_reverse
    ]

  val to_string_css : [< t ] -> string
end

module Flex_grow : sig
  type t = [ | Css_data_type.Number.t ]

  val to_string_css : [< t ] -> string
end

module Flex_shrink : sig
  type t = [ | Css_data_type.Number.t ]

  val to_string_css : [< t ] -> string
end

module Flex_wrap : sig
  type t =
    [ `Nowrap
    | `Wrap
    | `Wrap_reverse
    ]

  val to_string_css : [< t ] -> string
end

module Flex_flow : sig
  type t =
    [ Flex_direction.t
    | Flex_wrap.t
    ]

  val to_string_css : [< t ] -> string
end

module Float_defer : sig
  type t =
    [ Css_data_type.Integer.t
    | `Last
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Float_offset : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Float_reference : sig
  type t =
    [ `Column
    | `Inline
    | `Page
    | `Region
    ]

  val to_string_css : [< t ] -> string
end

module Flood_color : sig
  type t = [ | Css_data_type.Color.t ]

  val to_string_css : [< t ] -> string
end

module Flow_from : sig
  type t =
    [ Css_data_type.Ident.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Flow_into : sig
  type t =
    [ Css_data_type.Ident.t
    | `Content
    | `Element
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Font_family : sig
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

  val to_string_css : [< t ] -> string
end

module Font_feature_settings : sig
  type t =
    [ Css_data_type.Integer.t
    | Opentype_tag_type.t
    | `Normal
    | `Off
    | `On
    ]

  val to_string_css : [< t ] -> string
end

module Font_kerning : sig
  type t =
    [ `Auto
    | `None
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Font_language_override : sig
  type t =
    [ Css_data_type.Css_string.t
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Font_optical_sizing : sig
  type t =
    [ `Auto
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Font_size : sig
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

  val to_string_css : [< t ] -> string
end

module Font_size_adjust : sig
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

  val to_string_css : [< t ] -> string
end

module Font_stretch : sig
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

  val to_string_css : [< t ] -> string
end

module Font_style : sig
  type t =
    [ Css_data_type.Angle.t
    | `Italic
    | `Left
    | `Normal
    | `Oblique
    | `Right
    ]

  val to_string_css : [< t ] -> string
end

module Font_synthesis : sig
  type t =
    [ `None
    | `Position
    | `Small_caps
    | `Style
    | `Weight
    ]

  val to_string_css : [< t ] -> string
end

module Font_synthesis_position : sig
  type t =
    [ `Auto
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Font_synthesis_small_caps : sig
  type t =
    [ `Auto
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Font_synthesis_style : sig
  type t =
    [ `Auto
    | `None
    | `Oblique_only
    ]

  val to_string_css : [< t ] -> string
end

module Font_synthesis_weight : sig
  type t =
    [ `Auto
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Font_variant : sig
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

  val to_string_css : [< t ] -> string
end

module Font_variant_alternates : sig
  type t =
    [ Feature_value_name_type.t
    | `Historical_forms
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Font_variant_caps : sig
  type t =
    [ `All_petite_caps
    | `All_small_caps
    | `Normal
    | `Petite_caps
    | `Small_caps
    | `Titling_caps
    | `Unicase
    ]

  val to_string_css : [< t ] -> string
end

module Font_variant_east_asian : sig
  type t =
    [ East_asian_variant_values_type.t
    | East_asian_width_values_type.t
    | `Normal
    | `Ruby
    ]

  val to_string_css : [< t ] -> string
end

module Font_variant_emoji : sig
  type t =
    [ `Emoji
    | `Normal
    | `Text
    | `Unicode
    ]

  val to_string_css : [< t ] -> string
end

module Font_variant_ligatures : sig
  type t =
    [ Common_lig_values_type.t
    | Contextual_alt_values_type.t
    | Discretionary_lig_values_type.t
    | Historical_lig_values_type.t
    | `None
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Font_variant_numeric : sig
  type t =
    [ Numeric_figure_values_type.t
    | Numeric_fraction_values_type.t
    | Numeric_spacing_values_type.t
    | `Normal
    | `Ordinal
    | `Slashed_zero
    ]

  val to_string_css : [< t ] -> string
end

module Font_variant_position : sig
  type t =
    [ `Normal
    | `Sub
    | `Super
    ]

  val to_string_css : [< t ] -> string
end

module Font_variation_settings : sig
  type t =
    [ Css_data_type.Number.t
    | Opentype_tag_type.t
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Font_weight : sig
  type t =
    [ Css_data_type.Number.t
    | `Bold
    | `Bolder
    | `Lighter
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Font_width : sig
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

  val to_string_css : [< t ] -> string
end

module Footnote_display : sig
  type t =
    [ `Block
    | `Compact
    | `Inline
    ]

  val to_string_css : [< t ] -> string
end

module Footnote_policy : sig
  type t =
    [ `Auto
    | `Block
    | `Line
    ]

  val to_string_css : [< t ] -> string
end

module Forced_color_adjust : sig
  type t =
    [ `Auto
    | `None
    | `Preserve_parent_color
    ]

  val to_string_css : [< t ] -> string
end

module Glyph_orientation_vertical : sig
  type t =
    [ `Zero
    | `Zero_deg
    | `Ninety
    | `Ninety_deg
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Grid_area : sig
  type t = [ | Grid_line_type.t ]

  val to_string_css : [< t ] -> string
end

module Grid_auto_columns : sig
  type t = [ | Track_size_type.t ]

  val to_string_css : [< t ] -> string
end

module Grid_auto_flow : sig
  type t =
    [ `Column
    | `Dense
    | `Row
    ]

  val to_string_css : [< t ] -> string
end

module Grid_auto_rows : sig
  type t = [ | Track_size_type.t ]

  val to_string_css : [< t ] -> string
end

module Grid_column : sig
  type t = [ | Grid_line_type.t ]

  val to_string_css : [< t ] -> string
end

module Grid_column_end : sig
  type t = [ | Grid_line_type.t ]

  val to_string_css : [< t ] -> string
end

module Grid_column_gap : sig
  type t =
    [ Length_percentage_type.t
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Grid_column_start : sig
  type t = [ | Grid_line_type.t ]

  val to_string_css : [< t ] -> string
end

module Grid_row : sig
  type t = [ | Grid_line_type.t ]

  val to_string_css : [< t ] -> string
end

module Grid_row_end : sig
  type t = [ | Grid_line_type.t ]

  val to_string_css : [< t ] -> string
end

module Grid_row_gap : sig
  type t =
    [ Length_percentage_type.t
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Grid_row_start : sig
  type t = [ | Grid_line_type.t ]

  val to_string_css : [< t ] -> string
end

module Grid_template_areas : sig
  type t =
    [ Css_data_type.Css_string.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Hanging_punctuation : sig
  type t =
    [ `Allow_end
    | `First
    | `Force_end
    | `Last
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Hyphenate_character : sig
  type t =
    [ Css_data_type.Css_string.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Hyphenate_limit_chars : sig
  type t =
    [ Css_data_type.Integer.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Hyphenate_limit_last : sig
  type t =
    [ `Always
    | `Column
    | `None
    | `Page
    | `Spread
    ]

  val to_string_css : [< t ] -> string
end

module Hyphenate_limit_lines : sig
  type t =
    [ Css_data_type.Integer.t
    | `No_limit
    ]

  val to_string_css : [< t ] -> string
end

module Hyphenate_limit_zone : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Hyphens : sig
  type t =
    [ `Auto
    | `Manual
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Image_orientation : sig
  type t =
    [ Css_data_type.Angle.t
    | `Flip
    | `From_image
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Image_rendering : sig
  type t =
    [ `Auto
    | `Crisp_edges
    | `High_quality
    | `Pixelated
    | `Smooth
    ]

  val to_string_css : [< t ] -> string
end

module Image_resolution : sig
  type t =
    [ Css_data_type.Resolution.t
    | `From_image
    | `Snap
    ]

  val to_string_css : [< t ] -> string
end

module Initial_letter : sig
  type t =
    [ Css_data_type.Integer.t
    | Css_data_type.Number.t
    | `Drop
    | `Normal
    | `Raise
    ]

  val to_string_css : [< t ] -> string
end

module Initial_letter_align : sig
  type t =
    [ `Alphabetic
    | `Border_box
    | `Hanging
    | `Ideographic
    | `Leading
    ]

  val to_string_css : [< t ] -> string
end

module Initial_letter_wrap : sig
  type t =
    [ Length_percentage_type.t
    | `All
    | `First
    | `Grid
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Inline_sizing : sig
  type t =
    [ `Normal
    | `Stretch
    ]

  val to_string_css : [< t ] -> string
end

module Input_security : sig
  type t =
    [ `Auto
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Inset_block_end : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Inset_block_start : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Inset_inline_end : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Inset_inline_start : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Interactivity : sig
  type t =
    [ `Auto
    | `Inert
    ]

  val to_string_css : [< t ] -> string
end

module Interpolate_size : sig
  type t =
    [ `Allow_keywords
    | `Numeric_only
    ]

  val to_string_css : [< t ] -> string
end

module Isolation : sig
  type t =
    [ `Auto
    | `Isolate
    ]

  val to_string_css : [< t ] -> string
end

module Item_cross : sig
  type t =
    [ `Auto
    | `Normal
    | `Nowrap
    | `Reverse
    | `Wrap
    | `Wrap_reverse
    ]

  val to_string_css : [< t ] -> string
end

module Item_direction : sig
  type t =
    [ `Auto
    | `Column
    | `Column_reverse
    | `Row
    | `Row_reverse
    ]

  val to_string_css : [< t ] -> string
end

module Item_pack : sig
  type t =
    [ `Balance
    | `Dense
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Item_slack : sig
  type t =
    [ Length_percentage_type.t
    | `Infinite
    ]

  val to_string_css : [< t ] -> string
end

module Item_track : sig
  type t =
    [ `Auto
    | `Column
    | `Column_reverse
    | `Row
    | `Row_reverse
    ]

  val to_string_css : [< t ] -> string
end

module Item_wrap : sig
  type t =
    [ `Auto
    | `Normal
    | `Nowrap
    | `Reverse
    | `Wrap
    | `Wrap_reverse
    ]

  val to_string_css : [< t ] -> string
end

module Item_flow : sig
  type t =
    [ Item_direction.t
    | Item_pack.t
    | Item_slack.t
    | Item_wrap.t
    ]

  val to_string_css : [< t ] -> string
end

module Justify_content : sig
  type t =
    [ Content_distribution_type.t
    | Content_position_type.t
    | Overflow_position_type.t
    | `Left
    | `Normal
    | `Right
    ]

  val to_string_css : [< t ] -> string
end

module Justify_items : sig
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

  val to_string_css : [< t ] -> string
end

module Justify_self : sig
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

  val to_string_css : [< t ] -> string
end

module Letter_spacing : sig
  type t =
    [ Length_percentage_type.t
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Lighting_color : sig
  type t = [ | Css_data_type.Color.t ]

  val to_string_css : [< t ] -> string
end

module Line_break : sig
  type t =
    [ `Anywhere
    | `Auto
    | `Loose
    | `Normal
    | `Strict
    ]

  val to_string_css : [< t ] -> string
end

module Line_clamp : sig
  type t =
    [ Css_data_type.Integer.t
    | `None
    | Block_ellipsis.t
    ]

  val to_string_css : [< t ] -> string
end

module Line_fit_edge : sig
  type t =
    [ Text_edge_type.t
    | `Leading
    ]

  val to_string_css : [< t ] -> string
end

module Line_grid : sig
  type t =
    [ `Create
    | `Match_parent
    ]

  val to_string_css : [< t ] -> string
end

module Line_height : sig
  type t =
    [ Length_percentage_type.t
    | Css_data_type.Number.t
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Font : sig
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

  val to_string_css : [< t ] -> string
end

module Line_height_step : sig
  type t = [ | Css_data_type.Length.t ]

  val to_string_css : [< t ] -> string
end

module Line_padding : sig
  type t = [ | Css_data_type.Length.t ]

  val to_string_css : [< t ] -> string
end

module Line_snap : sig
  type t =
    [ `Baseline
    | `Contain
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Link_parameters : sig
  type t =
    [ Css_data_type.Custom_property_name.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module List_style_position : sig
  type t =
    [ `Inside
    | `Outside
    ]

  val to_string_css : [< t ] -> string
end

module Margin_break : sig
  type t =
    [ `Auto
    | `Discard
    | `Keep
    ]

  val to_string_css : [< t ] -> string
end

module Margin_trim : sig
  type t =
    [ `Block
    | `Block_end
    | `Block_start
    | `Inline
    | `Inline_end
    | `Inline_start
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Marker_side : sig
  type t =
    [ `Match_parent
    | `Match_self
    ]

  val to_string_css : [< t ] -> string
end

module Mask_border_mode : sig
  type t =
    [ `Alpha_
    | `Luminance
    ]

  val to_string_css : [< t ] -> string
end

module Mask_border_outset : sig
  type t =
    [ Css_data_type.Length.t
    | Css_data_type.Number.t
    ]

  val to_string_css : [< t ] -> string
end

module Mask_border_repeat : sig
  type t =
    [ `Repeat
    | `Round
    | `Space
    | `Stretch
    ]

  val to_string_css : [< t ] -> string
end

module Mask_border_slice : sig
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `Fill
    ]

  val to_string_css : [< t ] -> string
end

module Mask_border_width : sig
  type t =
    [ Length_percentage_type.t
    | Css_data_type.Number.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Mask_clip : sig
  type t =
    [ Coord_box_type.t
    | `No_clip
    ]

  val to_string_css : [< t ] -> string
end

module Mask_composite : sig
  type t = [ | Compositing_operator_type.t ]

  val to_string_css : [< t ] -> string
end

module Mask_mode : sig
  type t = [ | Masking_mode_type.t ]

  val to_string_css : [< t ] -> string
end

module Mask_origin : sig
  type t = [ | Coord_box_type.t ]

  val to_string_css : [< t ] -> string
end

module Mask_position : sig
  type t = [ | Position_type.t ]

  val to_string_css : [< t ] -> string
end

module Mask_repeat : sig
  type t = [ | Repeat_style_type.t ]

  val to_string_css : [< t ] -> string
end

module Mask_size : sig
  type t = [ | Bg_size_type.t ]

  val to_string_css : [< t ] -> string
end

module Mask_type : sig
  type t =
    [ `Alpha_
    | `Luminance
    ]

  val to_string_css : [< t ] -> string
end

module Math_depth : sig
  type t =
    [ Css_data_type.Integer.t
    | `Auto_add
    ]

  val to_string_css : [< t ] -> string
end

module Math_shift : sig
  type t =
    [ `Compact
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Math_style : sig
  type t =
    [ `Compact
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Max_lines : sig
  type t =
    [ Css_data_type.Integer.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Min_intrinsic_sizing : sig
  type t =
    [ `Legacy
    | `Zero_if_extrinsic
    | `Zero_if_scroll
    ]

  val to_string_css : [< t ] -> string
end

module Mix_blend_mode : sig
  type t =
    [ Blend_mode_type.t
    | `Plus_darker
    | `Plus_lighter
    ]

  val to_string_css : [< t ] -> string
end

module Background_blend_mode : sig
  type t = [ | Mix_blend_mode.t ]

  val to_string_css : [< t ] -> string
end

module Nav_down : sig
  type t =
    [ `Auto
    | `Current
    | `Root
    ]

  val to_string_css : [< t ] -> string
end

module Nav_left : sig
  type t =
    [ `Auto
    | `Current
    | `Root
    ]

  val to_string_css : [< t ] -> string
end

module Nav_right : sig
  type t =
    [ `Auto
    | `Current
    | `Root
    ]

  val to_string_css : [< t ] -> string
end

module Nav_up : sig
  type t =
    [ `Auto
    | `Current
    | `Root
    ]

  val to_string_css : [< t ] -> string
end

module Object_fit : sig
  type t =
    [ `Contain
    | `Cover
    | `Fill
    | `None
    | `Scale_down
    ]

  val to_string_css : [< t ] -> string
end

module Object_position : sig
  type t = [ | Position_type.t ]

  val to_string_css : [< t ] -> string
end

module Offset_anchor : sig
  type t =
    [ Position_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Offset_distance : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Offset_position : sig
  type t =
    [ Position_type.t
    | `Auto
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Offset_rotate : sig
  type t =
    [ Css_data_type.Angle.t
    | `Auto
    | `Reverse
    ]

  val to_string_css : [< t ] -> string
end

module Opacity : sig
  module Opacity_value_type : sig
    type t =
      [ Css_data_type.Number.t
      | Css_data_type.Percentage.t
      ]

    val to_string_css : [< t ] -> string
  end

  type t = [ | Opacity_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Fill_opacity : sig
  type t = [ | Opacity.t ]

  val to_string_css : [< t ] -> string
end

module Flood_opacity : sig
  type t = [ | Opacity.t ]

  val to_string_css : [< t ] -> string
end

module Order : sig
  type t = [ | Css_data_type.Integer.t ]

  val to_string_css : [< t ] -> string
end

module Orphans : sig
  type t = [ | Css_data_type.Integer.t ]

  val to_string_css : [< t ] -> string
end

module Outline_offset : sig
  type t = [ | Css_data_type.Length.t ]

  val to_string_css : [< t ] -> string
end

module Outline_style : sig
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

  val to_string_css : [< t ] -> string
end

module Outline_width : sig
  type t = [ | Line_width_type.t ]

  val to_string_css : [< t ] -> string
end

module Overflow_anchor : sig
  type t =
    [ `Auto
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Overflow_block : sig
  type t =
    [ `Auto
    | `Clip
    | `Hidden
    | `Overlay
    | `Scroll
    | `Visible
    ]

  val to_string_css : [< t ] -> string
end

module Overflow : sig
  type t = [ | Overflow_block.t ]

  val to_string_css : [< t ] -> string
end

module Overflow_clip_margin : sig
  type t =
    [ Css_data_type.Length.t
    | Visual_box_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Overflow_clip_margin_block : sig
  type t =
    [ Css_data_type.Length.t
    | Visual_box_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Overflow_clip_margin_block_end : sig
  type t =
    [ Css_data_type.Length.t
    | Visual_box_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Overflow_clip_margin_block_start : sig
  type t =
    [ Css_data_type.Length.t
    | Visual_box_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Overflow_clip_margin_bottom : sig
  type t =
    [ Css_data_type.Length.t
    | Visual_box_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Overflow_clip_margin_inline : sig
  type t =
    [ Css_data_type.Length.t
    | Visual_box_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Overflow_clip_margin_inline_end : sig
  type t =
    [ Css_data_type.Length.t
    | Visual_box_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Overflow_clip_margin_inline_start : sig
  type t =
    [ Css_data_type.Length.t
    | Visual_box_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Overflow_clip_margin_left : sig
  type t =
    [ Css_data_type.Length.t
    | Visual_box_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Overflow_clip_margin_right : sig
  type t =
    [ Css_data_type.Length.t
    | Visual_box_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Overflow_clip_margin_top : sig
  type t =
    [ Css_data_type.Length.t
    | Visual_box_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Overflow_inline : sig
  type t =
    [ `Auto
    | `Clip
    | `Hidden
    | `Scroll
    | `Visible
    ]

  val to_string_css : [< t ] -> string
end

module Overflow_wrap : sig
  type t =
    [ `Anywhere
    | `Break_word
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Overflow_x : sig
  type t =
    [ `Auto
    | `Clip
    | `Hidden
    | `Scroll
    | `Visible
    ]

  val to_string_css : [< t ] -> string
end

module Overflow_y : sig
  type t =
    [ `Auto
    | `Clip
    | `Hidden
    | `Scroll
    | `Visible
    ]

  val to_string_css : [< t ] -> string
end

module Overlay : sig
  type t =
    [ `Auto
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Overscroll_behavior : sig
  type t =
    [ `Auto
    | `Contain
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Overscroll_behavior_block : sig
  type t =
    [ `Auto
    | `Contain
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Overscroll_behavior_inline : sig
  type t =
    [ `Auto
    | `Contain
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Overscroll_behavior_x : sig
  type t =
    [ `Auto
    | `Contain
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Overscroll_behavior_y : sig
  type t =
    [ `Auto
    | `Contain
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Padding_bottom : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Padding_left : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Padding_right : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Padding_top : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Padding : sig
  type t = [ | Padding_top.t ]

  val to_string_css : [< t ] -> string
end

module Padding_block : sig
  type t = [ | Padding_top.t ]

  val to_string_css : [< t ] -> string
end

module Padding_block_end : sig
  type t = [ | Padding_top.t ]

  val to_string_css : [< t ] -> string
end

module Padding_block_start : sig
  type t = [ | Padding_top.t ]

  val to_string_css : [< t ] -> string
end

module Padding_inline : sig
  type t = [ | Padding_top.t ]

  val to_string_css : [< t ] -> string
end

module Padding_inline_end : sig
  type t = [ | Padding_top.t ]

  val to_string_css : [< t ] -> string
end

module Padding_inline_start : sig
  type t = [ | Padding_top.t ]

  val to_string_css : [< t ] -> string
end

module Page : sig
  type t =
    [ Css_data_type.Ident.Custom.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Page_break_after : sig
  type t =
    [ `Always
    | `Auto
    | `Avoid
    | `Inherit
    | `Left
    | `Right
    ]

  val to_string_css : [< t ] -> string
end

module Page_break_before : sig
  type t =
    [ `Always
    | `Auto
    | `Avoid
    | `Inherit
    | `Left
    | `Right
    ]

  val to_string_css : [< t ] -> string
end

module Page_break_inside : sig
  type t =
    [ `Auto
    | `Avoid
    | `Inherit
    ]

  val to_string_css : [< t ] -> string
end

module Paint_order : sig
  type t =
    [ `Fill
    | `Markers
    | `Normal
    | `Stroke
    ]

  val to_string_css : [< t ] -> string
end

module Pause_after : sig
  type t =
    [ Css_data_type.Time.t
    | `Medium
    | `None
    | `Strong
    | `Weak
    | `X_strong
    | `X_weak
    ]

  val to_string_css : [< t ] -> string
end

module Pause_before : sig
  type t =
    [ Css_data_type.Time.t
    | `Medium
    | `None
    | `Strong
    | `Weak
    | `X_strong
    | `X_weak
    ]

  val to_string_css : [< t ] -> string
end

module Pause : sig
  type t =
    [ Pause_after.t
    | Pause_before.t
    ]

  val to_string_css : [< t ] -> string
end

module Perspective : sig
  type t =
    [ Css_data_type.Length.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Perspective_origin : sig
  type t = [ | Position_type.t ]

  val to_string_css : [< t ] -> string
end

module Place_content : sig
  type t =
    [ Align_content.t
    | Justify_content.t
    ]

  val to_string_css : [< t ] -> string
end

module Place_items : sig
  type t =
    [ Align_items.t
    | Justify_items.t
    ]

  val to_string_css : [< t ] -> string
end

module Place_self : sig
  type t =
    [ Align_self.t
    | Justify_self.t
    ]

  val to_string_css : [< t ] -> string
end

module Pointer_events : sig
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

  val to_string_css : [< t ] -> string
end

module Pointer_timeline_axis : sig
  type t =
    [ `Block
    | `Inline
    | `X
    | `Y
    ]

  val to_string_css : [< t ] -> string
end

module Pointer_timeline_name : sig
  type t =
    [ Css_data_type.Ident.Dashed.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Pointer_timeline : sig
  type t =
    [ Pointer_timeline_axis.t
    | Pointer_timeline_name.t
    ]

  val to_string_css : [< t ] -> string
end

module Position_anchor : sig
  type t =
    [ Anchor_name_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Position_area : sig
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

  val to_string_css : [< t ] -> string
end

module Position_try_fallbacks : sig
  module Try_tactic_type : sig
    type t =
      [ `Flip_block
      | `Flip_inline
      | `Flip_start
      ]

    val to_string_css : [< t ] -> string
  end

  type t =
    [ Css_data_type.Ident.Dashed.t
    | Try_tactic_type.t
    | `None
    | Position_area.t
    ]

  val to_string_css : [< t ] -> string
end

module Position_try_order : sig
  type t =
    [ `Most_block_size
    | `Most_height
    | `Most_inline_size
    | `Most_width
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Position_try : sig
  type t =
    [ Position_try_fallbacks.t
    | Position_try_order.t
    ]

  val to_string_css : [< t ] -> string
end

module Position_visibility : sig
  type t =
    [ `Always
    | `Anchors_valid
    | `Anchors_visible
    | `No_overflow
    ]

  val to_string_css : [< t ] -> string
end

module Print_color_adjust : sig
  type t =
    [ `Economy
    | `Exact
    ]

  val to_string_css : [< t ] -> string
end

module Color_adjust : sig
  type t = [ | Print_color_adjust.t ]

  val to_string_css : [< t ] -> string
end

module Quotes : sig
  type t =
    [ Css_data_type.Css_string.t
    | `Auto
    | `Match_parent
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module R : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Reading_flow : sig
  type t =
    [ `Flex_flow
    | `Flex_visual
    | `Grid_columns
    | `Grid_order
    | `Grid_rows
    | `Normal
    | `Source_order
    ]

  val to_string_css : [< t ] -> string
end

module Reading_order : sig
  type t = [ | Css_data_type.Integer.t ]

  val to_string_css : [< t ] -> string
end

module Region_fragment : sig
  type t =
    [ `Auto
    | `Break
    ]

  val to_string_css : [< t ] -> string
end

module Resize : sig
  type t =
    [ `Block
    | `Both
    | `Horizontal
    | `Inline
    | `None
    | `Vertical
    ]

  val to_string_css : [< t ] -> string
end

module Rest_after : sig
  type t =
    [ Css_data_type.Time.t
    | `Medium
    | `None
    | `Strong
    | `Weak
    | `X_strong
    | `X_weak
    ]

  val to_string_css : [< t ] -> string
end

module Rest_before : sig
  type t =
    [ Css_data_type.Time.t
    | `Medium
    | `None
    | `Strong
    | `Weak
    | `X_strong
    | `X_weak
    ]

  val to_string_css : [< t ] -> string
end

module Rest : sig
  type t =
    [ Rest_after.t
    | Rest_before.t
    ]

  val to_string_css : [< t ] -> string
end

module Rotate : sig
  type t =
    [ Css_data_type.Angle.t
    | Css_data_type.Number.t
    | `None
    | `X
    | `Y
    | `Z
    ]

  val to_string_css : [< t ] -> string
end

module Row_gap : sig
  type t =
    [ Length_percentage_type.t
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Gap : sig
  type t =
    [ Column_gap.t
    | Row_gap.t
    ]

  val to_string_css : [< t ] -> string
end

module Grid_gap : sig
  type t =
    [ Column_gap.t
    | Row_gap.t
    ]

  val to_string_css : [< t ] -> string
end

module Row_rule : sig
  type t =
    [ Gap_auto_rule_list_type.t
    | Gap_rule_list_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Row_rule_break : sig
  type t =
    [ `Intersection
    | `None
    | `Spanning_item
    ]

  val to_string_css : [< t ] -> string
end

module Row_rule_color : sig
  type t =
    [ Auto_line_color_list_type.t
    | Line_color_list_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Row_rule_outset : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Row_rule_style : sig
  type t =
    [ Auto_line_style_list_type.t
    | Line_style_list_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Row_rule_width : sig
  type t =
    [ Auto_line_width_list_type.t
    | Line_width_list_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Ruby_align : sig
  type t =
    [ `Center
    | `Space_around
    | `Space_between
    | `Start
    ]

  val to_string_css : [< t ] -> string
end

module Ruby_merge : sig
  type t =
    [ `Auto
    | `Merge
    | `Separate
    ]

  val to_string_css : [< t ] -> string
end

module Ruby_overhang : sig
  type t =
    [ `Auto
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Ruby_position : sig
  type t =
    [ `Alternate
    | `Inter_character
    | `Over
    | `Under
    ]

  val to_string_css : [< t ] -> string
end

module Rule : sig
  type t = [ | Column_rule.t ]

  val to_string_css : [< t ] -> string
end

module Rule_break : sig
  type t = [ | Column_rule_break.t ]

  val to_string_css : [< t ] -> string
end

module Rule_color : sig
  type t = [ | Column_rule_color.t ]

  val to_string_css : [< t ] -> string
end

module Rule_outset : sig
  type t = [ | Column_rule_outset.t ]

  val to_string_css : [< t ] -> string
end

module Rule_paint_order : sig
  type t =
    [ `Column_over_row
    | `Row_over_column
    ]

  val to_string_css : [< t ] -> string
end

module Rule_style : sig
  type t = [ | Column_rule_style.t ]

  val to_string_css : [< t ] -> string
end

module Rule_width : sig
  type t = [ | Column_rule_width.t ]

  val to_string_css : [< t ] -> string
end

module Rx : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Ry : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Scale : sig
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Scroll_behavior : sig
  type t =
    [ `Auto
    | `Smooth
    ]

  val to_string_css : [< t ] -> string
end

module Scroll_initial_target : sig
  type t =
    [ `Nearest
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Scroll_margin : sig
  type t = [ | Css_data_type.Length.t ]

  val to_string_css : [< t ] -> string
end

module Scroll_margin_block : sig
  type t = [ | Css_data_type.Length.t ]

  val to_string_css : [< t ] -> string
end

module Scroll_margin_block_end : sig
  type t = [ | Css_data_type.Length.t ]

  val to_string_css : [< t ] -> string
end

module Scroll_margin_block_start : sig
  type t = [ | Css_data_type.Length.t ]

  val to_string_css : [< t ] -> string
end

module Scroll_margin_bottom : sig
  type t = [ | Css_data_type.Length.t ]

  val to_string_css : [< t ] -> string
end

module Scroll_margin_inline : sig
  type t = [ | Css_data_type.Length.t ]

  val to_string_css : [< t ] -> string
end

module Scroll_margin_inline_end : sig
  type t = [ | Css_data_type.Length.t ]

  val to_string_css : [< t ] -> string
end

module Scroll_margin_inline_start : sig
  type t = [ | Css_data_type.Length.t ]

  val to_string_css : [< t ] -> string
end

module Scroll_margin_left : sig
  type t = [ | Css_data_type.Length.t ]

  val to_string_css : [< t ] -> string
end

module Scroll_margin_right : sig
  type t = [ | Css_data_type.Length.t ]

  val to_string_css : [< t ] -> string
end

module Scroll_margin_top : sig
  type t = [ | Css_data_type.Length.t ]

  val to_string_css : [< t ] -> string
end

module Scroll_marker_group : sig
  type t =
    [ `After
    | `Before
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Scroll_padding : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Scroll_padding_block : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Scroll_padding_block_end : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Scroll_padding_block_start : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Scroll_padding_bottom : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Scroll_padding_inline : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Scroll_padding_inline_end : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Scroll_padding_inline_start : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Scroll_padding_left : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Scroll_padding_right : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Scroll_padding_top : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Scroll_snap_align : sig
  type t =
    [ `Center
    | `End
    | `None
    | `Start
    ]

  val to_string_css : [< t ] -> string
end

module Scroll_snap_stop : sig
  type t =
    [ `Always
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Scroll_snap_type : sig
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

  val to_string_css : [< t ] -> string
end

module Scroll_target_group : sig
  type t =
    [ `Auto
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Scroll_timeline_axis : sig
  type t =
    [ `Block
    | `Inline
    | `X
    | `Y
    ]

  val to_string_css : [< t ] -> string
end

module Scroll_timeline_name : sig
  type t =
    [ Css_data_type.Ident.Dashed.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Scroll_timeline : sig
  type t =
    [ Scroll_timeline_axis.t
    | Scroll_timeline_name.t
    ]

  val to_string_css : [< t ] -> string
end

module Scrollbar_color : sig
  type t =
    [ Css_data_type.Color.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Scrollbar_gutter : sig
  type t =
    [ `Auto
    | `Both_edges
    | `Stable
    ]

  val to_string_css : [< t ] -> string
end

module Scrollbar_width : sig
  type t =
    [ `Auto
    | `None
    | `Thin
    ]

  val to_string_css : [< t ] -> string
end

module Shape_image_threshold : sig
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    ]

  val to_string_css : [< t ] -> string
end

module Shape_margin : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Shape_padding : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Shape_rendering : sig
  type t =
    [ `Auto
    | `CrispEdges
    | `GeometricPrecision
    | `OptimizeSpeed
    ]

  val to_string_css : [< t ] -> string
end

module Slider_orientation : sig
  type t =
    [ `Auto
    | `Bottom_to_top
    | `Left_to_right
    | `Right_to_left
    | `Top_to_bottom
    ]

  val to_string_css : [< t ] -> string
end

module Spatial_navigation_action : sig
  type t =
    [ `Auto
    | `Focus
    | `Scroll
    ]

  val to_string_css : [< t ] -> string
end

module Spatial_navigation_contain : sig
  type t =
    [ `Auto
    | `Contain
    ]

  val to_string_css : [< t ] -> string
end

module Spatial_navigation_function : sig
  type t =
    [ `Grid
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Speak : sig
  type t =
    [ `Always
    | `Auto
    | `Never
    ]

  val to_string_css : [< t ] -> string
end

module Speak_as : sig
  type t =
    [ `Digits
    | `Literal_punctuation
    | `No_punctuation
    | `Normal
    | `Spell_out
    ]

  val to_string_css : [< t ] -> string
end

module String_set : sig
  type t =
    [ Css_data_type.Ident.Custom.t
    | Css_data_type.Css_string.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Stroke_align : sig
  type t =
    [ `Center
    | `Inset
    | `Outset
    ]

  val to_string_css : [< t ] -> string
end

module Stroke_alignment : sig
  type t =
    [ `Center
    | `Inner
    | `Outer
    ]

  val to_string_css : [< t ] -> string
end

module Stroke_break : sig
  type t =
    [ `Bounding_box
    | `Clone
    | `Slice
    ]

  val to_string_css : [< t ] -> string
end

module Stroke_color : sig
  type t = [ | Css_data_type.Color.t ]

  val to_string_css : [< t ] -> string
end

module Stroke_dash_corner : sig
  type t =
    [ Css_data_type.Length.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Stroke_dash_justify : sig
  type t =
    [ `Compress
    | `Dashes
    | `Gaps
    | `None
    | `Stretch
    ]

  val to_string_css : [< t ] -> string
end

module Stroke_dashadjust : sig
  type t =
    [ `Compress
    | `Dashes
    | `Gaps
    | `None
    | `Stretch
    ]

  val to_string_css : [< t ] -> string
end

module Stroke_dasharray : sig
  type t =
    [ Length_percentage_type.t
    | Css_data_type.Number.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Stroke_dashcorner : sig
  type t =
    [ Css_data_type.Length.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Stroke_dashoffset : sig
  type t =
    [ Length_percentage_type.t
    | Css_data_type.Number.t
    ]

  val to_string_css : [< t ] -> string
end

module Stroke_linecap : sig
  type t =
    [ `Butt
    | `Round
    | `Square
    ]

  val to_string_css : [< t ] -> string
end

module Stroke_linejoin : sig
  type t =
    [ `Arcs
    | `Bevel
    | `Crop
    | `Fallback
    | `Miter
    | `Round
    ]

  val to_string_css : [< t ] -> string
end

module Stroke_miterlimit : sig
  type t = [ | Css_data_type.Number.t ]

  val to_string_css : [< t ] -> string
end

module Stroke_opacity : sig
  type t = [ | Opacity.t ]

  val to_string_css : [< t ] -> string
end

module Stroke_origin : sig
  type t =
    [ `Border_box
    | `Content_box
    | `Fill_box
    | `Match_parent
    | `Padding_box
    | `Stroke_box
    ]

  val to_string_css : [< t ] -> string
end

module Stroke_position : sig
  type t = [ | Position_type.t ]

  val to_string_css : [< t ] -> string
end

module Stroke_repeat : sig
  type t = [ | Repeat_style_type.t ]

  val to_string_css : [< t ] -> string
end

module Stroke_size : sig
  type t = [ | Bg_size_type.t ]

  val to_string_css : [< t ] -> string
end

module Stroke_width : sig
  type t =
    [ Length_percentage_type.t
    | Css_data_type.Number.t
    ]

  val to_string_css : [< t ] -> string
end

module Tab_size : sig
  type t =
    [ Css_data_type.Length.t
    | Css_data_type.Number.t
    ]

  val to_string_css : [< t ] -> string
end

module Table_layout : sig
  type t =
    [ `Auto
    | `Fixed
    ]

  val to_string_css : [< t ] -> string
end

module Text_align : sig
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

  val to_string_css : [< t ] -> string
end

module Text_align_all : sig
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

  val to_string_css : [< t ] -> string
end

module Text_align_last : sig
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

  val to_string_css : [< t ] -> string
end

module Text_anchor : sig
  type t =
    [ `End
    | `Middle
    | `Start
    ]

  val to_string_css : [< t ] -> string
end

module Text_autospace : sig
  type t =
    [ Autospace_type.t
    | `Auto
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Text_box_edge : sig
  type t =
    [ Text_edge_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Text_box_trim : sig
  type t =
    [ `None
    | `Trim_both
    | `Trim_end
    | `Trim_start
    ]

  val to_string_css : [< t ] -> string
end

module Text_box : sig
  type t =
    [ `Normal
    | Text_box_edge.t
    | Text_box_trim.t
    ]

  val to_string_css : [< t ] -> string
end

module Text_combine_upright : sig
  type t =
    [ Css_data_type.Integer.t
    | `All
    | `Digits
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Text_decoration_color : sig
  type t = [ | Css_data_type.Color.t ]

  val to_string_css : [< t ] -> string
end

module Text_decoration_line : sig
  type t =
    [ `Blink
    | `Grammar_error
    | `Line_through
    | `None
    | `Overline
    | `Spelling_error
    | `Underline
    ]

  val to_string_css : [< t ] -> string
end

module Text_decoration_skip : sig
  type t =
    [ `Auto
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Text_decoration_skip_box : sig
  type t =
    [ `All
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Text_decoration_skip_ink : sig
  type t =
    [ `All
    | `Auto
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Text_decoration_skip_self : sig
  type t =
    [ `Auto
    | `No_skip
    | `Skip_all
    | `Skip_line_through
    | `Skip_overline
    | `Skip_underline
    ]

  val to_string_css : [< t ] -> string
end

module Text_decoration_skip_spaces : sig
  type t =
    [ `All
    | `End
    | `None
    | `Start
    ]

  val to_string_css : [< t ] -> string
end

module Text_decoration_style : sig
  type t =
    [ `Dashed
    | `Dotted
    | `Double
    | `Solid
    | `Wavy
    ]

  val to_string_css : [< t ] -> string
end

module Text_decoration_thickness : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    | `From_font
    ]

  val to_string_css : [< t ] -> string
end

module Text_decoration : sig
  type t =
    [ Text_decoration_color.t
    | Text_decoration_line.t
    | Text_decoration_style.t
    | Text_decoration_thickness.t
    ]

  val to_string_css : [< t ] -> string
end

module Text_decoration_trim : sig
  type t =
    [ Css_data_type.Length.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Text_emphasis_color : sig
  type t = [ | Css_data_type.Color.t ]

  val to_string_css : [< t ] -> string
end

module Text_emphasis_position : sig
  type t =
    [ `Left
    | `Over
    | `Right
    | `Under
    ]

  val to_string_css : [< t ] -> string
end

module Text_emphasis_skip : sig
  type t =
    [ `Narrow
    | `Punctuation
    | `Spaces
    | `Symbols
    ]

  val to_string_css : [< t ] -> string
end

module Text_emphasis_style : sig
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

  val to_string_css : [< t ] -> string
end

module Text_emphasis : sig
  type t =
    [ Text_emphasis_color.t
    | Text_emphasis_style.t
    ]

  val to_string_css : [< t ] -> string
end

module Text_group_align : sig
  type t =
    [ `Center
    | `End
    | `Left
    | `None
    | `Right
    | `Start
    ]

  val to_string_css : [< t ] -> string
end

module Text_indent : sig
  type t =
    [ Length_percentage_type.t
    | `Each_line
    | `Hanging
    ]

  val to_string_css : [< t ] -> string
end

module Text_justify : sig
  type t =
    [ `Auto
    | `Inter_character
    | `Inter_word
    | `No_compress
    | `None
    | `Ruby
    ]

  val to_string_css : [< t ] -> string
end

module Text_orientation : sig
  type t =
    [ `Mixed
    | `Sideways
    | `Upright
    ]

  val to_string_css : [< t ] -> string
end

module Text_rendering : sig
  type t =
    [ `Auto
    | `GeometricPrecision
    | `OptimizeLegibility
    | `OptimizeSpeed
    ]

  val to_string_css : [< t ] -> string
end

module Text_shadow : sig
  type t =
    [ Css_data_type.Color.t
    | Css_data_type.Length.t
    | `Inset
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Text_size_adjust : sig
  type t =
    [ Css_data_type.Percentage.t
    | `Auto
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Text_spacing : sig
  type t =
    [ Autospace_type.t
    | Spacing_trim_type.t
    | `Auto
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Text_spacing_trim : sig
  type t =
    [ Spacing_trim_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Text_transform : sig
  type t =
    [ `Capitalize
    | `Full_size_kana
    | `Full_width
    | `Lowercase
    | `Math_auto
    | `None
    | `Uppercase
    ]

  val to_string_css : [< t ] -> string
end

module Text_underline_offset : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Text_underline_position : sig
  type t =
    [ `Auto
    | `From_font
    | `Left
    | `Right
    | `Under
    ]

  val to_string_css : [< t ] -> string
end

module Text_wrap_mode : sig
  type t =
    [ `Nowrap
    | `Wrap
    ]

  val to_string_css : [< t ] -> string
end

module Text_wrap_style : sig
  type t =
    [ `Auto
    | `Avoid_orphans
    | `Balance
    | `Pretty
    | `Stable
    ]

  val to_string_css : [< t ] -> string
end

module Text_wrap : sig
  type t =
    [ Text_wrap_mode.t
    | Text_wrap_style.t
    ]

  val to_string_css : [< t ] -> string
end

module Timeline_scope : sig
  type t =
    [ Css_data_type.Ident.Dashed.t
    | `All
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Touch_action : sig
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

  val to_string_css : [< t ] -> string
end

module Transform_box : sig
  type t =
    [ `Border_box
    | `Content_box
    | `Fill_box
    | `Stroke_box
    | `View_box
    ]

  val to_string_css : [< t ] -> string
end

module Transform_origin : sig
  type t =
    [ Css_data_type.Length.t
    | Length_percentage_type.t
    | `Bottom
    | `Center
    | `Left
    | `Right
    | `Top
    ]

  val to_string_css : [< t ] -> string
end

module Transform_style : sig
  type t =
    [ `Flat
    | `Preserve_3d
    ]

  val to_string_css : [< t ] -> string
end

module Transition_behavior : sig
  type t = [ | Transition_behavior_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Transition_delay : sig
  type t = [ | Css_data_type.Time.t ]

  val to_string_css : [< t ] -> string
end

module Transition_duration : sig
  type t = [ | Css_data_type.Time.t ]

  val to_string_css : [< t ] -> string
end

module Transition_property : sig
  type t =
    [ Single_transition_property_type.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Translate : sig
  type t =
    [ Css_data_type.Length.t
    | Length_percentage_type.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Unicode_bidi : sig
  type t =
    [ `Bidi_override
    | `Embed
    | `Isolate
    | `Isolate_override
    | `Normal
    | `Plaintext
    ]

  val to_string_css : [< t ] -> string
end

module User_select : sig
  type t =
    [ `All
    | `Auto
    | `Contain
    | `None
    | `Text
    ]

  val to_string_css : [< t ] -> string
end

module Vector_effect : sig
  type t =
    [ `Fixed_position
    | `Non_rotation
    | `Non_scaling_size
    | `Non_scaling_stroke
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Vertical_align : sig
  type t =
    [ `First
    | `Last
    | Alignment_baseline.t
    | Baseline_shift.t
    ]

  val to_string_css : [< t ] -> string
end

module View_timeline_axis : sig
  type t =
    [ `Block
    | `Inline
    | `X
    | `Y
    ]

  val to_string_css : [< t ] -> string
end

module View_timeline_inset : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module View_timeline_name : sig
  type t =
    [ Css_data_type.Ident.Dashed.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module View_timeline : sig
  type t =
    [ View_timeline_axis.t
    | View_timeline_inset.t
    | View_timeline_name.t
    ]

  val to_string_css : [< t ] -> string
end

module View_transition_class : sig
  type t =
    [ Css_data_type.Ident.Custom.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module View_transition_group : sig
  type t =
    [ Css_data_type.Ident.Custom.t
    | `Contain
    | `Nearest
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module View_transition_name : sig
  type t =
    [ Css_data_type.Ident.Custom.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Visibility : sig
  type t =
    [ `Collapse
    | `Force_hidden
    | `Hidden
    | `Visible
    ]

  val to_string_css : [< t ] -> string
end

module Voice_balance : sig
  type t =
    [ Css_data_type.Number.t
    | `Center
    | `Left
    | `Leftwards
    | `Right
    | `Rightwards
    ]

  val to_string_css : [< t ] -> string
end

module Voice_duration : sig
  type t =
    [ Css_data_type.Time.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Voice_family : sig
  type t =
    [ Family_name_type.t
    | Css_data_type.Integer.t
    | `Preserve
    ]

  val to_string_css : [< t ] -> string
end

module Voice_pitch : sig
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

  val to_string_css : [< t ] -> string
end

module Voice_range : sig
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

  val to_string_css : [< t ] -> string
end

module Voice_rate : sig
  type t =
    [ Css_data_type.Percentage.t
    | `Fast
    | `Medium
    | `Normal
    | `Slow
    | `X_fast
    | `X_slow
    ]

  val to_string_css : [< t ] -> string
end

module Voice_stress : sig
  type t =
    [ `Moderate
    | `None
    | `Normal
    | `Reduced
    | `Strong
    ]

  val to_string_css : [< t ] -> string
end

module Voice_volume : sig
  type t =
    [ Css_data_type.Decibel.t
    | `Loud
    | `Medium
    | `Silent
    | `Soft
    | `X_loud
    | `X_soft
    ]

  val to_string_css : [< t ] -> string
end

module White_space_collapse : sig
  type t =
    [ `Break_spaces
    | `Collapse
    | `Discard
    | `Preserve
    | `Preserve_breaks
    | `Preserve_spaces
    ]

  val to_string_css : [< t ] -> string
end

module White_space_trim : sig
  type t =
    [ `Discard_after
    | `Discard_before
    | `Discard_inner
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module White_space : sig
  type t =
    [ `Normal
    | `Pre
    | `Pre_line
    | `Pre_wrap
    | Text_wrap_mode.t
    | White_space_collapse.t
    | White_space_trim.t
    ]

  val to_string_css : [< t ] -> string
end

module Widows : sig
  type t = [ | Css_data_type.Integer.t ]

  val to_string_css : [< t ] -> string
end

module Will_change : sig
  type t =
    [ Css_data_type.Ident.Custom.t
    | `Auto
    | `Contents
    | `Scroll_position
    ]

  val to_string_css : [< t ] -> string
end

module Word_break : sig
  type t =
    [ `Auto_phrase
    | `Break_all
    | `Break_word
    | `Keep_all
    | `Manual
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Word_space_transform : sig
  type t =
    [ `Auto_phrase
    | `Ideographic_space
    | `None
    | `Space
    ]

  val to_string_css : [< t ] -> string
end

module Word_spacing : sig
  type t =
    [ Length_percentage_type.t
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Word_wrap : sig
  type t =
    [ `Anywhere
    | `Break_word
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Wrap_after : sig
  type t =
    [ `Auto
    | `Avoid
    | `Avoid_flex
    | `Avoid_line
    | `Flex
    | `Line
    ]

  val to_string_css : [< t ] -> string
end

module Wrap_before : sig
  type t =
    [ `Auto
    | `Avoid
    | `Avoid_flex
    | `Avoid_line
    | `Flex
    | `Line
    ]

  val to_string_css : [< t ] -> string
end

module Wrap_flow : sig
  type t =
    [ `Auto
    | `Both
    | `Clear
    | `End
    | `Maximum
    | `Minimum
    | `Start
    ]

  val to_string_css : [< t ] -> string
end

module Wrap_inside : sig
  type t =
    [ `Auto
    | `Avoid
    ]

  val to_string_css : [< t ] -> string
end

module Wrap_through : sig
  type t =
    [ `None
    | `Wrap
    ]

  val to_string_css : [< t ] -> string
end

module Writing_mode : sig
  type t =
    [ `Horizontal_tb
    | `Sideways_lr
    | `Sideways_rl
    | `Vertical_lr
    | `Vertical_rl
    ]

  val to_string_css : [< t ] -> string
end

module X : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Y : sig
  type t = [ | Length_percentage_type.t ]

  val to_string_css : [< t ] -> string
end

module Z_index : sig
  type t =
    [ Css_data_type.Integer.t
    | `Auto
    | `Inherit
    ]

  val to_string_css : [< t ] -> string
end

module Zoom : sig
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    ]

  val to_string_css : [< t ] -> string
end

module Abs_fn : sig
  type t = [ | Calc_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Acos_fn : sig
  type t = [ | Calc_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Anchor_fn : sig
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

  val to_string_css : [< t ] -> string
end

module Anchor_size_fn : sig
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

  val to_string_css : [< t ] -> string
end

module Bottom : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Left : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Margin_bottom : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Margin_left : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Margin_right : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Margin_top : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Margin : sig
  type t = [ | Margin_top.t ]

  val to_string_css : [< t ] -> string
end

module Margin_block : sig
  type t = [ | Margin_top.t ]

  val to_string_css : [< t ] -> string
end

module Margin_block_end : sig
  type t = [ | Margin_top.t ]

  val to_string_css : [< t ] -> string
end

module Margin_block_start : sig
  type t = [ | Margin_top.t ]

  val to_string_css : [< t ] -> string
end

module Margin_inline : sig
  type t = [ | Margin_top.t ]

  val to_string_css : [< t ] -> string
end

module Margin_inline_end : sig
  type t = [ | Margin_top.t ]

  val to_string_css : [< t ] -> string
end

module Margin_inline_start : sig
  type t = [ | Margin_top.t ]

  val to_string_css : [< t ] -> string
end

module Right : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Top : sig
  type t =
    [ Length_percentage_type.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Inset : sig
  type t = [ | Top.t ]

  val to_string_css : [< t ] -> string
end

module Inset_block : sig
  type t = [ | Top.t ]

  val to_string_css : [< t ] -> string
end

module Inset_inline : sig
  type t = [ | Top.t ]

  val to_string_css : [< t ] -> string
end

module Asin_fn : sig
  type t = [ | Calc_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Atan_fn : sig
  type t = [ | Calc_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Atan2_fn : sig
  type t = [ | Calc_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Calc_fn : sig
  type t = [ | Calc_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Calc_mix_fn : sig
  type t =
    [ Calc_value_type.t
    | Css_data_type.Percentage.t
    ]

  val to_string_css : [< t ] -> string
end

module Calc_size_fn : sig
  type t =
    [ Calc_value_type.t
    | `Any
    ]

  val to_string_css : [< t ] -> string
end

module Calc_size_basis_type : sig
  type t =
    [ Calc_value_type.t
    | `Any
    ]

  val to_string_css : [< t ] -> string
end

module Clamp_fn : sig
  type t =
    [ Calc_value_type.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Color_fn : sig
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

  val to_string_css : [< t ] -> string
end

module Color_layers_fn : sig
  type t =
    [ Blend_mode_type.t
    | Css_data_type.Color.t
    ]

  val to_string_css : [< t ] -> string
end

module Color_mix_fn : sig
  type t =
    [ Css_data_type.Color.t
    | Color_interpolation_method_type.t
    | Css_data_type.Percentage.t
    ]

  val to_string_css : [< t ] -> string
end

module Conic_gradient_fn : sig
  type t = [ | Conic_gradient_syntax_type.t ]

  val to_string_css : [< t ] -> string
end

module Content_fn : sig
  type t =
    [ `After
    | `Before
    | `First_letter
    | `Marker
    | `Text
    ]

  val to_string_css : [< t ] -> string
end

module Contrast_color_fn : sig
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

  val to_string_css : [< t ] -> string
end

module Control_value_fn : sig
  type t =
    [ `Number_
    | `String_
    ]

  val to_string_css : [< t ] -> string
end

module Cos_fn : sig
  type t = [ | Calc_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Cubic_bezier_fn : sig
  type t = [ | Css_data_type.Number.t ]

  val to_string_css : [< t ] -> string
end

module Device_cmyk_fn : sig
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Dynamic_range_limit_mix_fn : sig
  type t =
    [ Css_data_type.Percentage.t
    | `Constrained
    | `No_limit
    | `Standard
    ]

  val to_string_css : [< t ] -> string
end

module Dynamic_range_limit : sig
  type t =
    [ Css_data_type.Percentage.t
    | `Constrained
    | `No_limit
    | `Standard
    ]

  val to_string_css : [< t ] -> string
end

module Env_fn : sig
  type t =
    [ Css_data_type.Ident.Custom.t
    | Css_data_type.Integer.t
    ]

  val to_string_css : [< t ] -> string
end

module Exp_fn : sig
  type t = [ | Calc_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Text_overflow : sig
  module Fade_fn : sig
    type t = [ | Length_percentage_type.t ]

    val to_string_css : [< t ] -> string
  end

  type t =
    [ Css_data_type.Css_string.t
    | `Clip
    | `Ellipsis
    | `Fade
    ]

  val to_string_css : [< t ] -> string
end

module Height : sig
  module Fit_content_fn : sig
    type t = [ | Length_percentage_type.t ]

    val to_string_css : [< t ] -> string
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

  val to_string_css : [< t ] -> string
end

module Max_height : sig
  module Fit_content_fn : sig
    type t = [ | Length_percentage_type.t ]

    val to_string_css : [< t ] -> string
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

  val to_string_css : [< t ] -> string
end

module Max_width : sig
  module Fit_content_fn : sig
    type t = [ | Length_percentage_type.t ]

    val to_string_css : [< t ] -> string
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

  val to_string_css : [< t ] -> string
end

module Max_block_size : sig
  type t = [ | Max_width.t ]

  val to_string_css : [< t ] -> string
end

module Max_inline_size : sig
  type t = [ | Max_width.t ]

  val to_string_css : [< t ] -> string
end

module Min_height : sig
  module Fit_content_fn : sig
    type t = [ | Length_percentage_type.t ]

    val to_string_css : [< t ] -> string
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

  val to_string_css : [< t ] -> string
end

module Min_width : sig
  module Fit_content_fn : sig
    type t = [ | Length_percentage_type.t ]

    val to_string_css : [< t ] -> string
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

  val to_string_css : [< t ] -> string
end

module Min_block_size : sig
  type t = [ | Min_width.t ]

  val to_string_css : [< t ] -> string
end

module Min_inline_size : sig
  type t = [ | Min_width.t ]

  val to_string_css : [< t ] -> string
end

module Width : sig
  module Fit_content_fn : sig
    type t = [ | Length_percentage_type.t ]

    val to_string_css : [< t ] -> string
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

  val to_string_css : [< t ] -> string
end

module Block_size : sig
  type t = [ | Width.t ]

  val to_string_css : [< t ] -> string
end

module Flex_basis : sig
  type t =
    [ `Content
    | Width.t
    ]

  val to_string_css : [< t ] -> string
end

module Flex : sig
  type t =
    [ `None
    | Flex_basis.t
    | Flex_grow.t
    | Flex_shrink.t
    ]

  val to_string_css : [< t ] -> string
end

module Inline_size : sig
  type t = [ | Width.t ]

  val to_string_css : [< t ] -> string
end

module Hdr_color_fn : sig
  type t =
    [ Css_data_type.Color.t
    | Css_data_type.Number.t
    ]

  val to_string_css : [< t ] -> string
end

module Hsl_fn : sig
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Color.Hue.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `From
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Hsla_fn : sig
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Color.Hue.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `From
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Hwb_fn : sig
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Color.Hue.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `From
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Hypot_fn : sig
  type t = [ | Calc_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Ictcp_fn : sig
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `From
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Ident_fn : sig
  type t =
    [ Css_data_type.Ident.t
    | Css_data_type.Integer.t
    | Css_data_type.Css_string.t
    ]

  val to_string_css : [< t ] -> string
end

module If_fn : sig
  type t = [ `Else ]

  val to_string_css : [< t ] -> string
end

module Inherit_fn : sig
  type t = [ | Css_data_type.Custom_property_name.t ]

  val to_string_css : [< t ] -> string
end

module Jzazbz_fn : sig
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `From
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Jzczhz_fn : sig
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Color.Hue.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `From
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Lab_fn : sig
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `From
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Lch_fn : sig
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Color.Hue.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `From
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Leader_fn : sig
  type t =
    [ Css_data_type.Css_string.t
    | `Dotted
    | `Solid
    | `Space
    ]

  val to_string_css : [< t ] -> string
end

module Light_dark_fn : sig
  type t = [ | Css_data_type.Color.t ]

  val to_string_css : [< t ] -> string
end

module Linear_fn : sig
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    ]

  val to_string_css : [< t ] -> string
end

module Linear_gradient_fn : sig
  type t = [ | Linear_gradient_syntax_type.t ]

  val to_string_css : [< t ] -> string
end

module Log_fn : sig
  type t = [ | Calc_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Matrix3d_fn : sig
  type t = [ | Css_data_type.Number.t ]

  val to_string_css : [< t ] -> string
end

module Max_fn : sig
  type t = [ | Calc_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Media_fn : sig
  type t =
    [ Css_data_type.Ident.t
    | Css_data_type.Number.t
    | Ratio_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Min_fn : sig
  type t = [ | Calc_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Grid_template_columns : sig
  module Fit_content_fn : sig
    type t = [ | Length_percentage_type.t ]

    val to_string_css : [< t ] -> string
  end

  module Minmax_fn : sig
    type t =
      [ `Max
      | `Min
      ]

    val to_string_css : [< t ] -> string
  end

  type t =
    [ Auto_track_list_type.t
    | Line_name_list_type.t
    | Track_list_type.t
    | `None
    | `Subgrid
    ]

  val to_string_css : [< t ] -> string
end

module Grid_template_rows : sig
  module Fit_content_fn : sig
    type t = [ | Length_percentage_type.t ]

    val to_string_css : [< t ] -> string
  end

  module Minmax_fn : sig
    type t =
      [ `Max
      | `Min
      ]

    val to_string_css : [< t ] -> string
  end

  type t =
    [ Auto_track_list_type.t
    | Line_name_list_type.t
    | Track_list_type.t
    | `None
    | `Subgrid
    ]

  val to_string_css : [< t ] -> string
end

module Grid_template : sig
  type t =
    [ Line_names_type.t
    | Css_data_type.Css_string.t
    | Track_size_type.t
    | `None
    | Grid_template_columns.t
    | Grid_template_rows.t
    ]

  val to_string_css : [< t ] -> string
end

module Grid : sig
  type t =
    [ `Auto_flow
    | `Dense
    | Grid_auto_columns.t
    | Grid_auto_rows.t
    | Grid_template.t
    | Grid_template_columns.t
    | Grid_template_rows.t
    ]

  val to_string_css : [< t ] -> string
end

module Mod_fn : sig
  type t = [ | Calc_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Oklab_fn : sig
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `From
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Oklch_fn : sig
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Color.Hue.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `From
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Paint_fn : sig
  type t = [ | Css_data_type.Ident.t ]

  val to_string_css : [< t ] -> string
end

module Palette_mix_fn : sig
  type t =
    [ Color_interpolation_method_type.t
    | Css_data_type.Percentage.t
    | `Dark
    | `Light
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Font_palette : sig
  type t =
    [ `Dark
    | `Light
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Perspective_fn : sig
  type t =
    [ Css_data_type.Length.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Pointer_fn : sig
  type t =
    [ `Block
    | `Inline
    | `Nearest
    | `Root
    | `Self
    | `X
    | `Y
    ]

  val to_string_css : [< t ] -> string
end

module Pow_fn : sig
  type t = [ | Calc_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Progress_fn : sig
  type t = [ | Calc_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Radial_gradient_fn : sig
  type t = [ | Radial_gradient_syntax_type.t ]

  val to_string_css : [< t ] -> string
end

module Random_fn : sig
  type t =
    [ Calc_value_type.t
    | Random_value_sharing_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Random_item_fn : sig
  type t = [ | Random_value_sharing_type.t ]

  val to_string_css : [< t ] -> string
end

module Ray_fn : sig
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

  val to_string_css : [< t ] -> string
end

module Clip : sig
  module Rect_fn : sig
    type t =
      [ Bottom_type.t
      | Left_type.t
      | Right_type.t
      | Top_type.t
      ]

    val to_string_css : [< t ] -> string
  end

  type t = [ `Auto ]

  val to_string_css : [< t ] -> string
end

module Rem_fn : sig
  type t = [ | Calc_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Repeating_conic_gradient_fn : sig
  type t = [ | Conic_gradient_syntax_type.t ]

  val to_string_css : [< t ] -> string
end

module Repeating_linear_gradient_fn : sig
  type t = [ | Linear_gradient_syntax_type.t ]

  val to_string_css : [< t ] -> string
end

module Repeating_radial_gradient_fn : sig
  type t = [ | Radial_gradient_syntax_type.t ]

  val to_string_css : [< t ] -> string
end

module Rgb_fn : sig
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `From
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Rgba_fn : sig
  type t =
    [ Alpha_value_type.t
    | Css_data_type.Color.t
    | Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | `From
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Color_base_type : sig
  type t =
    [ Css_data_type.Color.Hex.t
    | Css_data_type.Color.Named.t
    | `Transparent
    ]

  val to_string_css : [< t ] -> string
end

module Rotate3d_fn : sig
  type t =
    [ Css_data_type.Angle.t
    | Css_data_type.Number.t
    | Zero_type.t
    ]

  val to_string_css : [< t ] -> string
end

module RotateX_fn : sig
  type t =
    [ Css_data_type.Angle.t
    | Zero_type.t
    ]

  val to_string_css : [< t ] -> string
end

module RotateY_fn : sig
  type t =
    [ Css_data_type.Angle.t
    | Zero_type.t
    ]

  val to_string_css : [< t ] -> string
end

module RotateZ_fn : sig
  type t =
    [ Css_data_type.Angle.t
    | Zero_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Round_fn : sig
  type t =
    [ Calc_value_type.t
    | `Down
    | `Nearest
    | `To_zero
    | `Up
    ]

  val to_string_css : [< t ] -> string
end

module Running_fn : sig
  type t = [ | Css_data_type.Ident.Custom.t ]

  val to_string_css : [< t ] -> string
end

module Position : sig
  type t =
    [ `Absolute
    | `Fixed
    | `Relative
    | `Static
    | `Sticky
    ]

  val to_string_css : [< t ] -> string
end

module Scale_fn : sig
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    ]

  val to_string_css : [< t ] -> string
end

module Scale3d_fn : sig
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    ]

  val to_string_css : [< t ] -> string
end

module ScaleX_fn : sig
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    ]

  val to_string_css : [< t ] -> string
end

module ScaleY_fn : sig
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    ]

  val to_string_css : [< t ] -> string
end

module ScaleZ_fn : sig
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    ]

  val to_string_css : [< t ] -> string
end

module Scroll_fn : sig
  type t =
    [ Axis_type.t
    | `Nearest
    | `Root
    | `Self
    ]

  val to_string_css : [< t ] -> string
end

module Sign_fn : sig
  type t = [ | Calc_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Sin_fn : sig
  type t = [ | Calc_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Float : sig
  module Snap_block_fn : sig
    type t =
      [ Css_data_type.Length.t
      | `End
      | `Near
      | `Start
      ]

    val to_string_css : [< t ] -> string
  end

  module Snap_inline_fn : sig
    type t =
      [ Css_data_type.Length.t
      | `Left
      | `Near
      | `Right
      ]

    val to_string_css : [< t ] -> string
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

  val to_string_css : [< t ] -> string
end

module Sqrt_fn : sig
  type t = [ | Calc_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Src_fn : sig
  type t = [ | Css_data_type.Css_string.t ]

  val to_string_css : [< t ] -> string
end

module Steps_fn : sig
  type t =
    [ Css_data_type.Integer.t
    | `End
    | `Jump_both
    | `Jump_end
    | `Jump_none
    | `Jump_start
    | `Start
    ]

  val to_string_css : [< t ] -> string
end

module Easing_function_type : sig
  type t =
    [ `Ease
    | `Ease_in
    | `Ease_in_out
    | `Ease_out
    | `Linear
    | `Step_end
    | `Step_start
    ]

  val to_string_css : [< t ] -> string
end

module Animation_timing_function : sig
  type t = [ | Easing_function_type.t ]

  val to_string_css : [< t ] -> string
end

module Transition : sig
  type t =
    [ Easing_function_type.t
    | Single_transition_property_type.t
    | Css_data_type.Time.t
    | Transition_behavior_value_type.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Transition_timing_function : sig
  type t = [ | Easing_function_type.t ]

  val to_string_css : [< t ] -> string
end

module String_fn : sig
  type t =
    [ Css_data_type.Ident.Custom.t
    | `First
    | `First_except
    | `Last
    | `Start
    ]

  val to_string_css : [< t ] -> string
end

module Stripes_fn : sig
  type t =
    [ Css_data_type.Color.t
    | Css_data_type.Flex_fr.t
    | Length_percentage_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Border_block_end_color : sig
  type t = [ | Css_data_type.Color.t ]

  val to_string_css : [< t ] -> string
end

module Border_block_start_color : sig
  type t = [ | Css_data_type.Color.t ]

  val to_string_css : [< t ] -> string
end

module Border_bottom_color : sig
  type t = [ | Css_data_type.Color.t ]

  val to_string_css : [< t ] -> string
end

module Border_color : sig
  type t = [ | Css_data_type.Color.t ]

  val to_string_css : [< t ] -> string
end

module Border_inline_end_color : sig
  type t = [ | Css_data_type.Color.t ]

  val to_string_css : [< t ] -> string
end

module Border_inline_start_color : sig
  type t = [ | Css_data_type.Color.t ]

  val to_string_css : [< t ] -> string
end

module Border_left_color : sig
  type t = [ | Css_data_type.Color.t ]

  val to_string_css : [< t ] -> string
end

module Border_right_color : sig
  type t = [ | Css_data_type.Color.t ]

  val to_string_css : [< t ] -> string
end

module Border_top_color : sig
  type t = [ | Css_data_type.Color.t ]

  val to_string_css : [< t ] -> string
end

module Border_block_color : sig
  type t = [ | Border_top_color.t ]

  val to_string_css : [< t ] -> string
end

module Border_inline_color : sig
  type t = [ | Border_top_color.t ]

  val to_string_css : [< t ] -> string
end

module Outline_color : sig
  type t =
    [ Css_data_type.Color.t
    | `Auto
    ]

  val to_string_css : [< t ] -> string
end

module Outline : sig
  type t =
    [ Outline_color.t
    | Outline_style.t
    | Outline_width.t
    ]

  val to_string_css : [< t ] -> string
end

module Superellipse_fn : sig
  type t =
    [ Css_data_type.Number.t
    | `Negative_infinity
    | `Infinity
    ]

  val to_string_css : [< t ] -> string
end

module Corner_shape_value_type : sig
  type t =
    [ `Bevel
    | `Notch
    | `Round
    | `Scoop
    | `Square
    | `Squircle
    ]

  val to_string_css : [< t ] -> string
end

module Corner_block_end_shape : sig
  type t = [ | Corner_shape_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Corner_block_start_shape : sig
  type t = [ | Corner_shape_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Corner_bottom_left_shape : sig
  type t = [ | Corner_shape_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Corner_bottom_right_shape : sig
  type t = [ | Corner_shape_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Corner_bottom_shape : sig
  type t = [ | Corner_shape_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Corner_end_end_shape : sig
  type t = [ | Corner_shape_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Corner_end_start_shape : sig
  type t = [ | Corner_shape_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Corner_inline_end_shape : sig
  type t = [ | Corner_shape_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Corner_inline_start_shape : sig
  type t = [ | Corner_shape_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Corner_left_shape : sig
  type t = [ | Corner_shape_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Corner_right_shape : sig
  type t = [ | Corner_shape_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Corner_shape : sig
  type t = [ | Corner_shape_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Corner_start_end_shape : sig
  type t = [ | Corner_shape_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Corner_start_start_shape : sig
  type t = [ | Corner_shape_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Corner_top_left_shape : sig
  type t = [ | Corner_shape_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Corner_top_right_shape : sig
  type t = [ | Corner_shape_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Corner_top_shape : sig
  type t = [ | Corner_shape_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Tan_fn : sig
  type t = [ | Calc_value_type.t ]

  val to_string_css : [< t ] -> string
end

module Transform_mix_fn : sig
  type t = [ | Css_data_type.Percentage.t ]

  val to_string_css : [< t ] -> string
end

module Translate3d_fn : sig
  type t =
    [ Css_data_type.Length.t
    | Length_percentage_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Transform : sig
  module Matrix_fn : sig
    type t = [ | Css_data_type.Number.t ]

    val to_string_css : [< t ] -> string
  end

  module Rotate_fn : sig
    type t =
      [ Css_data_type.Angle.t
      | Zero_type.t
      ]

    val to_string_css : [< t ] -> string
  end

  module Scale_fn : sig
    type t = [ | Css_data_type.Number.t ]

    val to_string_css : [< t ] -> string
  end

  module ScaleX_fn : sig
    type t = [ | Css_data_type.Number.t ]

    val to_string_css : [< t ] -> string
  end

  module ScaleY_fn : sig
    type t = [ | Css_data_type.Number.t ]

    val to_string_css : [< t ] -> string
  end

  module Skew_fn : sig
    type t =
      [ Css_data_type.Angle.t
      | Zero_type.t
      ]

    val to_string_css : [< t ] -> string
  end

  module SkewX_fn : sig
    type t =
      [ Css_data_type.Angle.t
      | Zero_type.t
      ]

    val to_string_css : [< t ] -> string
  end

  module SkewY_fn : sig
    type t =
      [ Css_data_type.Angle.t
      | Zero_type.t
      ]

    val to_string_css : [< t ] -> string
  end

  module Translate_fn : sig
    type t = [ | Length_percentage_type.t ]

    val to_string_css : [< t ] -> string
  end

  module TranslateX_fn : sig
    type t = [ | Length_percentage_type.t ]

    val to_string_css : [< t ] -> string
  end

  module TranslateY_fn : sig
    type t = [ | Length_percentage_type.t ]

    val to_string_css : [< t ] -> string
  end

  type t = [ `None ]

  val to_string_css : [< t ] -> string
end

module TranslateZ_fn : sig
  type t = [ | Css_data_type.Length.t ]

  val to_string_css : [< t ] -> string
end

module Url_fn : sig
  type t = [ | Css_data_type.Css_string.t ]

  val to_string_css : [< t ] -> string
end

module Paint_type : sig
  type t =
    [ Css_data_type.Color.t
    | `Context_fill
    | `Context_stroke
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Backdrop_filter : sig
  type t = [ `None ]

  val to_string_css : [< t ] -> string
end

module Cursor : sig
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

  val to_string_css : [< t ] -> string
end

module Fill : sig
  type t = [ | Paint_type.t ]

  val to_string_css : [< t ] -> string
end

module Fill_image : sig
  type t = [ | Paint_type.t ]

  val to_string_css : [< t ] -> string
end

module Filter : sig
  module Blur_fn : sig
    type t = [ | Css_data_type.Length.t ]

    val to_string_css : [< t ] -> string
  end

  module Brightness_fn : sig
    type t =
      [ Css_data_type.Number.t
      | Css_data_type.Percentage.t
      ]

    val to_string_css : [< t ] -> string
  end

  module Contrast_fn : sig
    type t =
      [ Css_data_type.Number.t
      | Css_data_type.Percentage.t
      ]

    val to_string_css : [< t ] -> string
  end

  module Drop_shadow_fn : sig
    type t =
      [ Css_data_type.Color.t
      | Css_data_type.Length.t
      ]

    val to_string_css : [< t ] -> string
  end

  module Grayscale_fn : sig
    type t =
      [ Css_data_type.Number.t
      | Css_data_type.Percentage.t
      ]

    val to_string_css : [< t ] -> string
  end

  module Hue_rotate_fn : sig
    type t =
      [ Css_data_type.Angle.t
      | Zero_type.t
      ]

    val to_string_css : [< t ] -> string
  end

  module Invert_fn : sig
    type t =
      [ Css_data_type.Number.t
      | Css_data_type.Percentage.t
      ]

    val to_string_css : [< t ] -> string
  end

  module Opacity_fn : sig
    type t =
      [ Css_data_type.Number.t
      | Css_data_type.Percentage.t
      ]

    val to_string_css : [< t ] -> string
  end

  module Saturate_fn : sig
    type t =
      [ Css_data_type.Number.t
      | Css_data_type.Percentage.t
      ]

    val to_string_css : [< t ] -> string
  end

  module Sepia_fn : sig
    type t =
      [ Css_data_type.Number.t
      | Css_data_type.Percentage.t
      ]

    val to_string_css : [< t ] -> string
  end

  type t = [ `None ]

  val to_string_css : [< t ] -> string
end

module Marker : sig
  type t = [ `None ]

  val to_string_css : [< t ] -> string
end

module Marker_end : sig
  type t = [ `None ]

  val to_string_css : [< t ] -> string
end

module Marker_mid : sig
  type t = [ `None ]

  val to_string_css : [< t ] -> string
end

module Marker_start : sig
  type t = [ `None ]

  val to_string_css : [< t ] -> string
end

module Stroke : sig
  type t = [ | Paint_type.t ]

  val to_string_css : [< t ] -> string
end

module Stroke_image : sig
  type t = [ | Paint_type.t ]

  val to_string_css : [< t ] -> string
end

module Image_fn : sig
  type t =
    [ Css_data_type.Color.t
    | Css_data_type.Css_string.t
    | `Ltr
    | `Rtl
    ]

  val to_string_css : [< t ] -> string
end

module Bg_image_type : sig
  type t = [ `None ]

  val to_string_css : [< t ] -> string
end

module Bg_layer_type : sig
  type t =
    [ Attachment_type.t
    | Bg_image_type.t
    | Bg_position_type.t
    | Bg_size_type.t
    | Repeat_style_type.t
    | Visual_box_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Cf_image_type : sig
  type t =
    [ Css_data_type.Color.t
    | Css_data_type.Percentage.t
    ]

  val to_string_css : [< t ] -> string
end

module Image_set_option_type : sig
  type t =
    [ Css_data_type.Resolution.t
    | Css_data_type.Css_string.t
    ]

  val to_string_css : [< t ] -> string
end

module Mask_reference_type : sig
  type t = [ `None ]

  val to_string_css : [< t ] -> string
end

module Background : sig
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

  val to_string_css : [< t ] -> string
end

module Background_image : sig
  type t = [ | Bg_image_type.t ]

  val to_string_css : [< t ] -> string
end

module Background_tbd : sig
  type t = [ | Bg_layer_type.t ]

  val to_string_css : [< t ] -> string
end

module Border_image_source : sig
  type t = [ `None ]

  val to_string_css : [< t ] -> string
end

module Border_image : sig
  type t =
    [ Border_image_outset.t
    | Border_image_repeat.t
    | Border_image_slice.t
    | Border_image_source.t
    | Border_image_width.t
    ]

  val to_string_css : [< t ] -> string
end

module List_style_image : sig
  type t = [ `None ]

  val to_string_css : [< t ] -> string
end

module Mask : sig
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

  val to_string_css : [< t ] -> string
end

module Mask_border_source : sig
  type t = [ `None ]

  val to_string_css : [< t ] -> string
end

module Mask_border : sig
  type t =
    [ Mask_border_mode.t
    | Mask_border_outset.t
    | Mask_border_repeat.t
    | Mask_border_slice.t
    | Mask_border_source.t
    | Mask_border_width.t
    ]

  val to_string_css : [< t ] -> string
end

module Mask_image : sig
  type t = [ | Mask_reference_type.t ]

  val to_string_css : [< t ] -> string
end

module Filter_fn : sig
  type t = [ | Css_data_type.Css_string.t ]

  val to_string_css : [< t ] -> string
end

module Symbols_fn : sig
  type t =
    [ Css_data_type.Css_string.t
    | `Alphabetic
    | `Cyclic
    | `Fixed
    | `Numeric
    | `Symbolic
    ]

  val to_string_css : [< t ] -> string
end

module List_style_type : sig
  type t =
    [ Css_data_type.Css_string.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module List_style : sig
  type t =
    [ List_style_image.t
    | List_style_position.t
    | List_style_type.t
    ]

  val to_string_css : [< t ] -> string
end

module Counters_fn : sig
  type t = [ | Css_data_type.Css_string.t ]

  val to_string_css : [< t ] -> string
end

module Content_list_type : sig
  type t = [ | Css_data_type.Css_string.t ]

  val to_string_css : [< t ] -> string
end

module Bookmark_label : sig
  type t = [ | Content_list_type.t ]

  val to_string_css : [< t ] -> string
end

module Content : sig
  type t =
    [ Content_list_type.t
    | Css_data_type.Css_string.t
    | `None
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Copy_into : sig
  type t =
    [ Css_data_type.Ident.Custom.t
    | `Content
    | `Element
    | `None
    | `Text
    ]

  val to_string_css : [< t ] -> string
end

module Target_counter_fn : sig
  type t =
    [ Css_data_type.Ident.Custom.t
    | Css_data_type.Css_string.t
    ]

  val to_string_css : [< t ] -> string
end

module Target_counters_fn : sig
  type t =
    [ Css_data_type.Ident.Custom.t
    | Css_data_type.Css_string.t
    ]

  val to_string_css : [< t ] -> string
end

module Target_text_fn : sig
  type t =
    [ Css_data_type.Css_string.t
    | `After
    | `Before
    | `Content
    | `First_letter
    ]

  val to_string_css : [< t ] -> string
end

module Var_fn : sig
  type t = [ | Css_data_type.Custom_property_name.t ]

  val to_string_css : [< t ] -> string
end

module View_fn : sig
  type t =
    [ Axis_type.t
    | View_timeline_inset.t
    ]

  val to_string_css : [< t ] -> string
end

module Single_animation_timeline_type : sig
  type t =
    [ Css_data_type.Ident.Dashed.t
    | `Auto
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Animation : sig
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

  val to_string_css : [< t ] -> string
end

module Animation_timeline : sig
  type t = [ | Single_animation_timeline_type.t ]

  val to_string_css : [< t ] -> string
end

module Progress_source_type : sig
  type t =
    [ Css_data_type.Number.t
    | Css_data_type.Percentage.t
    | Animation_timeline.t
    ]

  val to_string_css : [< t ] -> string
end

module Animation_trigger : sig
  type t =
    [ Css_data_type.Ident.Dashed.t
    | Length_percentage_type.t
    | Single_animation_trigger_behavior_type.t
    | `Auto
    | `None
    | `Normal
    ]

  val to_string_css : [< t ] -> string
end

module Animation_trigger_timeline : sig
  type t = [ | Single_animation_timeline_type.t ]

  val to_string_css : [< t ] -> string
end

module Calc_interpolate_fn : sig
  module Input_position_type : sig
    type t =
      [ Css_data_type.Number.t
      | Css_data_type.Percentage.t
      ]

    val to_string_css : [< t ] -> string
  end

  type t =
    [ Calc_value_type.t
    | Easing_function_type.t
    | Input_position_type.t
    | Progress_source_type.t
    | `By
    ]

  val to_string_css : [< t ] -> string
end

module Color_interpolate_fn : sig
  module Input_position_type : sig
    type t =
      [ Css_data_type.Number.t
      | Css_data_type.Percentage.t
      ]

    val to_string_css : [< t ] -> string
  end

  type t =
    [ Css_data_type.Color.t
    | Color_interpolation_method_type.t
    | Easing_function_type.t
    | Input_position_type.t
    | Progress_source_type.t
    | `By
    ]

  val to_string_css : [< t ] -> string
end

module Interpolate_fn : sig
  module Input_position_type : sig
    type t =
      [ Css_data_type.Number.t
      | Css_data_type.Percentage.t
      ]

    val to_string_css : [< t ] -> string
  end

  type t =
    [ Easing_function_type.t
    | Input_position_type.t
    | Keyframes_name_type.t
    | Progress_source_type.t
    | `By
    | `Of
    ]

  val to_string_css : [< t ] -> string
end

module Transform_interpolate_fn : sig
  module Input_position_type : sig
    type t =
      [ Css_data_type.Number.t
      | Css_data_type.Percentage.t
      ]

    val to_string_css : [< t ] -> string
  end

  type t =
    [ Easing_function_type.t
    | Input_position_type.t
    | Progress_source_type.t
    | `By
    ]

  val to_string_css : [< t ] -> string
end

module Border_shape : sig
  type t =
    [ Geometry_box_type.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Clip_path : sig
  type t =
    [ Geometry_box_type.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Object_view_box : sig
  type t = [ `None ]

  val to_string_css : [< t ] -> string
end

module Offset_path : sig
  type t =
    [ Coord_box_type.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Offset : sig
  type t =
    [ Offset_anchor.t
    | Offset_distance.t
    | Offset_path.t
    | Offset_position.t
    | Offset_rotate.t
    ]

  val to_string_css : [< t ] -> string
end

module Shape_inside : sig
  type t =
    [ `Auto
    | `Display
    | `Outside_shape
    | `Shape_box
    ]

  val to_string_css : [< t ] -> string
end

module Shape_outside : sig
  type t =
    [ Shape_box_type.t
    | `None
    ]

  val to_string_css : [< t ] -> string
end

module Shape_subtract : sig
  type t = [ `None ]

  val to_string_css : [< t ] -> string
end
