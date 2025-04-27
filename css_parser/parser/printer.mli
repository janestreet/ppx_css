open! Core
module Types = Types

val stylesheet_to_string : Types.Stylesheet.t -> string
val style_block_contents_to_string : Types.Style_block.t -> string
val for_apply_style_to_string : Types.For_apply_style.t -> string
