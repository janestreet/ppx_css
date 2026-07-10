open Core

(** [float_to_string] converts a float to a human-readable string. It drops all trailing
    zeroes and bounds the significant digits after the zero to 5 places. Ocaml uses 64 bit
    floats, so our float epsilon is 2.22e-16. While this still technically allows for
    floating point precision issues for very large numbers, in practice I don't think this
    will have an effect as CSS doesn't require numbers large enough for this to actually
    affect anything *)
let float_to_string f =
  (* Account for floating point precision errors *)
  Float.to_string_hum ~delimiter:'_' ~decimals:5 ~strip_zero:true f
  |> (* Unfortunately cannot set delimiter to an empty char, so have to filter it out *)
  String.filter ~f:(function
    | '_' -> false
    | _ -> true)
;;

let filter_allowed_ident_chars s =
  String.filter s ~f:(function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' | '-' -> true
    | _ -> false)
;;

let normalize_identifier s =
  (* We are restricting what the identifiers can be returned for simplicity. This should
     be fine, as we're just trying to make these identifiers unique

     https://www.w3.org/TR/css-syntax-3/#check-if-three-code-points-would-start-an-ident-sequence
  *)
  match filter_allowed_ident_chars s |> String.to_list with
  | [] -> ""
  | first :: rest ->
    let first_char_with_prefix =
      (* Matches [ident-start] code point but removes the non-ASCII values
         https://www.w3.org/TR/css-syntax-3/#ident-start-code-point *)
      match first with
      | 'a' .. 'z' | 'A' .. 'Z' | '_' -> [ first ]
      | _ ->
        (* Add an underscore if this is not an ident start char. We should have filtered
           out all invalid chars, so [first] should be a valid ident char *)
        [ '_'; first ]
    in
    String.of_char_list (first_char_with_prefix @ rest)
;;

module Css_string = struct
  type t = [ `String of string ]

  let to_string_css = function
    | `String s ->
      (* Escape any quotes that exist within *)
      let s =
        String.substr_replace_all ~pattern:{|\|} ~with_:{|\\|} s
        |> String.substr_replace_all ~pattern:{|"|} ~with_:{|\"|}
      in
      {%string|"%{s}"|}
  ;;
end

module Ident = struct
  module Custom = struct
    type ident = string
    type t = [ `Custom_ident of ident ]

    let create s =
      let normalized = normalize_identifier s in
      `Custom_ident {%string|%{normalized}-custom-ident|}
    ;;

    let create_unsafe s = `Custom_ident s

    let to_string_css = function
      | `Custom_ident s -> s
    ;;
  end

  module Dashed = struct
    type ident = string
    type t = [ `Dashed_ident of ident ]

    let create s =
      let s = String.chop_prefix_if_exists s ~prefix:"--" |> normalize_identifier in
      `Dashed_ident {%string|--%{s}|}
    ;;

    let to_string_css = function
      | `Dashed_ident s -> s
    ;;
  end

  (* [ident] is defined multiple times here so that the types in the [mli] are unique and
     non-interchangeable *)
  type ident = string

  type t =
    [ Custom.t
    | Dashed.t
    | `Ident of ident
    ]

  let create s =
    match normalize_identifier s with
    | "" ->
      Or_error.error_s
        [%message "Input string was normalized to an empty string" ~input:(s : string)]
    | value -> Ok (`Ident value)
  ;;

  let create_exn s =
    match create s with
    | Ok value -> value
    | Error error -> Error.raise error
  ;;

  let to_string_css = function
    | #Custom.t as t -> Custom.to_string_css t
    | #Dashed.t as t -> Dashed.to_string_css t
    | `Ident s -> s
  ;;
end

module Custom_property_name = struct
  type ident = string
  type t = [ `Custom_property_name of ident ]

  let create s =
    match String.chop_prefix_if_exists s ~prefix:"--" |> normalize_identifier with
    | "" ->
      Or_error.error_string
        {%string|[%{s}] is normalized to [--] which is not a valid custom property name|}
    | s -> Ok (`Custom_property_name {%string|--%{s}|})
  ;;

  let create_exn s =
    match create s with
    | Ok s -> s
    | Error error -> Error.raise error
  ;;

  let to_string_css = function
    | `Custom_property_name s -> s
  ;;
end

module Integer = struct
  type t = [ `Int of int ]

  let to_string_css = function
    | `Int n -> Int.to_string n
  ;;
end

module Number = struct
  type t =
    [ Integer.t
    | `Float of float
    | `Number_with_exponent of float * float
    ]

  let to_string_css = function
    | #Integer.t as t -> Integer.to_string_css t
    | `Float n -> float_to_string n
    | `Number_with_exponent (n, e) -> {%string|%{float_to_string n}e%{float_to_string e}|}
  ;;
end

module Custom_property = struct
  type 'a t = Custom_property_name.t * 'a option

  let name t = fst t
  let default t = snd t

  module Expert = struct
    let create_without_registering ?default name =
      let%map.Or_error name = Custom_property_name.create name in
      name, default
    ;;

    let create_without_registering_exn ?default name =
      let name = Custom_property_name.create_exn name in
      name, default
    ;;

    let create_without_registering' ?default name = name, default
  end
end

module Var = struct
  type 'a t = [ `Var of Custom_property_name.t * 'a option ]

  let to_string_css (a_to_string_css : 'a -> string) t =
    match t with
    | `Var (name, default) ->
      let value_suffix =
        match default with
        | None -> ""
        | Some default -> [%string ", %{a_to_string_css default}"]
      in
      let name = Custom_property_name.to_string_css name in
      [%string "var(%{name}%{value_suffix})"]
  ;;
end

module Global_values = struct
  type 'a t =
    [ 'a Var.t
    | `Initial
    | `Inherit
    | `Revert
    | `Revert_layer
    | `Unset
    ]

  let to_string_css to_string_css = function
    | #Var.t as t -> Var.to_string_css to_string_css t
    | `Initial -> "initial"
    | `Inherit -> "inherit"
    | `Revert -> "revert"
    | `Revert_layer -> "revert-layer"
    | `Unset -> "unset"
  ;;
end

module Percentage = struct
  type t = [ `Percent of Percent.t ]

  let to_string_css = function
    | `Percent p ->
      let p = Percent.to_percentage p |> float_to_string in
      p ^ "%"
  ;;
end

module Time = struct
  type t =
    [ `Seconds of float
    | `Milliseconds of float
    ]

  let to_string_css = function
    | `Seconds s -> {%string|%{float_to_string s}s|}
    | `Milliseconds ms -> {%string|%{float_to_string ms}ms|}
  ;;
end

module Flex_fr = struct
  type t = [ `Fr of float ]

  let to_string_css = function
    | `Fr f -> {%string|%{float_to_string f}fr|}
  ;;
end

module Angle = struct
  type t =
    [ `Deg of float
    | `Grad of float
    | `Rad of float
    | `Turn of float
    ]

  let to_string_css = function
    | `Deg n -> {%string|%{float_to_string n}deg|}
    | `Grad n -> {%string|%{float_to_string n}grad|}
    | `Rad n -> {%string|%{float_to_string n}rad|}
    | `Turn n -> {%string|%{float_to_string n}turn|}
  ;;
end

module Alpha_value = struct
  type t =
    [ Number.t
    | Percentage.t
    ]

  let to_string_css = function
    | #Number.t as t -> Number.to_string_css t
    | #Percentage.t as p -> Percentage.to_string_css p
  ;;
end

let percentage_number_none_to_string_css : [ Percentage.t | Number.t | `None ] -> string
  = function
  | #Number.t as t -> Number.to_string_css t
  | #Percentage.t as t -> Percentage.to_string_css t
  | `None -> "none"
;;

module Color = struct
  module Named = struct
    type t =
      [ `Red
      | `Purple
      | `Teal
      | `Yellow
      | `Green
      ]

    let to_string_css = function
      | `Red -> "red"
      | `Purple -> "purple"
      | `Teal -> "teal"
      | `Yellow -> "yellow"
      | `Green -> "green"
    ;;
  end

  module Hex = struct
    type hexcode = string
    type t = [ `Hex_color of hexcode ]

    let to_string_css = function
      | `Hex_color hex -> hex
    ;;

    let create s =
      let s = String.chop_prefix_if_exists s ~prefix:"#" in
      match
        ( String.length s
        , String.for_all s ~f:(function
            | 'a' .. 'f' | 'A' .. 'F' | '0' .. '9' -> true
            | _ -> false) )
      with
      (* Hexcodes must have 3, 4, 6, or 8 valid hex characters
         https://www.w3.org/TR/css-color-4/#hex-notation *)
      | (3 | 4 | 6 | 8), true -> Ok (`Hex_color {%string|#%{s}|})
      | _ -> Or_error.error_string {%string|[#%{s}] is not a valid hex color string|}
    ;;

    let create_exn s =
      match create s with
      | Ok value -> value
      | Error error -> Error.raise error
    ;;
  end

  module Hue = struct
    type t =
      [ Number.t
      | Angle.t
      ]

    let to_string_css = function
      | #Number.t as t -> Number.to_string_css t
      | #Angle.t as t -> Angle.to_string_css t
    ;;
  end

  type color_channel =
    [ Percentage.t
    | Number.t
    | `None
    ]

  type hue_channel =
    [ Hue.t
    | `None
    ]

  type alpha_channel =
    [ Alpha_value.t
    | `None
    ]

  type oklcha =
    | Relative of
        { from : color_with_var
        ; l : [ color_channel | `Relative ]
        ; c : [ color_channel | `Relative ]
        ; h : [ hue_channel | `Relative ]
        ; a : [ alpha_channel | `Relative ] option
        }
    | Normal of
        { l : color_channel
        ; c : color_channel
        ; h : hue_channel
        ; a : alpha_channel option
        }

  and lcha =
    | Relative of
        { from : color_with_var
        ; l : [ color_channel | `Relative ]
        ; c : [ color_channel | `Relative ]
        ; h : [ hue_channel | `Relative ]
        ; a : [ alpha_channel | `Relative ] option
        }
    | Normal of
        { l : color_channel
        ; c : color_channel
        ; h : hue_channel
        ; a : alpha_channel option
        }

  and rgba =
    | Relative of
        { from : color_with_var
        ; r : [ color_channel | `Relative ]
        ; g : [ color_channel | `Relative ]
        ; b : [ color_channel | `Relative ]
        ; a : [ alpha_channel | `Relative ] option
        }
    | Normal of
        { r : [ Percentage.t | Number.t | `None ]
        ; g : [ Percentage.t | Number.t | `None ]
        ; b : [ Percentage.t | Number.t | `None ]
        ; a : [ Alpha_value.t | `None ] option
        }

  and hsla =
    | Relative of
        { from : color_with_var
        ; h : [ hue_channel | `Relative ]
        ; s : [ color_channel | `Relative ]
        ; l : [ color_channel | `Relative ]
        ; a : [ alpha_channel | `Relative ] option
        }
    | Normal of
        { h : hue_channel
        ; s : color_channel
        ; l : color_channel
        ; a : alpha_channel option
        }

  and alpha =
    { from : color_with_var
    ; alpha : [ alpha_channel | `Relative ] option
    }

  and lab =
    | Relative of
        { from : color_with_var
        ; l : [ color_channel | `Relative ]
        ; a : [ color_channel | `Relative ]
        ; b : [ color_channel | `Relative ]
        ; alpha : [ alpha_channel | `Relative ] option
        }
    | Normal of
        { l : color_channel
        ; a : color_channel
        ; b : color_channel
        ; alpha : alpha_channel option
        }

  and oklab =
    | Relative of
        { from : color_with_var
        ; l : [ color_channel | `Relative ]
        ; a : [ color_channel | `Relative ]
        ; b : [ color_channel | `Relative ]
        ; alpha : [ alpha_channel | `Relative ] option
        }
    | Normal of
        { l : color_channel
        ; a : color_channel
        ; b : color_channel
        ; alpha : alpha_channel option
        }

  and hwb =
    | Relative of
        { from : color_with_var
        ; h : [ hue_channel | `Relative ]
        ; w : [ color_channel | `Relative ]
        ; b : [ color_channel | `Relative ]
        ; a : [ alpha_channel | `Relative ] option
        }
    | Normal of
        { h : hue_channel
        ; w : color_channel
        ; b : color_channel
        ; a : alpha_channel option
        }

  and t =
    [ `Rgba of rgba
    | `Hsla of hsla
    | `Lcha of lcha
    | `Hwb of hwb
    | `Lab of lab
    | `Oklab of oklab
    | `Oklcha of oklcha
    | `Alpha of alpha
    | `Light_dark of color_with_var * color_with_var
    | `Named of Named.t
    | Hex.t
    ]

  and color_with_var =
    [ `Color of t
    | color_with_var Var.t
    ]

  type color_t = color_with_var

  let color_channel_to_string ~relative = function
    | #color_channel as p -> percentage_number_none_to_string_css p
    | `Relative_to _ -> relative
  ;;

  module Rgba = struct
    type t = rgba =
      | Relative of
          { from : color_t
          ; r : [ color_channel | `Relative ]
          ; g : [ color_channel | `Relative ]
          ; b : [ color_channel | `Relative ]
          ; a : [ alpha_channel | `Relative ] option
          }
      | Normal of
          { r : color_channel
          ; g : color_channel
          ; b : color_channel
          ; a : alpha_channel option
          }

    let create ~r ~g ~b ?a () = Normal { r; g; b; a }
    let create_relative ~r ~g ~b ?a from = Relative { from; r; g; b; a }

    let from = function
      | Relative { from; _ } -> Some from
      | Normal _ -> None
    ;;

    let r = function
      | Relative { from; r = `Relative; _ } -> `Relative_to from
      | Normal { r = #color_channel as r; _ } | Relative { r = #color_channel as r; _ } ->
        r
    ;;

    let g = function
      | Relative { from; g = `Relative; _ } -> `Relative_to from
      | Normal { g = #color_channel as g; _ } | Relative { g = #color_channel as g; _ } ->
        g
    ;;

    let b = function
      | Relative { from; b = `Relative; _ } -> `Relative_to from
      | Normal { b = #color_channel as b; _ } | Relative { b = #color_channel as b; _ } ->
        b
    ;;

    let a = function
      | Relative { from; a = Some `Relative; _ } -> Some (`Relative_to from)
      | Normal { a = Some (#alpha_channel as a); _ }
      | Relative { a = Some (#alpha_channel as a); _ } -> Some a
      | Relative { a = None; _ } | Normal { a = None; _ } -> None
    ;;
  end

  module Hsla = struct
    type t = hsla =
      | Relative of
          { from : color_with_var
          ; h : [ hue_channel | `Relative ]
          ; s : [ color_channel | `Relative ]
          ; l : [ color_channel | `Relative ]
          ; a : [ alpha_channel | `Relative ] option
          }
      | Normal of
          { h : hue_channel
          ; s : color_channel
          ; l : color_channel
          ; a : alpha_channel option
          }

    let create ~h ~s ~l ?a () = Normal { h; s; l; a }
    let create_relative ~h ~s ~l ?a from = Relative { from; h; s; l; a }

    let from = function
      | Relative { from; _ } -> Some from
      | Normal _ -> None
    ;;

    let h = function
      | Relative { from; h = `Relative; _ } -> `Relative_to from
      | Normal { h = #hue_channel as h; _ } | Relative { h = #hue_channel as h; _ } -> h
    ;;

    let s = function
      | Relative { from; s = `Relative; _ } -> `Relative_to from
      | Normal { s = #color_channel as s; _ } | Relative { s = #color_channel as s; _ } ->
        s
    ;;

    let l = function
      | Relative { from; l = `Relative; _ } -> `Relative_to from
      | Normal { l = #color_channel as l; _ } | Relative { l = #color_channel as l; _ } ->
        l
    ;;

    let a = function
      | Relative { from; a = Some `Relative; _ } -> Some (`Relative_to from)
      | Normal { a = Some (#alpha_channel as a); _ }
      | Relative { a = Some (#alpha_channel as a); _ } -> Some a
      | Relative { a = None; _ } | Normal { a = None; _ } -> None
    ;;
  end

  module Oklcha = struct
    type t = oklcha =
      | Relative of
          { from : color_t
          ; l : [ color_channel | `Relative ]
          ; c : [ color_channel | `Relative ]
          ; h : [ hue_channel | `Relative ]
          ; a : [ alpha_channel | `Relative ] option
          }
      | Normal of
          { l : color_channel
          ; c : color_channel
          ; h : hue_channel
          ; a : alpha_channel option
          }

    let create ~l ~c ~h ?a () = Normal { l; c; h; a }
    let create_relative ~l ~c ~h ?a from = Relative { from; l; c; h; a }

    let from = function
      | Relative { from; _ } -> Some from
      | Normal _ -> None
    ;;

    let l = function
      | Relative { from; l = `Relative; _ } -> `Relative_to from
      | Normal { l = #color_channel as l; _ } | Relative { l = #color_channel as l; _ } ->
        l
    ;;

    let c = function
      | Relative { from; c = `Relative; _ } -> `Relative_to from
      | Normal { c = #color_channel as c; _ } | Relative { c = #color_channel as c; _ } ->
        c
    ;;

    let h = function
      | Relative { from; h = `Relative; _ } -> `Relative_to from
      | Normal { h = #hue_channel as h; _ } | Relative { h = #hue_channel as h; _ } -> h
    ;;

    let a = function
      | Relative { from; a = Some `Relative; _ } -> Some (`Relative_to from)
      | Normal { a = Some (#alpha_channel as a); _ }
      | Relative { a = Some (#alpha_channel as a); _ } -> Some a
      | Relative { a = None; _ } | Normal { a = None; _ } -> None
    ;;
  end

  module Lcha = struct
    type t = lcha =
      | Relative of
          { from : color_t
          ; l : [ color_channel | `Relative ]
          ; c : [ color_channel | `Relative ]
          ; h : [ hue_channel | `Relative ]
          ; a : [ alpha_channel | `Relative ] option
          }
      | Normal of
          { l : color_channel
          ; c : color_channel
          ; h : hue_channel
          ; a : alpha_channel option
          }

    let create ~l ~c ~h ?a () = Normal { l; c; h; a }
    let create_relative ~l ~c ~h ?a from = Relative { from; l; c; h; a }

    let from = function
      | Relative { from; _ } -> Some from
      | Normal _ -> None
    ;;

    let l = function
      | Relative { from; l = `Relative; _ } -> `Relative_to from
      | Normal { l = #color_channel as l; _ } | Relative { l = #color_channel as l; _ } ->
        l
    ;;

    let c = function
      | Relative { from; c = `Relative; _ } -> `Relative_to from
      | Normal { c = #color_channel as c; _ } | Relative { c = #color_channel as c; _ } ->
        c
    ;;

    let h = function
      | Relative { from; h = `Relative; _ } -> `Relative_to from
      | Normal { h = #hue_channel as h; _ } | Relative { h = #hue_channel as h; _ } -> h
    ;;

    let a = function
      | Relative { from; a = Some `Relative; _ } -> Some (`Relative_to from)
      | Normal { a = Some (#alpha_channel as a); _ }
      | Relative { a = Some (#alpha_channel as a); _ } -> Some a
      | Relative { a = None; _ } | Normal { a = None; _ } -> None
    ;;
  end

  module Lab = struct
    type t = lab =
      | Relative of
          { from : color_t
          ; l : [ color_channel | `Relative ]
          ; a : [ color_channel | `Relative ]
          ; b : [ color_channel | `Relative ]
          ; alpha : [ alpha_channel | `Relative ] option
          }
      | Normal of
          { l : color_channel
          ; a : color_channel
          ; b : color_channel
          ; alpha : alpha_channel option
          }

    let create ~l ~a ~b ?alpha () = Normal { l; a; b; alpha }
    let create_relative ~l ~a ~b ?alpha from = Relative { from; l; a; b; alpha }

    let from = function
      | Relative { from; _ } -> Some from
      | Normal _ -> None
    ;;

    let l = function
      | Relative { from; l = `Relative; _ } -> `Relative_to from
      | Normal { l = #color_channel as l; _ } | Relative { l = #color_channel as l; _ } ->
        l
    ;;

    let a = function
      | Relative { from; a = `Relative; _ } -> `Relative_to from
      | Normal { a = #color_channel as a; _ } | Relative { a = #color_channel as a; _ } ->
        a
    ;;

    let b = function
      | Relative { from; b = `Relative; _ } -> `Relative_to from
      | Normal { b = #color_channel as b; _ } | Relative { b = #color_channel as b; _ } ->
        b
    ;;

    let alpha = function
      | Relative { from; alpha = Some `Relative; _ } -> Some (`Relative_to from)
      | Normal { alpha = Some (#alpha_channel as alpha); _ }
      | Relative { alpha = Some (#alpha_channel as alpha); _ } -> Some alpha
      | Relative { alpha = None; _ } | Normal { alpha = None; _ } -> None
    ;;
  end

  module Oklab = struct
    type t = oklab =
      | Relative of
          { from : color_t
          ; l : [ color_channel | `Relative ]
          ; a : [ color_channel | `Relative ]
          ; b : [ color_channel | `Relative ]
          ; alpha : [ alpha_channel | `Relative ] option
          }
      | Normal of
          { l : color_channel
          ; a : color_channel
          ; b : color_channel
          ; alpha : alpha_channel option
          }

    let create ~l ~a ~b ?alpha () = Normal { l; a; b; alpha }
    let create_relative ~l ~a ~b ?alpha from = Relative { from; l; a; b; alpha }

    let from = function
      | Relative { from; _ } -> Some from
      | Normal _ -> None
    ;;

    let l = function
      | Relative { from; l = `Relative; _ } -> `Relative_to from
      | Normal { l = #color_channel as l; _ } | Relative { l = #color_channel as l; _ } ->
        l
    ;;

    let a = function
      | Relative { from; a = `Relative; _ } -> `Relative_to from
      | Normal { a = #color_channel as a; _ } | Relative { a = #color_channel as a; _ } ->
        a
    ;;

    let b = function
      | Relative { from; b = `Relative; _ } -> `Relative_to from
      | Normal { b = #color_channel as b; _ } | Relative { b = #color_channel as b; _ } ->
        b
    ;;

    let alpha = function
      | Relative { from; alpha = Some `Relative; _ } -> Some (`Relative_to from)
      | Normal { alpha = Some (#alpha_channel as alpha); _ }
      | Relative { alpha = Some (#alpha_channel as alpha); _ } -> Some alpha
      | Relative { alpha = None; _ } | Normal { alpha = None; _ } -> None
    ;;
  end

  module Hwb = struct
    type t = hwb =
      | Relative of
          { from : color_with_var
          ; h : [ hue_channel | `Relative ]
          ; w : [ color_channel | `Relative ]
          ; b : [ color_channel | `Relative ]
          ; a : [ alpha_channel | `Relative ] option
          }
      | Normal of
          { h : hue_channel
          ; w : color_channel
          ; b : color_channel
          ; a : alpha_channel option
          }

    let create ~h ~w ~b ?a () = Normal { h; w; b; a }
    let create_relative ~h ~w ~b ?a from = Relative { from; h; w; b; a }

    let from = function
      | Relative { from; _ } -> Some from
      | Normal _ -> None
    ;;

    let h = function
      | Relative { from; h = `Relative; _ } -> `Relative_to from
      | Normal { h = #hue_channel as h; _ } | Relative { h = #hue_channel as h; _ } -> h
    ;;

    let w = function
      | Relative { from; w = `Relative; _ } -> `Relative_to from
      | Normal { w = #color_channel as w; _ } | Relative { w = #color_channel as w; _ } ->
        w
    ;;

    let b = function
      | Relative { from; b = `Relative; _ } -> `Relative_to from
      | Normal { b = #color_channel as b; _ } | Relative { b = #color_channel as b; _ } ->
        b
    ;;

    let a = function
      | Relative { from; a = Some `Relative; _ } -> Some (`Relative_to from)
      | Normal { a = Some (#alpha_channel as a); _ }
      | Relative { a = Some (#alpha_channel as a); _ } -> Some a
      | Relative { a = None; _ } | Normal { a = None; _ } -> None
    ;;
  end

  module Alpha = struct
    type t = alpha =
      { from : color_t
      ; alpha : [ alpha_channel | `Relative ] option
      }

    let create ~from ?alpha () = { from; alpha }

    let alpha = function
      | { from; alpha = Some `Relative; _ } -> Some (`Relative_to from)
      | { alpha = Some (#alpha_channel as a); _ } -> Some a
      | { alpha = None; _ } -> None
    ;;
  end

  let rgba ~r ~g ~b ?a () = `Rgba (Rgba.create ~r ~g ~b ?a ())
  let rgba_relative ~r ~g ~b ?a from = `Rgba (Rgba.create_relative ~r ~g ~b ?a from)
  let hsla ~h ~s ~l ?a () = `Hsla (Hsla.create ~h ~s ~l ?a ())
  let hsla_relative ~h ~s ~l ?a from = `Hsla (Hsla.create_relative ~h ~s ~l ?a from)
  let oklcha ~l ~c ~h ?a () = `Oklcha (Oklcha.create ~l ~c ~h ?a ())
  let oklcha_relative ~l ~c ~h ?a from = `Oklcha (Oklcha.create_relative ~l ~c ~h ?a from)
  let lcha ~l ~c ~h ?a () = `Lcha (Lcha.create ~l ~c ~h ?a ())
  let lcha_relative ~l ~c ~h ?a from = `Lcha (Lcha.create_relative ~l ~c ~h ?a from)
  let lab ~l ~a ~b ?alpha () = `Lab (Lab.create ~l ~a ~b ?alpha ())
  let lab_relative ~l ~a ~b ?alpha from = `Lab (Lab.create_relative ~l ~a ~b ?alpha from)
  let oklab ~l ~a ~b ?alpha () = `Oklab (Oklab.create ~l ~a ~b ?alpha ())

  let oklab_relative ~l ~a ~b ?alpha from =
    `Oklab (Oklab.create_relative ~l ~a ~b ?alpha from)
  ;;

  let hwb ~h ~w ~b ?a () = `Hwb (Hwb.create ~h ~w ~b ?a ())
  let hwb_relative ~h ~w ~b ?a from = `Hwb (Hwb.create_relative ~h ~w ~b ?a from)
  let alpha ~from ?alpha () = `Alpha (Alpha.create ~from ?alpha ())

  let rec to_string_css : t -> string = function
    | `Named named -> Named.to_string_css named
    | #Hex.t as t -> Hex.to_string_css t
    | `Light_dark (light, dark) ->
      [%string
        "light-dark(%{with_var_to_string_css light}, %{with_var_to_string_css dark})"]
    | `Lcha lcha ->
      let from = Lcha.from lcha |> from_to_string_css in
      let l = Lcha.l lcha |> color_channel_to_string ~relative:"l" in
      let c = Lcha.c lcha |> color_channel_to_string ~relative:"c" in
      let h = Lcha.h lcha |> hue_channel_to_string_css in
      let a = Lcha.a lcha |> alpha_to_string_css in
      [%string "lch(%{from}%{l} %{c} %{h}%{a})"]
    | `Oklcha oklcha ->
      let from = Oklcha.from oklcha |> from_to_string_css in
      let l = Oklcha.l oklcha |> color_channel_to_string ~relative:"l" in
      let c = Oklcha.c oklcha |> color_channel_to_string ~relative:"c" in
      let h = Oklcha.h oklcha |> hue_channel_to_string_css in
      let a = Oklcha.a oklcha |> alpha_to_string_css in
      [%string "oklch(%{from}%{l} %{c} %{h}%{a})"]
    | `Rgba rgba ->
      let from = Rgba.from rgba |> from_to_string_css in
      let r = Rgba.r rgba |> color_channel_to_string ~relative:"r" in
      let g = Rgba.g rgba |> color_channel_to_string ~relative:"g" in
      let b = Rgba.b rgba |> color_channel_to_string ~relative:"b" in
      let a = Rgba.a rgba in
      let fn_name =
        match a with
        | None -> "rgb"
        | Some _ -> "rgba"
      in
      let a = alpha_to_string_css a in
      [%string "%{fn_name}(%{from}%{r} %{g} %{b}%{a})"]
    | `Hsla hsla ->
      let from = Hsla.from hsla |> from_to_string_css in
      let h = Hsla.h hsla |> hue_channel_to_string_css in
      let s = Hsla.s hsla |> color_channel_to_string ~relative:"s" in
      let l = Hsla.l hsla |> color_channel_to_string ~relative:"l" in
      let a = Hsla.a hsla in
      let fn_name =
        match a with
        | None -> "hsl"
        | Some _ -> "hsla"
      in
      let a = alpha_to_string_css a in
      [%string "%{fn_name}(%{from}%{h} %{s} %{l}%{a})"]
    | `Lab value ->
      let from = Lab.from value |> from_to_string_css in
      let l = Lab.l value |> color_channel_to_string ~relative:"l" in
      let a = Lab.a value |> color_channel_to_string ~relative:"a" in
      let b = Lab.b value |> color_channel_to_string ~relative:"b" in
      let alpha = Lab.alpha value |> alpha_to_string_css in
      [%string "lab(%{from}%{l} %{a} %{b}%{alpha})"]
    | `Oklab value ->
      let from = Oklab.from value |> from_to_string_css in
      let l = Oklab.l value |> color_channel_to_string ~relative:"l" in
      let a = Oklab.a value |> color_channel_to_string ~relative:"a" in
      let b = Oklab.b value |> color_channel_to_string ~relative:"b" in
      let alpha = Oklab.alpha value |> alpha_to_string_css in
      [%string "oklab(%{from}%{l} %{a} %{b}%{alpha})"]
    | `Hwb hwb ->
      let from = Hwb.from hwb |> from_to_string_css in
      let h = Hwb.h hwb |> hue_channel_to_string_css in
      let w = Hwb.w hwb |> color_channel_to_string ~relative:"w" in
      let b = Hwb.b hwb |> color_channel_to_string ~relative:"b" in
      let a = Hwb.a hwb |> alpha_to_string_css in
      [%string "hwb(%{from}%{h} %{w} %{b}%{a})"]
    | `Alpha alpha ->
      let from = Some alpha.from |> from_to_string_css in
      let a =
        Alpha.alpha alpha
        |> alpha_to_string_css
        |> String.lstrip (* remove leading space *)
      in
      [%string "alpha(%{from}%{a})"]

  and hue_channel_to_string_css h =
    match h with
    | #Hue.t as h -> Hue.to_string_css h
    | `None -> percentage_number_none_to_string_css `None
    | `Relative_to _ -> "h"

  and alpha_to_string_css = function
    | None -> ""
    | Some a ->
      let a =
        match a with
        | #Alpha_value.t as a -> Alpha_value.to_string_css a
        | `None -> percentage_number_none_to_string_css `None
        | `Relative_to _ ->
          (* All of the relative colors use the keyword [alpha] to signify the alpha value
             to disambiguate with [a] in certain functions

             https://www.w3.org/TR/css-color-5/#relative-RGB
             https://www.w3.org/TR/css-color-5/#relative-Lab

             You can find more functions in the links above
          *)
          "alpha"
      in
      {%string| / %{a}|}

  and with_var_to_string_css = function
    | `Color color -> to_string_css color
    | #Var.t as t -> Var.to_string_css with_var_to_string_css t

  and from_to_string_css = function
    | None -> ""
    | Some from ->
      let from = with_var_to_string_css from in
      {%string|from %{from} |}
  ;;

  module With_var = struct
    type t = color_with_var

    let to_string_css = with_var_to_string_css
  end
end

module Length = struct
  module Relative = struct
    module Font = struct
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

      let to_string_css = function
        | `Em n -> {%string|%{float_to_string n}em|}
        | `Rem n -> {%string|%{float_to_string n}rem|}
        | `Ex n -> {%string|%{float_to_string n}ex|}
        | `Rex n -> {%string|%{float_to_string n}rex|}
        | `Cap n -> {%string|%{float_to_string n}cap|}
        | `Rcap n -> {%string|%{float_to_string n}rcap|}
        | `Ch n -> {%string|%{float_to_string n}ch|}
        | `Rch n -> {%string|%{float_to_string n}rch|}
        | `Ic n -> {%string|%{float_to_string n}ic|}
        | `Ric n -> {%string|%{float_to_string n}ric|}
        | `Lh n -> {%string|%{float_to_string n}lh|}
        | `Rlh n -> {%string|%{float_to_string n}rlh|}
      ;;
    end

    module Viewport = struct
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

      let to_string_css = function
        | `Vw p -> {%string|%{float_to_string (Percent.to_percentage p)}vw|}
        | `Vh p -> {%string|%{float_to_string (Percent.to_percentage p)}vh|}
        | `Vi p -> {%string|%{float_to_string (Percent.to_percentage p)}vi|}
        | `Vb p -> {%string|%{float_to_string (Percent.to_percentage p)}vb|}
        | `Vmin p -> {%string|%{float_to_string (Percent.to_percentage p)}vmin|}
        | `Vmax p -> {%string|%{float_to_string (Percent.to_percentage p)}vmax|}
        | `Svw p -> {%string|%{float_to_string (Percent.to_percentage p)}svw|}
        | `Svh p -> {%string|%{float_to_string (Percent.to_percentage p)}svh|}
        | `Svi p -> {%string|%{float_to_string (Percent.to_percentage p)}svi|}
        | `Svb p -> {%string|%{float_to_string (Percent.to_percentage p)}svb|}
        | `Svmin p -> {%string|%{float_to_string (Percent.to_percentage p)}svmin|}
        | `Svmax p -> {%string|%{float_to_string (Percent.to_percentage p)}svmax|}
        | `Lvw p -> {%string|%{float_to_string (Percent.to_percentage p)}lvw|}
        | `Lvh p -> {%string|%{float_to_string (Percent.to_percentage p)}lvh|}
        | `Lvi p -> {%string|%{float_to_string (Percent.to_percentage p)}lvi|}
        | `Lvb p -> {%string|%{float_to_string (Percent.to_percentage p)}lvb|}
        | `Lvmin p -> {%string|%{float_to_string (Percent.to_percentage p)}lvmin|}
        | `Lvmax p -> {%string|%{float_to_string (Percent.to_percentage p)}lvmax|}
        | `Dvw p -> {%string|%{float_to_string (Percent.to_percentage p)}dvw|}
        | `Dvh p -> {%string|%{float_to_string (Percent.to_percentage p)}dvh|}
        | `Dvi p -> {%string|%{float_to_string (Percent.to_percentage p)}dvi|}
        | `Dvb p -> {%string|%{float_to_string (Percent.to_percentage p)}dvb|}
        | `Dvmin p -> {%string|%{float_to_string (Percent.to_percentage p)}dvmin|}
        | `Dvmax p -> {%string|%{float_to_string (Percent.to_percentage p)}dvmax|}
      ;;
    end

    type t =
      [ Font.t
      | Viewport.t
      ]

    let to_string_css = function
      | #Font.t as t -> Font.to_string_css t
      | #Viewport.t as t -> Viewport.to_string_css t
    ;;
  end

  module Absolute = struct
    type t =
      [ `Cm of float
      | `Mm of float
      | `Quarter_mm of float
      | `Inch of float
      | `Pica of float
      | `Pt of float
      | `Px of float
      ]

    let to_string_css = function
      | `Cm n -> {%string|%{float_to_string n}cm|}
      | `Mm n -> {%string|%{float_to_string n}mm|}
      | `Quarter_mm n -> {%string|%{float_to_string n}Q|}
      | `Inch n -> {%string|%{float_to_string n}in|}
      | `Pica n -> {%string|%{float_to_string n}pc|}
      | `Pt n ->
        (* [pt] has to stay as-is in order to maintain backwards compatibility with
           [Css_gen] *)
        {%string|%{float_to_string n}pt|}
      | `Px n -> {%string|%{float_to_string n}px|}
    ;;
  end

  (** [Length] represents all possible values that can be used as a CSS length.

      For the most part, the value of the argument will be a [float].

      In some cases, we will have both [UNIT int] and [UNIT_float float] variants in order
      to maintain backwards compatibility with [Css_gen]

      https://www.w3.org/TR/css-values-4/#lengths *)
  type t =
    [ Relative.t
    | Absolute.t
    | `Zero
    ]

  let to_string_css = function
    | #Relative.t as t -> Relative.to_string_css t
    | #Absolute.t as t -> Absolute.to_string_css t
    | `Zero -> "0"
  ;;
end

module Decibel = struct
  type t = [ `Decibel of float ]

  let to_string_css = function
    | `Decibel f -> {%string|%{float_to_string f}dB|}
  ;;
end

module Frequency = struct
  type t =
    [ `Hz of float
    | `Khz of float
    ]

  let to_string_css = function
    | `Hz f -> {%string|%{float_to_string f}Hz|}
    | `Khz f -> {%string|%{float_to_string f}kHz|}
  ;;
end

module Resolution = struct
  type t =
    [ `Dpi of float
    | `Dpcm of float
    | `Dppx of float
    ]

  let to_string_css = function
    | `Dpi f -> {%string|%{float_to_string f}dpi|}
    | `Dpcm f -> {%string|%{float_to_string f}dpcm|}
    | `Dppx f -> {%string|%{float_to_string f}dppx|}
  ;;
end
