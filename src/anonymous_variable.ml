open! Core
open Ppxlib

module Name : sig
  type t

  include Identifiable.S with type t := t

  val to_css_variable : t -> string
end = struct
  include String

  let to_css_variable t = [%string {|var(--%{t})|}]
end

let curr = ref 0

let mint_name () =
  incr curr;
  Name.of_string [%string "ppx_css_anonymous_var_%{!curr#Int}"]
;;

type t =
  { name : Name.t
  ; expression : expression
  }
[@@deriving fields ~getters]

let of_expression expression =
  let name = mint_name () in
  { name; expression }
;;

module Collection = struct
  type nonrec t =
    { variables : t list
    ; unique_name : string
    }

  let empty = { unique_name = "ppx_css_empty_anonymous_variables"; variables = [] }

  let of_list variables =
    match variables with
    | [] -> empty
    | _ :: _ ->
      let unique_name = gen_symbol ~prefix:"ppx_css__internal_anonymous_variables" () in
      { variables; unique_name }
  ;;
end

module For_testing = struct
  let restart_identifiers () = curr := 0
end
