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

let sanitize_filename filename =
  (* Extract just the basename without extension *)
  let basename = Filename.basename filename in
  let without_extension =
    match String.rsplit2 basename ~on:'.' with
    | Some (base, _ext) -> base
    | None -> basename
  in
  (* Replace any non-alphanumeric characters with underscores *)
  String.map without_extension ~f:(fun c -> if Char.is_alphanum c then c else '_')
;;

let mint_name ~loc =
  incr curr;
  let filename = Ppx_here_expander.expand_filename loc.loc_start.pos_fname in
  let sanitized_filename = sanitize_filename filename in
  Name.of_string [%string "ppx_css_%{sanitized_filename}_anon_variable_%{!curr#Int}"]
;;

type t =
  { name : Name.t
  ; expression : expression
  }
[@@deriving fields ~getters]

let of_expression ~loc expression =
  let name = mint_name ~loc in
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
