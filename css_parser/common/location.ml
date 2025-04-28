open! Core

type t = Warnings.loc =
  { loc_start : Types.Position.t
  ; loc_end : Types.Position.t
  ; loc_ghost : bool
  }
[@@deriving sexp_of, equal]

let to_string { loc_start; loc_end; loc_ghost = _ } =
  [%string "(%{loc_start#Types.Position}, %{loc_end#Types.Position})"]
;;

let of_positions ~start ~end_ = { loc_start = start; loc_end = end_; loc_ghost = false }

let merge
  ~start:{ loc_start; loc_end = _; loc_ghost = _ }
  ~end_:{ loc_end; loc_start = _; loc_ghost = _ }
  =
  of_positions ~start:loc_start ~end_:loc_end
;;

let from_token (_, loc) = loc

let between_tokens ~start:(_, start_loc) ~end_:(_, end_loc) =
  merge ~start:start_loc ~end_:end_loc
;;
