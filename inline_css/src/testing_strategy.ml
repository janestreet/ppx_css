open! Core

type t = string Reversed_list.t ref

let name = "testing-strategy"
let initialize () = Or_error.return (ref Reversed_list.[])
let update : t -> string -> unit = fun t s -> t := s :: !t

module For_testing = struct
  let dump_testing_state t = Reversed_list.rev !t |> String.concat_lines
end
