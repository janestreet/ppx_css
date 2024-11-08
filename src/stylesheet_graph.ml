open! Core

module V = struct
  module T = struct
    type t = Css_identifier.t [@@deriving compare, sexp, hash, equal]
  end

  include Hashable.Make (T)
  include T
end

type t =
  { vertices : V.Hash_set.t
  ; edges : V.t list V.Table.t
  }

let create () = { vertices = V.Hash_set.create (); edges = V.Table.create () }
let iter_vertex f t = Hash_set.iter ~f t.vertices

let iter_succ f t v =
  Hashtbl.find_and_call
    t.edges
    v
    ~if_found:(fun vs -> List.iter ~f vs)
    ~if_not_found:(fun _ -> ())
;;

let add_vertex_if_needed t v = Hash_set.add t.vertices v

let add_edge t v1 v2 =
  add_vertex_if_needed t v1;
  add_vertex_if_needed t v2;
  Hashtbl.add_multi t.edges ~key:v1 ~data:v2;
  Hashtbl.add_multi t.edges ~key:v2 ~data:v1
;;
