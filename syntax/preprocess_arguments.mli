open! Core

(** This module contains the parameters sent in via the jbuild/dune file's [preprocess]
    field.

    The way of interfacing/reading/setting is side-effecty. The arguments 
    are read at the top-level when the module is loaded, and then read with [get] *)

type lazy_loading_optimization =
  (* Run CSS graph traversal and lazily instantiate the styles *)
  | Lazy_graph
  (* Eagerly adds all CSS to the page *)
  | Eager
  (* The user has not explicitly set the value. Will do whatever the default is 
     (currently Eager) *)
  | Default

type t =
  { dont_hash_prefixes : String.Set.t
  ; dont_hash : String.Set.t
  ; lazy_loading_optimization : lazy_loading_optimization
  }

val get : unit -> t
val add_dont_hash : string -> unit
val add_dont_hash_prefixes : string -> unit
val set_lazy_loading_optimization : bool option -> unit
