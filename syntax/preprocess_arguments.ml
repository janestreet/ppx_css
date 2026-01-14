open! Core

type lazy_loading_optimization =
  (* Run CSS graph traversal and lazily instantiate the styles *)
  | Lazy_graph
  (* Eagerly adds all CSS to the page *)
  | Eager
  (* The user has not explicitly set the value. Will do whatever the default is (currently
     Eager) *)
  | Default

type t =
  { dont_hash_prefixes : String.Set.t
  ; dont_hash : String.Set.t
  ; lazy_loading_optimization : lazy_loading_optimization
  }

let singleton =
  ref
    { dont_hash_prefixes = String.Set.empty
    ; dont_hash = String.Set.empty
    ; lazy_loading_optimization = Eager
    }
;;

let get () = !singleton
let add ~get ~set new_ = singleton := set !singleton (Set.add (get !singleton) new_)

let add_dont_hash new_ =
  add ~get:(fun t -> t.dont_hash) ~set:(fun t dont_hash -> { t with dont_hash }) new_
;;

let add_dont_hash_prefixes new_ =
  add
    ~get:(fun t -> t.dont_hash_prefixes)
    ~set:(fun t dont_hash_prefixes -> { t with dont_hash_prefixes })
    new_
;;

let set_lazy_loading_optimization value =
  let optimization_value =
    match value with
    | Some true -> Lazy_graph
    | Some false -> Eager
    | None -> Default
  in
  singleton := { !singleton with lazy_loading_optimization = optimization_value }
;;
