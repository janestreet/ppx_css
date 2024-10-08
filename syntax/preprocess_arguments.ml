open! Core

type t =
  { dont_hash_prefixes : String.Set.t
  ; dont_hash : String.Set.t
  }

let singleton =
  ref { dont_hash_prefixes = String.Set.empty; dont_hash = String.Set.empty }
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
