open! Core

type t = Missing_semicolon_at_end_of_declaration_list of { loc : Warnings.loc }

module Handler : sig
  type nonrec t = t -> unit
end
