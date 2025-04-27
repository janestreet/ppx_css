open! Core

type t = Missing_semicolon_at_end_of_declaration_list of { loc : Warnings.loc }

module Handler = struct
  type nonrec t = t -> unit
end
