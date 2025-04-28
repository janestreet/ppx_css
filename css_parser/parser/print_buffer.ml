open! Core

let make_padding ~size level = String.make (level * size) ' '

type t =
  { indent_size : int
  ; mutable indent_level : int
  ; mutable current_working_line : string Reversed_list.t
  ; buffer : Buffer.t
  }

let is_at_start_of_line t = Reversed_list.is_empty t.current_working_line
let clear_working_line t = t.current_working_line <- Reversed_list.[]

let undo_and_return t =
  match t.current_working_line with
  | [] -> raise_s [%message "Tried to undo changes to an empty working line"]
  | hd :: tl ->
    t.current_working_line <- tl;
    hd
;;

let undo_and_ignore t = (undo_and_return t : string) |> ignore
let get_current_working_line t = Reversed_list.rev t.current_working_line |> String.concat

let flush t =
  get_current_working_line t |> Buffer.add_string t.buffer;
  clear_working_line t
;;

let append t s =
  let string_to_append =
    match is_at_start_of_line t with
    | true ->
      let indent = make_padding ~size:t.indent_size t.indent_level in
      [%string "%{indent}%{s}"]
    | false -> s
  in
  t.current_working_line <- string_to_append :: t.current_working_line
;;

let start_newline t =
  flush t;
  Buffer.add_string t.buffer "\n"
;;

let indent t = t.indent_level <- t.indent_level + 1
let dedent t = t.indent_level <- t.indent_level - 1

let create ?(indent_size = 2) capacity =
  { indent_level = 0
  ; current_working_line = Reversed_list.[]
  ; indent_size
  ; buffer = Buffer.create capacity
  }
;;

let contents t =
  flush t;
  Buffer.contents t.buffer
;;
