open! Core

(* A buffer that holds the working line so that some actions can be undone. Once
   [clear_working_line], [flush] or [start_newline] are called, undos are not possible
   until more content is added to the buffer.

   [contents] will also call [flush], but will otherwise not mutate the buffer
*)
type t

(* Checks if the current working line has had content added to it *)
val is_at_start_of_line : t -> bool

(* Clears all content that has been added since the last call to [flush], [start_newline],
   [clear_working_line], or [contents] *)
val clear_working_line : t -> unit

(* Undoes the last [append] and returns the value. Will throw an error if there's nothing
   left to undo. Number of possible undos is the number of times [append] has been called
   since the last call to [flush], [start_newline], [clear_working_line], or [contents]
*)
val undo_and_return : t -> string

(* Same as above but ignores the values *)
val undo_and_ignore : t -> unit

(* Returns the current working line as a string *)
val get_current_working_line : t -> string

(* Appends the current working line to the contents *)
val flush : t -> unit

(* Appends input string to the current working line *)
val append : t -> string -> unit

(* Calls [flush] and appends a newline character to the committed content *)
val start_newline : t -> unit

(* Increases the indent level *)
val indent : t -> unit

(* Decreases the airdrop level *)
val dedent : t -> unit

(* Creates a print buffer *)
val create : ?indent_size:int -> int -> t

(* Calls [flush] and then prints out the committed contents *)
val contents : t -> string
