open! Core

type t =
  { handle_recoverable_error : Recoverable_error.Handler.t
  ; partial_parsing_behavior : Partial_parsing_behavior.t
  }

let raise_on_recoverable_errors : t =
  let handle_recoverable_error = function
    | Recoverable_error.Missing_semicolon_at_end_of_declaration_list
        { loc = { loc_start; loc_end; loc_ghost = _ } } ->
      raise
        (Css_parser_common.Errors.Parse_error
           { start_pos = loc_start
           ; end_pos = loc_end
           ; message = "Declaration must end with a semicolon."
           })
  in
  { handle_recoverable_error
  ; partial_parsing_behavior = Partial_parsing_behavior.No_partial_parsing
  }
;;

let ignore_recoverable_errors : t =
  let handle_recoverable_error _ = () in
  { handle_recoverable_error
  ; partial_parsing_behavior = Partial_parsing_behavior.No_partial_parsing
  }
;;
