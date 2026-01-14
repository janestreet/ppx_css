open! Core
open Ppx_css

(* We need to format the error message from these exceptions in a specific way so that the
   (css ()) stanzas in the jbuild direct the user to the appropriate file whenever errors
   are thrown
*)
let format_message { Css_parser_common.Errors.start_pos; end_pos; message } =
  let filename = start_pos.pos_fname in
  let start_line_num = start_pos.pos_lnum in
  let end_line_num = end_pos.pos_lnum in
  let start_col_num = start_pos.pos_cnum - start_pos.pos_bol in
  let end_col_num = end_pos.pos_cnum - end_pos.pos_bol in
  let is_multiline_error = end_line_num - start_line_num > 0 in
  let line_string =
    match is_multiline_error with
    | true -> [%string "lines %{start_line_num#Int}-%{end_line_num#Int}"]
    | false -> [%string "line %{start_line_num#Int}"]
  in
  let file_pos_string =
    [%string
      {|File "%{filename}", %{line_string}, characters %{start_col_num#Int}-%{end_col_num#Int}:|}]
  in
  file_pos_string ^ "\n" ^ message
;;

let create_file filename ~f =
  let buffer = Buffer.create 64 in
  f buffer;
  Filesystem_core.write_file filename ~contents:(Buffer.contents buffer)
;;

let standalone ~serializable_options ~src =
  let css_string = Core.In_channel.read_all src in
  let start_pos = { Lexing.pos_fname = src; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } in
  (* We have to assign a filename and a starting [pos_lnum] of 1, otherwise the css
     inliner will not report the filename nor the proper line number *)
  let start_loc =
    { Ppxlib.Location.loc_start = start_pos; loc_end = start_pos; loc_ghost = true }
  in
  let options =
    Ppx_css_syntax.Serializable_options.to_stylesheet_options
      ~css_string:{ css_string; string_loc = start_loc; delimiter = None }
      serializable_options
  in
  let name = Filename.chop_extension src in
  let generated_name = name ^ "__generated" in
  let out_ml = File_path.of_string (generated_name ^ ".ml") in
  let out_mli = File_path.of_string (generated_name ^ ".mli") in
  let alias_ml = File_path.of_string (name ^ ".ml") in
  let alias_mli = File_path.of_string (name ^ ".mli") in
  let%tydi { ml_file; css_string_for_testing = _ } =
    For_css_inliner.gen_struct
      ~dont_hash:options.dont_hash
      ~css_string:options.css_string.css_string
      ~dont_hash_prefixes:options.dont_hash_prefixes
      ~stylesheet_location:start_loc
      ~lazy_loading_optimization:options.lazy_loading_optimization
      ~disable_hashing:false
  in
  let mli_file = For_css_inliner.gen_sig ~stylesheet_location:start_loc css_string in
  create_file out_ml ~f:(fun w ->
    Buffer.add_string w "include ";
    Buffer.add_string w ml_file;
    Buffer.add_string w " end\n");
  create_file out_mli ~f:(fun w ->
    Buffer.add_string w "include ";
    Buffer.add_string w mli_file;
    Buffer.add_string w " end\n");
  create_file alias_ml ~f:(fun w ->
    Buffer.add_string w ("include " ^ String.capitalize generated_name ^ "\n"));
  create_file alias_mli ~f:(fun w ->
    Buffer.add_string
      w
      ("include module type of " ^ String.capitalize generated_name ^ "\n"))
;;

let command =
  Command.basic_or_error
    ~summary:"Generate ml and mli files for a css file."
    ~readme:(fun () ->
      {|
Generates ocaml code defining string constants constaining the hashed or referenced
classnames in addition to a call to [Inline_css.Private.append] that resgisters the css
for said classes.
|})
    (let open Command.Let_syntax in
     let%map_open src = anon ("CSS_FILE" %: string)
     and serializable_options =
       anon
         (Command.Anons.map_anons
            ("OPTIONS" %: sexp)
            ~f:Ppx_css_syntax.Serializable_options.t_of_sexp)
     in
     fun () ->
       try Ok (standalone ~serializable_options ~src) with
       | Css_parser_common.Errors.Parse_error e
       | Css_parser_common.Errors.Lexing_error e
       | Css_parser_common.Errors.Unknown_error e ->
         Error (format_message e |> Error.of_string)
       | exn ->
         (* Return the error if it doesn't match any of the parser errors *)
         Error (Error.of_exn exn))
;;

let () = Command_unix.run command
