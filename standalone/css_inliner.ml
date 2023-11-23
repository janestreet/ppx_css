open! Core
open Ppx_css
open Async

let standalone ~serializable_options ~src =
  let css_string = Core.In_channel.read_all src in
  let options =
    Ppx_css_syntax.Serializable_options.to_stylesheet_options
      ~css_string:{ css_string; string_loc = Ppxlib.Location.none; delimiter = None }
      serializable_options
  in
  let name = Filename.chop_extension src in
  let generated_name = name ^ "__generated" in
  let out_ml = generated_name ^ ".ml" in
  let out_mli = generated_name ^ ".mli" in
  let alias_ml = name ^ ".ml" in
  let alias_mli = name ^ ".mli" in
  let%bind () =
    Writer.with_file out_ml ~f:(fun w ->
      Writer.write w "include ";
      Writer.write
        w
        (For_css_inliner.gen_struct
           ~rewrite:options.rewrite
           ~css_string:options.css_string.css_string
           ~dont_hash_prefixes:options.dont_hash_prefixes
           ~stylesheet_location:options.css_string.string_loc);
      Writer.write w " end";
      Writer.close w)
  and () =
    Writer.with_file out_mli ~f:(fun w ->
      Writer.write w "include ";
      Writer.write w (For_css_inliner.gen_sig css_string);
      Writer.write w " end";
      Writer.close w)
  and () =
    Writer.with_file alias_ml ~f:(fun w ->
      Writer.write_line w ("include " ^ String.capitalize generated_name);
      Writer.close w)
  and () =
    Writer.with_file alias_mli ~f:(fun w ->
      Writer.write_line w ("include module type of " ^ String.capitalize generated_name);
      Writer.close w)
  in
  return ()
;;

let command =
  Command.async
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
     fun () -> standalone ~serializable_options ~src)
    ~behave_nicely_in_pipeline:false
;;

let () = Command_unix.run command
