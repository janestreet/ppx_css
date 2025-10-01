open! Core
open Virtual_dom

(* NOTE: This file is used for testing syntax highlighting behaviors on all supported
   text editors vim, emacs, and vscode. *)

[@@@warning "-60-32"]

module Stylesheet_syntax =
  [%css
  stylesheet
    {|
      @charset " utf-8";

      .foo,
      div {
        background-color: tomato;
        /* flex and flex-end is interesting because it is both a "property" and an "attribute" so getting the
                 syntax highting to have the right context is important here. */
        flex: flex-end;
      }
    |}]

module Foo = struct
  include String

  let to_string_css s = s
end

let message : string = "foo"

let styled_component_syntax : Virtual_dom.Vdom.Attr.t list =
  [ [%css {|background-color: tomato;|}]
  ; [%css {|background-color: tomato;|}]
  ; (* No whitespace after %css|*)
    {%css|background-color: tomato;|}
  ; (* Whitespace after %css| *)
    {%css|background-color: tomato;|}
  ; (* Semi-colon at the end. *)
    {%css|background-color: tomato;|}
  ; [%css {|background-color: tomato;|}]
  ; [%css
      {|
        background-color: tomato;
        flex: flex-end;

        .foo {
          background-color: tomato;
          flex: flex-end;
          content: %{message};
          content: %{message#Foo};
        }

        flex: flex-end;
      |}]
  ; {%css|
      background-color: tomato;
      flex: flex-end;

      .foo {
        background-color: tomato;
        flex: flex-end;
      }

      flex: flex-end;
    |}
  ; [%css
      {|
        flex: flex-end;
      |}]
  ; {%css|
      flex: flex-end;
    |}
  ; (* Ampersand. *)
    [%css
      {|
        & {
          background-color: tomato;
          flex: flex-end;
          content: %{message#Foo};
        }
      |}]
  ; {%css|
      & {
        background-color: tomato;
        flex: flex-end;
      }
      content: %{message#Foo};
    |}
  ]
;;

let make_icon ~primary_color ~background_color =
  (* This is a regression test for a syntax bug in VScode. *)
  let module Style =
    [%css
    stylesheet
      {|
        .icon-wrapper {
          border-radius: 50%;
          padding: 6px;
          border: 1px solid %{primary_color#Css_gen.Color};
          background-color: %{background_color#Css_gen.Color};
          width: 2.5rem;
          height: 2.5rem;
          display: flex;
          justify-content: center;
          align-items: center;
        }
      |}]
  in
  Vdom.Node.div ~attrs:[ Style.icon_wrapper ] []
;;

module Style =
  (* This test case is a regression test for a highlighting bug where the interpolated
     OCaml broke the syntax highlighting for the rest of the file in vim's syntax
     highlighting. *)
  [%css
  stylesheet
    {|
      .settings_icon {
        background-color: none;
        position: absolute;
        top: 2px;
        right: %{`Px 1#Css_gen.Length};
        /* i am a comment */
        width: 20px;
        height: 20px;
        text-align: center;
        cursor: pointer;
      }
    |}]

let interpolated_ocaml =
  [%css
    {|
      background-color: %{
        match `Red, `Green with
        | `Red, `Green -> Int.to_string 100
      };
    |}]
;;

module I_am_a_module_at_the_end = struct
  let i = "have unbroken highlighting" ^ Int.to_string 200
end
