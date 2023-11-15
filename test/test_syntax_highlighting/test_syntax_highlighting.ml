open! Core

[@@@warning "-60-32"]

module Stylesheet_syntax =
[%css
stylesheet
  {|
   @charset "utf-8";

  .foo, div {
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
  [ [%css {|background-color: tomato |}]
  ; [%css {| background-color: tomato |}]
  ; (* No whitespace after %css|*)
    {%css|background-color: tomato|}
  ; (* Whitespace after %css| *)
    {%css| background-color: tomato|}
  ; (* Semi-colon at the end. *)
    {%css|background-color: tomato;|}
  ; [%css {| background-color: tomato;|}]
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

    flex: flex-end
  |}]
  ; {%css|
     background-color: tomato;
     flex: flex-end;

.foo {
  background-color: tomato;
  flex: flex-end;
}

flex: flex-end
|}
  ; [%css {|flex: flex-end|}]
  ; {%css|flex: flex-end|}
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

module I_am_a_module_at_the_end = struct
  let i = "have unbroken highlighting" ^ Int.to_string 200
end
