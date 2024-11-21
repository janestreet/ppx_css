open! Core

module Private = struct
  module Stylesheet = struct
    let create_stylesheet () = ()
    let create_stylesheet_exn () = ()
    let update_stylesheet _ _ = ()
  end

  let append _ = ()
  let append_but_do_not_update _ = ()
  let update_stylesheet _ _ = ()
end
