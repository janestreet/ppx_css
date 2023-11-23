open! Core

module Vdom = struct
  module Attr = struct
    type t = unit

    let __css_vars_no_kebabs _ = ()
    let combine () () = ()
    let class_ _ = ()
  end
end

module Css_gen = struct
  module Color = struct
    let to_string_css _ = "beep boop I am a test stub."
  end
end

module Inline_css = struct
  module Private = struct
    let append _ = ()
  end
end
