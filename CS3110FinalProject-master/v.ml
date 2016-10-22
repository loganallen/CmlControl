module Html = Dom_html
let document = Html.window##document
let js = Js.string

type image = {
  src: string;
}

type that = {
  (* context: canvas##getContext (Html._2d_); *)
  width: int;
  height: int;
  image: image;
}

let create_canvas () =
let canvas = Html.createCanvas document in
canvas##width <- 500;
canvas##height <- 500;
canvas

let create_image src =
let image = Html.createImg document in
image##src <- (Js.string src);
image

let canvas = create_canvas ()
let img = create_image "coin-sprite-animation.png"

let () =
let body =
    Js.Opt.get (document##getElementById(js"body"))
      (fun () -> assert false) in
  Dom.appendChild body canvas;
  Dom.appendChild body img;
  Html.window##alert (Js.string "hello, world")
