module Html = Dom_html
let document = Html.window##document
let js = Js.string

type image = {
  src: string;
}

type option = {
  context: Html.canvasRenderingContext2D Js.t;
  width: float;
  height: float;
  image: Html.imageElement Js.t;
}

let sprite options =
  let that = {
    context = options.context;
    width = options.width;
    height = options.height;
    image = options.image;
  } in
  that

let render sprite =
  sprite.context##drawImage_full(sprite.image,0.0, 0.0,sprite.width,sprite.height,0.0,0.0,sprite.width,sprite.height)

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
let coinImg = create_image "coin-sprite-animation.png"

let coin = sprite{
  context=canvas##getContext (Html._2d_);
  width=100.0;
  height= 100.0;
  image=coinImg;
} in
render coin

let () =
let body =
    Js.Opt.get (document##getElementById(js"body"))
      (fun () -> assert false) in
  Dom.appendChild body canvas;
