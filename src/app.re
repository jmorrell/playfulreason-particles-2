/* config */

let damping = 0.999;
let numParticles = 2000;

/* externals */

open ReasonJs.Dom;

/*
Work around a type issue in bs-webapi.
https://github.com/BuckleTypes/bs-webapi-incubator/issues/63
*/
external eventToMouseEvent : Dom.event => MouseEvent.t = "%identity";

/* main app state */

type vecT = {
  mutable x: float,
  mutable y: float
};

type particleT = {
  pos: vecT,
  oldPos: vecT
};

type windowT = {
  mutable width: int,
  mutable height: int
};

type mousePositionT = {
  mutable x: int,
  mutable y: int
};

type stateT = {
  mutable particles: array particleT,
  window: windowT,
  mouse: mousePositionT,
};

let state : stateT = {
  particles: [| |],
  window: {
    width: 0,
    height: 0
  },
  mouse: {
    x: 0,
    y: 0
  }
};

/* canvas/context setup */

let width = Window.innerWidth window;
let height = Window.innerHeight window;

state.window.width = width;
state.window.height = height;
state.mouse.x = width / 2;
state.mouse.y = height / 2;

let canvas = Document.createElement "canvas" document;
let ctx = Canvas.getContext canvas "2d";

document
  |> Document.asHtmlDocument
  |> Option.andThen  HtmlDocument.body
  |> Option.map (Element.appendChild canvas);

Element.setAttribute "height" (string_of_int height) canvas;
Element.setAttribute "width" (string_of_int width) canvas;

let onMouseMove e => {
  state.mouse.x = MouseEvent.clientX (eventToMouseEvent e);
  state.mouse.y =  MouseEvent.clientY (eventToMouseEvent e);
};

Element.addEventListener "mousemove" onMouseMove canvas;

/* app code */

let genItems num callback => {
  let emptyArray = Array.make num 0;

  Array.map (fun i => {
    callback i;
  }) emptyArray;
};

state.particles = genItems numParticles (fun _ => {
  let x = (Random.float 1.) *. float_of_int width;
  let y = (Random.float 1.) *. float_of_int height;
  {
    pos: {
      x,
      y
    },
    oldPos: {
      x,
      y
    }
  };
});

let integrateParticle p => {
  let velocityX = (p.pos.x -. p.oldPos.x) *. damping;
  let velocityY = (p.pos.y -. p.oldPos.y) *. damping;
  p.oldPos.x = p.pos.x;
  p.oldPos.y = p.pos.y;
  p.pos.x = p.pos.x +. velocityX;
  p.pos.y = p.pos.y +. velocityY;
  ();
};

let attractParticle mouse p => {
  let dx = (float_of_int mouse.x) -. p.pos.x;
  let dy = (float_of_int mouse.y) -. p.pos.y;
  let distance = sqrt(dx *. dx +. dy *. dy);
  p.pos.x = p.pos.x +. dx /. distance;
  p.pos.y = p.pos.y +. dy /. distance;
  ();
};

let updateParticle mouse p => {
  attractParticle mouse p;
  integrateParticle p;
  ();
};

let update state => {
  Array.iter (updateParticle state.mouse) state.particles;
};

let drawBackground color width height => {
  Canvas.fillStyle ctx color;
  Canvas.fillRect ctx 0 0 width height;
};

let drawParticle p => {
  Canvas.strokeStyle ctx "#ffffff";
  Canvas.lineWidth ctx 2;
  Canvas.beginPath ctx;
  Canvas.moveTo ctx (int_of_float p.oldPos.x) (int_of_float p.oldPos.y);
  Canvas.lineTo ctx (int_of_float p.pos.x) (int_of_float p.pos.y);
  Canvas.stroke ctx;
  ();
};

let draw state => {
  drawBackground "#000000" width height;
  Array.iter drawParticle state.particles;
};

let rec loop () => {
  update state;
  draw state;
  ReasonJs.requestAnimationFrame loop;
};

loop ();
