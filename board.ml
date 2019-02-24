open Tetronimo
open Graphics
open Images

(*Right now doing a jank solution by pixel testing to check for collisions so we
  have no separate board state.*)
type t = {active_tet : Tetronimo.t}

(*Board dimensions (x, y) are 10 by 20*)
let init x =
  open_graph "";
  set_window_title "tetris-caml";
  resize_window 700 700;
  let img = Png.load "bg.png" [] in
  let g = Graphic_image.of_image img in
  draw_image g 0 0;
  fill_rect 200 50 300 600

let draw_cell (x, y) =
  let x_c = x * 30 + 200 and
  y_c = 620 - (y * 30) in
  fill_rect x_c y_c 30 30

let draw_tetronimo (t:Tetronimo.t) =
  t |> get_color |> set_color;
  List.map (fun coords -> draw_cell coords) (Tetronimo.locs t)

let erase_tetronimo t =
  set_color black;
  let _ = List.map (fun coords -> draw_cell coords) (Tetronimo.locs t) in ()

let collides t = 
  Tetronimo.locs t |> 
  List.map (fun (x,y) -> point_color (215 + 30*x) (635 - (30 *y)) <> black) |>
  List.fold_left (fun acc e -> acc || e) false

let rec drop_helper t =
  if collides t then t else drop_helper (Tetronimo.m_down 1 t)

let hard_drop t =
  erase_tetronimo t;
  drop_helper t |> Tetronimo.m_down (-1) |> draw_tetronimo
