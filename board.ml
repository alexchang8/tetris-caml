open Tetronimo
open Graphics
open Images

type cell = Empty | Filled of Graphics.color

type t = {active_tet : Tetronimo.t; swap: Tetronimo.t; matrix: (cell array) array}

let draw_cell (x, y) =
  let x_c = x * 30 + 201 and
  y_c = 621 - (y * 30) in
  fill_rect x_c y_c 28 28

let draw_tetronimo b (t:Tetronimo.t) =
  t |> get_color |> set_color;
  let t_locs = Tetronimo.locs t in
  let _ = List.map (fun (x,y) -> Array.set (b.matrix.(y)) x (Filled(get_color t))) t_locs in 
  let _ = List.map (fun coords -> draw_cell coords) t_locs in ()

(*TODO: abstract similarities w/ draw tet*)
let erase_tetronimo b t =
  set_color black;
  let t_locs = Tetronimo.locs t in
  let _ = List.map (fun (x,y) -> Array.set (b.matrix.(y)) x Empty) t_locs in 
  let _ = List.map (fun coords -> draw_cell coords) t_locs in ()

let coord_conflict matrix (x,y) = 
  if x > 9 || x < 0 then true
  else if y > 19 || y < 0 then true
  else match matrix.(y).(x) with
    | Empty -> false
    | Filled(_) -> true 

let collides board t = 
  Tetronimo.locs t |> 
  List.fold_left (fun acc coords -> acc || coord_conflict board.matrix coords) false

let num_to_piece = function
  | 0 -> Tetronimo.I
  | 1 -> Tetronimo.O
  | 2 -> Tetronimo.T
  | 3 -> Tetronimo.S
  | 4 -> Tetronimo.Z
  | 5 -> Tetronimo.J
  | 6 -> Tetronimo.L
  | _ -> failwith "invalid num"

let random_tetronimo (x:unit) : Tetronimo.t = 
  Random.int 7 |> num_to_piece |> Tetronimo.new_piece

let rec drop_helper board t =
  if collides board t then t else drop_helper board (Tetronimo.m_down 1 t)

let board_new_piece board = 
  let tetr = random_tetronimo () in 
  let x = {active_tet = tetr;
           swap = board.swap;
           matrix = board.matrix} in
  let _ = draw_tetronimo x tetr in
  x
let hard_drop board =
  erase_tetronimo board board.active_tet;
  drop_helper board board.active_tet |> Tetronimo.m_down (-1) |> draw_tetronimo board;
  board_new_piece board

(*Board dimensions (x, y) are 10 by 20*)
let init (_:unit) =
  open_graph "";
  set_window_title "tetris-caml";
  resize_window 700 700;
  let img = Png.load "bg.png" [] in
  let g = Graphic_image.of_image img in
  draw_image g 0 0;
  fill_rect 200 50 300 600;
  Random.self_init ();
  let tetr = random_tetronimo () in
  let x = {active_tet = tetr;
           swap = random_tetronimo ();
           matrix = Array.make_matrix 20 10 Empty} in
  let _ = draw_tetronimo x tetr in 
  x

let delete_filled b = failwith "unimplemented"

let change_active_tet board tetr =
  {active_tet = tetr;
   swap = board.swap;
   matrix = board.matrix}

let soft_drop board =
  erase_tetronimo board board.active_tet;
  let down1 = board.active_tet |> Tetronimo.m_down 1 in
  if collides board down1
  then (draw_tetronimo board board.active_tet; 
        board_new_piece board)
  else (draw_tetronimo board down1; 
        change_active_tet board down1)

let transform board f =
  erase_tetronimo board board.active_tet;
  let tet' = board.active_tet |> f in
  if collides board tet'
  then soft_drop board
  else (draw_tetronimo board tet'; 
        change_active_tet board tet')

let move_board_right board = transform board Tetronimo.m_right

let move_board_left board = transform board Tetronimo.m_left

let board_rotate board r = transform board (Tetronimo.rotate r)

type action = Rotate of bool | HardDrop | Swap | Translate of bool | NoAction

let update (b:t) (a:action) : t = 
  (*TODO: check rows to delete*)
  (*TODO: show swap block*)
  (* TODO: stop swap abuse*)
  match a with
  | HardDrop -> hard_drop b
  | Swap -> erase_tetronimo b b.active_tet;
    draw_tetronimo b b.swap;
    {active_tet = b.swap; swap = get_piece b.active_tet |> new_piece; matrix = b.matrix}
  | NoAction -> soft_drop b
  | Translate(r) -> if r then move_board_right b else move_board_left b 
  | Rotate(r) -> board_rotate b r