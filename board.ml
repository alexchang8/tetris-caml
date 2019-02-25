open Tetronimo
open Graphics
open Images

type cell = Empty | Filled of Graphics.color

type t = {active_tet : Tetronimo.t; swap: Tetronimo.t; matrix: (cell array) array}

let dark_grey = rgb 45 45 45

let draw_cell (x, y) =
  let x_c = x * 30 + 201 and
  y_c = 651 - (y * 30) in
  if y = 0 then fill_rect x_c y_c 28 14
  else fill_rect x_c y_c 28 28

let paint_tetronimo b t color cell =
  set_color color;
  let t_locs = Tetronimo.locs t in
  let _ = List.map (fun (x,y) -> Array.set (b.matrix.(y)) x cell) t_locs in 
  let _ = List.map (fun coords -> draw_cell coords) t_locs in () 

let draw_tetronimo b (t:Tetronimo.t) =
  let c = get_color t in
  let _ = paint_tetronimo b t c (Filled(c)) in
  Graphics.auto_synchronize true

let erase_tetronimo b t =
  Graphics.auto_synchronize false;
  paint_tetronimo b t dark_grey Empty

let coord_conflict matrix (x,y) = 
  if x > 9 || x < 0 then true
  else if y > 20 || y < 0 then true
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

let rec draw_helper_y x y = 
  if y < 21 then begin
    draw_cell (x,y);
    draw_helper_y x (y + 1)
  end
  else ()

let rec draw_helper_x (x:int) : unit = 
  if x < 10 then begin
    draw_helper_y x 0; draw_helper_x (x + 1)
  end
  else ()

let init_cells () = 
  set_color dark_grey;
  draw_helper_x 0

(*Board dimensions (x, y) are 10 by 20*)
let init () =
  open_graph "";
  set_window_title "tetris-caml";
  resize_window 700 700;
  let img = Png.load "bg.png" [] in
  let g = Graphic_image.of_image img in
  draw_image g 0 0;
  set_color Graphics.black;
  fill_rect 200 50 300 615;
  init_cells ();
  Random.self_init ();
  let tetr = random_tetronimo () in
  let x = {active_tet = tetr;
           swap = random_tetronimo ();
           matrix = Array.make_matrix 21 10 Empty} in
  let _ = draw_tetronimo x tetr in 
  x

let delete_filled b = failwith "unimplemented"

let is_filled_cell  = function 
  | Filled(_) -> true
  | Empty -> false

let check_soft_drop board =
  erase_tetronimo board board.active_tet;
  let down1 = board.active_tet |> Tetronimo.m_down 1 in
  if collides board down1 then true
  else (draw_tetronimo board board.active_tet; false)

let lost_game board = 
  Array.exists is_filled_cell board.matrix.(0) && check_soft_drop board

let change_active_tet board tetr =
  {active_tet = tetr;
   swap = board.swap;
   matrix = board.matrix}

let soft_drop board frame_count =
  if frame_count > 9 then begin
    erase_tetronimo board board.active_tet;
    let down1 = board.active_tet |> Tetronimo.m_down 1 in
    if collides board down1
    then (draw_tetronimo board board.active_tet; 
          board_new_piece board)
    else (draw_tetronimo board down1; 
          change_active_tet board down1)
  end
  else board

let transform board f frame_count =
  erase_tetronimo board board.active_tet;
  let tet' = board.active_tet |> f in
  if collides board tet'
  then soft_drop board frame_count
  else (draw_tetronimo board tet'; 
        change_active_tet board tet')

let move_board_right board = transform board Tetronimo.m_right

let move_board_left board = transform board Tetronimo.m_left

let board_rotate board r = transform board (Tetronimo.rotate r)

type action = Rotate of bool | HardDrop | Swap | Translate of bool | NoAction

let update (b:t) (a:action) frame_count : t = 
  (*TODO: check rows to delete*)
  (*TODO: show swap block*)
  (* TODO: stop swap abuse*)
  match a with
  | HardDrop -> hard_drop b
  | Swap -> erase_tetronimo b b.active_tet;
    draw_tetronimo b b.swap;
    {active_tet = b.swap; swap = get_piece b.active_tet |> new_piece; matrix = b.matrix}
  | NoAction -> soft_drop b frame_count 
  | Translate(r) -> if r then move_board_right b frame_count else move_board_left b frame_count
  | Rotate(r) -> board_rotate b r frame_count