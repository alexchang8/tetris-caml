open Tetronimo
open Graphics
open Images

type cell = Empty | Filled of Graphics.color

type t = {active_tet : Tetronimo.t; 
          swap: Tetronimo.t; 
          matrix: (cell array) array; 
          queue: Tetronimo.piece list}

let dark_grey = rgb 45 45 45

let px_cell (x, y) f =
  let x_c = x * 30 + 201 and
  y_c = 651 - (y * 30) in
  if y = 0 then f x_c y_c 28 14
  else f x_c y_c 28 28

let draw_cell coords =
  px_cell coords Graphics.fill_rect

let highlight_cell coords =
  px_cell coords Graphics.draw_rect

let paint_tetronimo t color =
  set_color color;
  let t_locs = Tetronimo.locs t in
  let _ = List.map (fun coords -> draw_cell coords) t_locs in () 

let draw_tetronimo (t:Tetronimo.t) =
  let c = get_color t in
  let _ = paint_tetronimo t c in
  Graphics.auto_synchronize true

let erase_tetronimo t =
  Graphics.auto_synchronize false;
  paint_tetronimo t dark_grey

let add_tetronimo_to_matrix b t =
  let c = get_color t in
  let _ = List.map (fun (x,y) -> Array.set (b.matrix.(y)) x (Filled(c))) (Tetronimo.locs t) in () 

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

let rec drop_helper board t =
  if collides board t then t else drop_helper board (Tetronimo.m_down 1 t)

let shuffle d =
  d |> List.map (fun c -> (Random.bits (), c)) |> List.sort compare |> List.map snd 

let rand_tet_list () = 
  shuffle [Tetronimo.I; Tetronimo.O; Tetronimo.T; 
           Tetronimo.S; Tetronimo.Z; Tetronimo.J;
           Tetronimo.L]

let board_new_piece board =
  match board.queue with
  | [] -> let new_q = rand_tet_list () in 
    {active_tet = List.hd new_q |> Tetronimo.new_piece;
     swap = board.swap;
     matrix = board.matrix;
     queue = List.tl new_q}
  | h::t ->
    {active_tet = h |> Tetronimo.new_piece;
     swap = board.swap;
     matrix = board.matrix;
     queue = List.tl board.queue}

let hard_drop board =
  erase_tetronimo board.active_tet;
  let t' = drop_helper board board.active_tet |> Tetronimo.m_down (-1) in
  draw_tetronimo t';
  add_tetronimo_to_matrix board t';
  board_new_piece board

let redraw_row y row = Array.iteri 
    (fun x cell -> match cell with 
       | Empty -> set_color dark_grey; draw_cell (x, y)
       | Filled(c) -> set_color c; draw_cell (x,y)) row

let redraw_matrix = Array.iteri redraw_row

let random_tetronimo (x:unit) : Tetronimo.t = 
  Random.int 7 |> num_to_piece |> Tetronimo.new_piece

(*Board dimensions (x, y) are 10 by 20*)
let init () =
  open_graph "";
  set_window_title "tetris-caml";
  resize_window 700 700;
  (*load background image*)
  let img = Png.load "bg.png" [] in
  let g = Graphic_image.of_image img in
  draw_image g 0 0;

  set_color Graphics.black;
  fill_rect 200 50 300 615;
  Random.self_init ();
  let new_q = rand_tet_list () in
  let tetr = List.hd new_q |> Tetronimo.new_piece in
  let matrix = Array.make_matrix 21 10 Empty in
  draw_tetronimo tetr; 
  redraw_matrix matrix;
  {active_tet = tetr;
   swap = random_tetronimo ();
   matrix = matrix;
   queue = List.tl new_q}

let is_filled_cell  = function 
  | Filled(_) -> true
  | Empty -> false

let lost_game board = Array.exists is_filled_cell board.matrix.(0)

let change_active_tet board tetr =
  {active_tet = tetr;
   swap = board.swap;
   matrix = board.matrix;
   queue = board.queue}

let soft_drop board frame_count fast =
  let hot_frame = if fast then true else frame_count > 18 in
  if hot_frame then begin
    erase_tetronimo board.active_tet;
    let down1 = board.active_tet |> Tetronimo.m_down 1 in
    if collides board down1
    then (draw_tetronimo board.active_tet; 
          add_tetronimo_to_matrix board board.active_tet;
          board_new_piece board)
    else (draw_tetronimo down1; 
          change_active_tet board down1)
  end
  else board

let transform board f frame_count =
  erase_tetronimo  board.active_tet;
  let tet' = board.active_tet |> f in
  if collides board tet'
  then soft_drop board frame_count false
  else (draw_tetronimo tet'; 
        change_active_tet board tet')

let move_board_right board = transform board Tetronimo.m_right

let move_board_left board = transform board Tetronimo.m_left

let board_rotate board r = transform board (Tetronimo.rotate r)

type action = Rotate of bool | HardDrop | Swap | Translate of bool | NoAction | FastDrop

let not_full row = not (Array.for_all is_filled_cell row)

let rec add_empty_rows list_matrix = 
  if List.length list_matrix < 21 then
    add_empty_rows (Array.make 10 Empty :: list_matrix)
  else list_matrix

let check_clear_lines b =
  let filtered_full = Array.to_list b.matrix |> List.filter not_full in
  if List.length filtered_full < 21 then begin
    let new_matrix = filtered_full |> add_empty_rows |> Array.of_list in
    redraw_matrix new_matrix;
    draw_tetronimo b.active_tet;
    {active_tet = b.active_tet; swap = b.swap; matrix = new_matrix; queue = b.queue}
  end
  else b

let do_highlight board c = 
  set_color c;
  let t_locs = drop_helper board board.active_tet |>
               Tetronimo.m_down (-1) |> Tetronimo.locs in
  let _ = List.map (fun coords -> highlight_cell coords) t_locs in () 

let highlight_tetronimo board = do_highlight board white

let remove_highlight board = do_highlight board dark_grey

let board_after_action b a frame_count = 
  match a with
  | HardDrop -> hard_drop b
  | Swap -> erase_tetronimo b.active_tet;
    draw_tetronimo b.swap;
    {active_tet = b.swap; swap = get_piece b.active_tet |> new_piece; matrix = b.matrix; queue = b.queue}
  | NoAction -> soft_drop b frame_count false 
  | Translate(r) -> if r then move_board_right b frame_count else move_board_left b frame_count
  | Rotate(r) -> board_rotate b r frame_count
  | FastDrop -> soft_drop b frame_count true

let update (b:t) (a:action) frame_count : t = 
  (*TODO: show swap block*)
  (* TODO: stop swap abuse*)
  remove_highlight b;
  let b' = board_after_action b a frame_count |> check_clear_lines in
  highlight_tetronimo b';
  b'