open Graphics_js

type cell = Empty | Filled of Graphics_js.color

let dark_grey = rgb 45 45 45

let px_cell (x, y) f =
  let x_c = x * 30 + 202 and
  y_c = 652 - (y * 30) in
  if y = 0 then f x_c y_c 28 14
  else f x_c y_c 28 28

(*Becuse pixels are slightly off in canvas, we need a separate function for this*)
let px_cell_line (x, y) f =
  let x_c = x * 30 + 201 and
  y_c = 651 - (y * 30) in
  if y = 0 then f x_c y_c 30 15
  else f x_c y_c 30 30

let draw_cell coords =
  px_cell coords Graphics_js.fill_rect

let highlight_cell coords =
  px_cell_line coords Graphics_js.draw_rect

let do_highlight c locs = 
  set_color c;
  let _ = List.map (fun coords -> highlight_cell coords) locs in () 

let paint_tetronimo t color =
  set_color color;
  let t_locs = Tetronimo.locs t in
  let _ = List.map (fun coords -> draw_cell coords) t_locs in () 

let draw_tetronimo (t:Tetronimo.t) =
  let c = Tetronimo.get_color t in
  paint_tetronimo t c

let erase_tetronimo t =
  paint_tetronimo t dark_grey

let strong_erase_highlight locs =
  do_highlight black locs;
  do_highlight black locs;
  do_highlight black locs;
  do_highlight black locs;
  do_highlight black locs

let highlight t =
  Tetronimo.locs t |> do_highlight white

let erase_highlight t =
  Tetronimo.locs t |> strong_erase_highlight

let update_highlights t t' =  
  let old_locs = Tetronimo.locs t and
  new_locs = Tetronimo.locs t' in
  let f_locs = List.filter (fun x -> not (List.mem x new_locs)) old_locs in
  strong_erase_highlight f_locs;
  new_locs |> do_highlight white

let redraw_row y row = Array.iteri 
    (fun x cell -> match cell with 
       | Empty -> set_color dark_grey; draw_cell (x, y)
       | Filled(c) -> set_color c; draw_cell (x,y)) row

let redraw_matrix = Array.iteri redraw_row

let draw_init matrix tet =
  set_color Graphics_js.black;
  fill_rect 200 50 302 615;
  redraw_matrix matrix;
  draw_tetronimo tet
