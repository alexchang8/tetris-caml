open Graphics
type piece = I | O | T | S | Z | J | L

type t = {matrix: (bool list) list; orient: int; piece: piece; x_off: int; y_off: int}

let ipiece = 
  [
    [[false;false;false;false];
     [true;true;true;true];
     [false;false;false;false];
     [false;false;false;false]];
    [[false;false;true;false];
     [false;false;true;false];
     [false;false;true;false];
     [false;false;true;false]];
    [[false;false;false;false];
     [false;false;false;false];
     [true;true;true;true];
     [false;false;false;false]];
    [[false;true;false;false];
     [false;true;false;false];
     [false;true;false;false];
     [false;true;false;false]]
  ]

let opiece = 
  [
    [[false;true;true;false];
     [false;true;true;false];
     [false;false;false;false]];
    [[false;true;true;false];
     [false;true;true;false];
     [false;false;false;false]];
    [[false;true;true;false];
     [false;true;true;false];
     [false;false;false;false]];
    [[false;true;true;false];
     [false;true;true;false];
     [false;false;false;false]]
  ]

let tpiece =
  [
    [[false;true;false];
     [true;true;true];
     [false;false;false]];
    [[false;true;false];
     [false;true;true];
     [false;true;false]];
    [[false;false;false];
     [true;true;true];
     [false;true;false]];
    [[false;true;false];
     [true;true;false];
     [false;true;false]]
  ]

let spiece =
  [
    [[false;true;true];
     [true;true;false];
     [false;false;false]];
    [[false;true;false];
     [false;true;true];
     [false;false;true]];
    [[false;false;false];
     [false;true;true];
     [true;true;false]];
    [[true;false;false];
     [true;true;false];
     [false;true;false]]
  ]

let zpiece =
  [
    [[true;true;false];
     [false;true;true];
     [false;false;false]];
    [[false;false;true];
     [false;true;true];
     [false;true;false]];
    [[false;false;false];
     [true;true;false];
     [false;true;true]];
    [[false;true;false];
     [true;true;false];
     [true;false;false]]
  ]

let jpiece =
  [
    [[true;false;false];
     [true;true;true];
     [false;false;false]];
    [[false;true;true];
     [false;true;false];
     [false;true;false]];
    [[false;false;false];
     [true;true;true];
     [false;false;true]];
    [[false;true;false];
     [false;true;false];
     [true;true;false]]
  ]

let lpiece =
  [
    [[false;false;true];
     [true;true;true];
     [false;false;false]];
    [[false;true;false];
     [false;true;false];
     [false;true;true]];
    [[false;false;false];
     [true;true;true];
     [true;false;false]];
    [[true;true;false];
     [false;true;false];
     [false;true;false]]
  ]

let get_piece t = t.piece

let get_color t = 
  match t.piece with
  | I -> cyan
  | O -> yellow
  | T -> magenta
  | S -> rgb 191 255 0
  | Z -> red
  | J -> blue
  | L -> rgb 253 106 2

let get_rot_matrix = function
  | I -> ipiece
  | O -> opiece
  | T -> tpiece
  | S -> spiece
  | Z -> zpiece
  | J -> jpiece
  | L -> lpiece

let new_piece p =
  {matrix = List.hd (get_rot_matrix p);
   orient = 0; piece = p;
   x_off = 3; y_off = 0} (*should be changed to match offset of board*)

let wrap n =
  if n < 0 then 3
  else if n > 3 then 0
  else n

let rotate lr t = 
  let orient = (if lr = true then 1 + t.orient else t.orient - 1) |> wrap in
  {matrix = List.nth (get_rot_matrix t.piece) orient;
   orient = orient; piece = t.piece;
   x_off = t.x_off; y_off = t.y_off}

let rec fold_lefti op acc i = function
  | [] -> acc
  | h::t -> fold_lefti op (op i acc h) (i + 1) t

let row_locs (y:int) (coord_list:(int*int) list) (row: bool list) =
  fold_lefti (fun x acc cell -> if cell then (x,y)::acc else acc) [] 0 row @ coord_list

let locs t = fold_lefti row_locs [] 0 t.matrix |> 
             List.map (fun (x,y) -> (x + t.x_off, y+t.y_off))

(**Returns a copy of the matrix except offset replaced by x and y*)
let change_off t x y =
  {matrix = t.matrix; orient = t.orient; piece = t.piece; x_off = x; y_off = y}

let m_right t = change_off t (t.x_off + 1) t.y_off

let m_left t = change_off t (t.x_off - 1) t.y_off

let m_down n t = change_off t t.x_off (t.y_off + n)