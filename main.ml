open Unix
open Board
open Lwt_js
open Dom_html

(*ocamlbuild -pkg js_of_ocaml-lwt.Graphics_js -pkg unix -pkg js_of_ocaml -pkg js_of_ocaml-ppx main.byte*)
(*let flush_kp () = while Graphics_js.key_pressed () do
    let _ = Graphics_js.read_key ()
    in ()
  done

  let get_input () : Board.action =
  if not (Graphics_js.key_pressed ()) then Board.NoAction
  else let k = Graphics_js.read_key () in
    match k with
    (*| ' ' -> Board.HardDrop
      | 'z' -> Board.Rotate(false)
      | 'x' -> Board.Rotate(true)
      | 'c' -> Board.Swap
      | 'n' -> Board.Translate(false)
      | 'm' -> Board.Translate(true)
      | 'b' -> Board.FastDrop*)
    | _ -> Board.NoAction*)


let input = ref (Board.NoAction)

let state = ref (Lwt.task ())

let wait () = fst !state

let rec main board_state frame_time frame_count =
  (* Display state here. *)
  let next_state = Board.update board_state (!input) frame_count in
  Lwt.bind (wait ()) (fun () -> 
      if Board.lost_game next_state then begin
        let init_board = Board.init () in
        let start_time = (Unix.gettimeofday  ()) in
        main init_board start_time 0 
      end
      else
        let next_frame_count = if frame_count > 30 then 0 else frame_count + 1 in
        (* Advance state by one frame here. *)
        (* If less than 25ms have passed, delay until they have. *)
        let diff = frame_time +. 0.025 -. Unix.gettimeofday () in
        let sleep_duration = if diff < 0. then 0. else diff in
        main next_state (Unix.gettimeofday  ()) next_frame_count
    )
let _ =   Js.Opt.iter
    (CoerceTo.canvas (getElementById "canvas2"))
    Graphics_js.open_canvas 

let c1 =
  Html.addEventListener
    Html.document
    Html.Event.mousemove
    (Dom_html.handler (fun ev ->
         let x = ev##.clientX and y = ev##.clientY in
         let dx = x - !mx and dy = y - !my in
         if dy != 0
         then m := matrix_mul (yz_rotation (2. *. float dy /. float width)) !m;
         if dx != 0
         then m := matrix_mul (xz_rotation (2. *. float dx /. float width)) !m;
         mx := x;
         my := y;
         Js._true ))
    Js._true

let rec start () =
  let t = Lwt.task () in
  let _, w = !state in
  state := t;
  Lwt.wakeup w ();
  Lwt.bind (Lwt_js.sleep (1. /. 40.)) start

let init =
  let init_board = Board.init () in
  let start_time = (Unix.gettimeofday  ()) in
  main init_board start_time 0 

let _ = start ()
