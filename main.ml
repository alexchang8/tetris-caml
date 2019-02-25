open Unix
open Board
(*ocamlbuild -pkg js_of_ocaml-lwt.graphics -pkg unix -pkg js_of_ocaml -pkg js_of_ocaml-ppx main.byte*)
let flush_kp () = while Graphics.key_pressed () do
    let _ = Graphics.read_key ()
    in ()
  done 

let get_input () : Board.action =
  if not (Graphics.key_pressed ()) then Board.NoAction
  else let k = Graphics.read_key () in
    flush_kp ();
    match k with
    | ' ' -> Board.HardDrop
    | 'z' -> Board.Rotate(false)
    | 'x' -> Board.Rotate(true)
    | 'c' -> Board.Swap
    | 'n' -> Board.Translate(false)
    | 'm' -> Board.Translate(true)
    | _ -> Board.NoAction

let rec main board_state frame_time frame_count =
  (* Display state here. *)
  let next_state = Board.update board_state (get_input ()) frame_count in
  let next_frame_count = if frame_count > 9 then 0 else frame_count + 1 in
  (* Advance state by one frame here. *)
  (* If less than 25ms have passed, delay until they have. *)
  let rec delay () =
    let duration = frame_time +. 0.025 -. Unix.gettimeofday () in
    if duration > 0.0 then
      try
        sleepf duration
      with Unix.Unix_error (Unix.EAGAIN, _, _) -> delay ()
  in delay ();
  main next_state (Unix.gettimeofday  ()) next_frame_count

let init =
  let init_board = Board.init () in
  let start_time = (Unix.gettimeofday  ()) in
  main init_board start_time 0 
