open Tetronimo
type t

val draw_tetronimo: Tetronimo.t -> unit list

val init: unit -> unit

val draw_cell: (int * int) -> unit