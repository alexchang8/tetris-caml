open Tetronimo

type cell = Empty | Filled of Graphics.color

type t = {active_tet : Tetronimo.t; swap: Tetronimo.t; matrix: (cell array) array}
type action = Rotate of bool | HardDrop | Swap | Translate of bool | NoAction 

val draw_tetronimo: t -> Tetronimo.t -> unit

val init: unit -> t

val draw_cell: (int * int) -> unit

val hard_drop: t -> t

val collides: t -> Tetronimo.t -> bool

val drop_helper: t-> Tetronimo.t -> Tetronimo.t

val erase_tetronimo: t -> Tetronimo.t -> unit

val update: t -> action -> t