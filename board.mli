open Tetronimo

type cell = Empty | Filled of Graphics.color
type t = {active_tet : Tetronimo.t; swap: Tetronimo.t; matrix: (cell array) array}

type action = Rotate of bool | HardDrop | Swap | Translate of bool | NoAction 

val update: t -> action -> int -> t

val init: unit -> t

val lost_game: t -> bool

val check_clear_lines: t -> t