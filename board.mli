open Tetronimo

type t

type action = Rotate of bool | HardDrop | Swap | Translate of bool | NoAction 

val update: t -> action -> int -> t

val init: unit -> t