open Tetronimo

type t 

type action = 
    Rotate of bool | 
    HardDrop | 
    Swap | 
    Translate of bool | 
    NoAction | 
    FastDrop 

val update: t -> action -> int -> t

val init: unit -> t

val lost_game: t -> bool