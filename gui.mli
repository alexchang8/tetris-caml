type cell = Empty | Filled of Graphics_js.color

val erase_tetronimo: Tetronimo.t -> unit

val draw_tetronimo: Tetronimo.t -> unit

val update_highlights: Tetronimo.t -> Tetronimo.t -> unit

val erase_highlight: Tetronimo.t -> unit

val redraw_matrix: cell array array -> unit

val draw_init: cell array array -> Tetronimo.t -> unit
