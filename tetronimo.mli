open Graphics
(** the abstract type representing a tetronimo*)
type t

(** represents the type of piece*)
type piece = I | O | T | S | Z | J | L

(** Creates a new piece with given type at the top of the board *)
val new_piece : piece -> t

(** Rotates a given tetronimo. Rotates right if true, left if false *)
val rotate : bool -> t -> t

(** Returns a list of tuples representing the current location of the tetronimo
    on the board.*)
val locs : t -> (int * int) list

val m_right: t -> t

val m_left: t -> t

(** [m_down t n] returns a new tetronimo moved down n units*)
val m_down: int -> t -> t

val get_color: t -> Graphics.color