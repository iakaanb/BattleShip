(* lib/grid.mli: Interface for the Battleship grid module *)

(**[cell] is the type of a particular spot in a grid. It can take on different
   values, such as Water, Missed, a Ship, a Hit ship, a Destroyed ship, or
   Hidden depending on the state of the game and the context it is called in.*)
type cell =
  | Water
  | Miss
  | Ship of {
      id : int;
      length : int;
    }
  | Hit of {
      id : int;
      length : int;
    }
    (* destroyed = sunk *)
  | Destroyed of {
      id : int;
      length : int;
    }
  | Hidden (*for printing the opponent's board, the cell type is "hidden"*)

type t = cell array array
(** [t] is the representation type of the grid. *)

val string_of_cell : cell -> string
(** [string_of_cell c] is the string-readable version of a cell in a grid. *)

val coordinates : string -> int * int
(** [coordinates str] is the integer tuple Cartesian coordinates of a string
    indicating a position on the board. Requires that [str] is in the form where
    first character is "A-Z" and second character is a number. *)

(* more time potential fxns: low and high fxn, low outputs a and high outputs
   1. *)

val create_board : int -> t
(** [create_board size] creates a battleship grid of [size] by [size] with all
    cells initialized to Water. *)

val print_grid : t -> unit
(** [print_grid grid] prints a grid to the console with color coding for
    different cell states. *)

val print_their_board : t -> unit
(** [print_their_board grid] prints specifically an opponent's grid to the
    console with color coding for different cell states, and keeps unknown
    information Hidden. *)

val get_ships : int -> int list
(** [get_ships size] takes in the size of the board and determines a list of
    ships that the user must place and what lengths they should be. *)

(* ToDo 3: take in the two coordinates and the grid and return true if all three
   hold and false if otherwise: 1) is not diagonal (either x value has to be the
   same on both or y) 2) does not overlap a pre existing ship 3) doesn't go out
   of bounds. *)

val validate_ship : string -> string -> t -> bool * int
(** [validate_ship coord1 coord2 grid] determines if the given coordinates can
    create a valid ship. Must satisfy the following: 1) is not diagonal 2) does
    not overlap a pre existing ship 3) doesn't go out of bounds If valid, also
    determines the length of the ship that would go there. *)

val hit_ships : string -> int -> t -> (int * int) list
(** [hit_ships coord ship_id grid] checks every cell in the row and column of
    that coordinate for other ships of that ID. (row/col of coordinate). A cell
    will be added to a "hit list" (string list of cells) if 1) it has the same
    ship id 2) is hit. This returns a list of other ship cells of that same ID
    that have been hit. *)

val is_sunk : string -> int -> t -> bool
(** [is_sunk coord ship_id grid] Returns true if length of "hit list" is length
    of ship ID list (number of ships of that ID), false if otherwise. This means
    that the ship that was just hit has sunk the entire ship. *)

val change_to_ship : t -> int -> int -> int * int -> unit
(** [change_to_ship grid ship_id ship_length] changes the state of the cell to
    ship. Used when placing initial ships. Also adds ship id to Ship type when
    initializing ship. Each ship must have different Id.*)

val change_state : t -> string -> unit
(** [change_state grid state] changes the state of the cell. Example: ship ->
    hit, water -> miss.*)

val sink_ship : string -> int -> t -> unit
(**[sink_ship coord ship_id grid] changes the state of all ship cells in [grid]
   with the same ship_id as the ship at [coord] from sx (hit) to ss (sunken
   ship). *)

val set_ships : int list -> t -> unit
(** [set_ships lst grid] asks user to place the ships of the lengths specified
    in [lst], or randomly places the ships for them, and changes the necessary
    cells on [grid] to "ship", a.k.a "so". *)
(* val set_random_ships : int list -> t -> unit (** [set_random_ships lst grid]
   places random ships of the lengths specified in [lst] by changing the
   necessary cells on [grid] to "ship", a.k.a "so". *) *)

val set_random_ships_given_ids : int list -> int list -> t -> unit

val get_length : int list -> int
(** [get_lengths size] finds the amount of ships the user must place.**)

val num_ships : int -> int list
(** [num_ships get_lengths] creates a list of the ship ids. **)

val random_grid : int list -> t -> t
(** [random_grid size grid] creates a random grid with ships placed. **)
