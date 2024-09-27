(* lib/grid.ml: Module for Battleship grid representation and utilities *)

(** Abstract Function (AF): The battleship grid is represented as a string list
    list, where each string can be "wo", "wx", "so", "sx", "sxx" corresponding
    to different states of a cell in the game grid. This representation helps
    visualize the game state with colors. *)

(** Representation Invariant (RI): The grid is always a square with dimensions
    between 5 and 26. *)
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
  | Destroyed of {
      id : int;
      length : int;
    }
    (* destroyed = sunk *)
  | Hidden (*for printing the opponent's board, the cell type is "hidden"*)

type t = cell array array

let string_of_cell = function
  | Water -> "wo"
  | Miss -> "wx"
  | Ship _ -> "so" (* Ignoring id and length *)
  | Hit _ -> "sx" (* Ignoring id and length *)
  | Destroyed _ -> "ss" (* Ignoring id and length *)
  | Hidden -> ". "

let create_board size =
  if size < 5 || size > 26 then
    invalid_arg "Board size must be between 5 and 26";
  (* Array.make size (Array.make size Water) *)
  Array.init size (fun _ -> Array.init size (fun _ -> Water))
(* List.init size (fun _ -> List.init size (fun _ -> string_of_cell Water)) *)

(* Function to print the entire grid with row and column labels *)
let print_grid (grid : t) =
  let print_header size =
    ANSITerminal.print_string [ ANSITerminal.on_default ] "    ";
    (* Padding for row numbers alignment *)
    for i = 0 to size - 1 do
      let letter = Char.chr (65 + i) |> String.make 1 in
      (* Convert integer to A, B, C, etc. *)
      ANSITerminal.print_string [ ANSITerminal.on_default ] (letter ^ "   ")
      (* Append two spaces for spacing *)
    done;
    ANSITerminal.print_string [ ANSITerminal.on_default ] "\n"
    (* Move to the next line after headers *)
  in
  Array.iteri
    (fun i row ->
      ANSITerminal.print_string
        [ ANSITerminal.on_default ]
        (if i >= 9 then string_of_int (i + 1) ^ "  "
         else "0" ^ string_of_int (i + 1) ^ "  ");
      (* Print row number with padding *)
      Array.iter
        (fun cell ->
          let color =
            match string_of_cell cell with
            | "wo" -> [ ANSITerminal.blue ]
            | "wx" -> [ ANSITerminal.cyan ]
            | "sx" -> [ ANSITerminal.magenta ]
            | "ss" -> [ ANSITerminal.red ]
            | _ -> [ ANSITerminal.on_default ]
          in
          ANSITerminal.print_string color (string_of_cell cell ^ "  "))
        row;
      (* Print each cell with color and spacing *)
      ANSITerminal.print_string [ ANSITerminal.on_default ] "\n")
    grid;
  (* New line for each row *)
  print_header (Array.length grid)
(* Call to print the column headers at the bottom *)

(* Additional function to print the opponent's view of the board *)
let print_their_board (board : t) =
  let masked_board =
    Array.map
      (Array.map (fun cell ->
           match cell with
           | Hit _ -> cell
           | Miss -> cell
           | Destroyed _ -> cell (* Misses and hits remain visible *)
           | _ -> Hidden (* Hide water and ships not hit *)))
      board
  in
  print_grid masked_board (* Reuse print_grid function *)

(** [coordinates str] "A5" returns a tuple containing the pair "4,0" i.e. (row
    index, col index) (x = 0, y = 4)*)
let coordinates (str : string) =
  let col = String.get str 0 in
  (* This gets the letter *)
  let row = String.sub str 1 (String.length str - 1) in

  (* This gets the number *)

  (* Convert the column letter to a zero-based index: Subtract the ASCII value
     of 'A' from the ASCII value of the letter to get 0 for 'A', 1 for 'B',
     etc. *)
  let x = int_of_char col - int_of_char 'A' in

  (* Convert the row string to an integer and adjust it to be zero-based:
     Subtract 1 from the integer value of the row string ('2' becomes 1, '1'
     becomes 0, etc.). *)
  let y = int_of_string row - 1 in

  (y, x)
(* Return the coordinates as (y, x) to match the grid's row and column
   indexing *)

(*Function takes a coordinate string like 'B2' and returns (1, 1), where 'B' is
  the second column and '2' is the second row, typical zero-based indexing in
  programming. *)

let get_ships size =
  (* Example: For a 5x5 board, return a ship of size 4, two ships of size 3, and
     a ship of size 2 *)
  if size >= 5 && size <= 9 then [ 4; 3; 2 ] (* [ 2; 2 ] *)
  else if size >= 10 && size <= 14 then [ 4; 4; 3; 3; 2; 2 ]
  else if size >= 15 && size <= 18 then [ 5; 5; 4; 3; 3; 3; 2; 2 ]
  else if size >= 19 && size <= 21 then [ 7; 5; 4; 4; 3; 3; 2 ]
  else if size >= 22 && size <= 24 then [ 9; 8; 6; 5; 5; 4; 3; 3; 3; 2; 2 ]
  else if size >= 25 && size <= 26 then
    [ 10; 9; 9; 8; 8; 6; 4; 4; 4; 3; 3; 3; 2 ]
  else [ 2; 3; 4; 5 ]
(* Default case for larger boards *)

let validate_ship_given_index_coords ((c1x, c1y) : int * int)
    ((c2x, c2y) : int * int) (grid : t) =
  try
    if
      c1x < 0
      || c1x >= Array.length grid.(0)
      || c2x < 0
      || c2x >= Array.length grid.(0)
      || c1y < 0
      || c1y >= Array.length grid
      || c2y < 0
      || c2y >= Array.length grid
      || (c1x = c2x && c1y = c2y)
      (*using dummy length*)
    then (false, 0)
    else if c1x = c2x then
      let length = abs (c2y - c1y) + 1 in
      if c2y < c1y then (false, length)
      else
        let tmp = Array.make length Water in
        (*gets the specific location we are looking at*)
        (*COULD BE PLACE FOR EXCEPTION IF OUT OF BOUNDS*)
        let () =
          for x = 0 to length - 1 do
            tmp.(x) <- grid.(c1x).(c1y + x)
          done
        in
        (* returns whether ship valid and calculated length *)
        ( Array.for_all
            (fun x ->
              match x with
              | Ship _ -> false
              | Destroyed _ -> false
              | _ -> true)
            tmp,
          length )
    else if c1y = c2y then
      let length = abs (c2x - c1x) + 1 in
      if c2x < c1x then (false, length)
      else
        let tmp = Array.make length Water in
        (*COULD BE PLACE FOR EXCEPTION IF OUT OF BOUNDS*)
        let () =
          for x = 0 to length - 1 do
            tmp.(x) <- grid.(c1x + x).(c1y)
          done
        in
        ( Array.for_all
            (fun x ->
              match x with
              | Ship _ -> false
              | Destroyed _ -> false
              | _ -> true)
            tmp,
          abs (c2x - c1x) + 1 )
    else (false, 0)
  with Failure _ -> (false, 0)

(*Possible edge case, what if coord1 and coord2 are the same? is ship still
  valid?*)
(* Should always be in the form where coord1 is leftmost/topmost coordinate and
   coord2 is rightmost/lowest coordinate*)
let validate_ship coord1 coord2 (grid : t) =
  try
    let c1x, c1y = coordinates coord1 in
    let c2x, c2y = coordinates coord2 in
    validate_ship_given_index_coords (c1x, c1y) (c2x, c2y) grid
  with Failure _ -> (false, 0)

(*TODO 4 COMPLETED *)
let hit_ships coord ship_id grid =
  (* change_state *)
  let row_index, col_index = coordinates coord in
  let hit_coords = ref [] in

  let check_if_hit cell row_index col_index =
    match cell with
    | Hit { id = curr_id; length = _ } when curr_id = ship_id ->
        if not (List.mem (row_index, col_index) !hit_coords) then
          hit_coords := (row_index, col_index) :: !hit_coords
    | Destroyed { id = curr_id; length = _ } when curr_id = ship_id ->
        if not (List.mem (row_index, col_index) !hit_coords) then
          hit_coords := (row_index, col_index) :: !hit_coords
    | _ -> ()
  in

  (* Function that loops through the row of the given coordinate *)
  let check_row grid =
    Array.iteri
      (fun col_index cell -> check_if_hit cell row_index col_index)
      grid.(row_index)
  in

  (* Function that loops through the column of the given coordinate *)
  let check_col grid =
    Array.iteri
      (fun row_index row -> check_if_hit row.(col_index) row_index col_index)
      grid
  in

  check_row grid;
  check_col grid;

  (* Sort list let result = List.sort compare !hit_coords in *)

  (* Delete duplicates and sort list*)
  List.sort_uniq compare !hit_coords

(*TODO 5 COMPLETED *)
let is_sunk coord ship_id grid =
  let hit_coords = hit_ships coord ship_id grid in
  Array.exists
    (fun row ->
      Array.exists
        (fun cell ->
          match cell with
          | Hit { id = curr_id; length = len_ship }
            when len_ship = List.length hit_coords && curr_id = ship_id -> true
          | _ -> false)
        row)
    grid

(* TODO 7 COMPLETED *)
(*Changes the state of a cell, either upon a player hitting the cell, or upon
  positioning initial ships at the beginning of the game.*)
(*This is more of updating a cell, once a player has tried to hit it.*)
let change_state (grid : t) (coordinate : string) =
  let row, col = coordinates coordinate in
  match grid.(row).(col) with
  | Water -> grid.(row).(col) <- Miss
  | Miss -> grid.(row).(col) <- Miss
  | Ship { id; length } ->
      grid.(row).(col) <- Hit { id; length };
      if is_sunk coordinate id grid = true then
        grid.(row).(col) <- Destroyed { id; length }
      else ()
  | Destroyed _ -> ()
  | Hidden -> ()
  | Hit _ -> ()
(* let row, col = coordinates coordinate in Array.mapi (fun x row_cell -> if x =
   row then Array.mapi (fun y col_cell -> if y = col then state else col_cell)
   row_cell else row_cell) grid *)

(*Changes the state of a cell, either upon a player hitting the cell, or upon
  positioning initial ships at the beginning of the game.*)
(*This is called by sink_ship once a player has sunk a ship, to change the state
  of all ship cells with the same ship_id from sx (hit) to ss (sunken ship).*)
let change_state_to_sunk (grid : t) (coordinate : int * int) =
  let row, col = coordinate in
  match grid.(row).(col) with
  | Hit { id; length } -> grid.(row).(col) <- Destroyed { id; length }
  | _ -> ()

(**[sink_ship coord ship_id grid] changes the state of all ship cells in [grid]
   with the same ship_id as the ship at [coord] from sx (hit) to ss (sunken
   ship). *)
let sink_ship coord ship_id grid =
  let ship_coords = hit_ships coord ship_id grid in
  List.iter (change_state_to_sunk grid) ship_coords

(*Changes the state of a cell SPECIFICALLY TO A SHIP of ship_id [ship_id]. It
  was previously water,for positioning initial ships at the beginning of the
  game.*)
let change_to_ship (grid : t) (ship_id : int) (ship_length : int)
    (index : int * int) =
  (* let row, col = index in Array.mapi (fun x row_cell -> if x = row then
     Array.mapi (fun y col_cell -> if y = col then Ship { id = ship_id; length =
     ship_length } else col_cell) row_cell else row_cell) grid *)
  let row, col = index in
  grid.(row).(col) <- Ship { id = ship_id; length = ship_length }

(*need to verify user input further - what if they put CC or 55. C0 case is
  taken care of in validate ships*)
let rec ask_for_coords (grid : t) : string * string =
  print_endline "\nEnter the top/left coordinate of a new ship. (e.g A5): ";
  let left_coord = read_line () in
  print_endline "Enter the bottom/right coordinate. (e.g C5):";
  let right_coord = read_line () in
  if Array.length grid < 10 then
    if String.length left_coord <> 2 || String.length right_coord <> 2 then
      let () = print_endline "Your input is not valid. Try again. " in
      ask_for_coords grid
    else if fst (validate_ship left_coord right_coord grid) = true then
      (left_coord, right_coord)
    else
      let () = print_endline "Your coordinates are not valid. Try again. " in
      ask_for_coords grid
  else if
    String.length left_coord < 2
    || String.length left_coord > 3
    || String.length left_coord < 2
    || String.length right_coord > 3
  then
    let () = print_endline "Your input is not valid. Try again. " in
    ask_for_coords grid
  else if fst (validate_ship left_coord right_coord grid) = true then
    (left_coord, right_coord)
  else
    let () = print_endline "Your coordinates are not valid. Try again. " in
    ask_for_coords grid

let remove_first_element lst1 =
  match lst1 with
  | [] -> []
  | _ :: t -> t

let rec set_one_ship (ship_length : int) (id : int) (grid : t) =
  let c1, c2 = ask_for_coords grid in
  let is_valid, users_ship_length = validate_ship c1 c2 grid in

  if is_valid = true && users_ship_length = ship_length then
    let c1x, c1y = coordinates c1 in
    let c2x, c2y = coordinates c2 in
    (*getting rid of debugging code i think*)
    (* let () = print_endline ("c1x" ^ string_of_int c1x ^ "c1y" ^ string_of_int
       c1y) in let () = print_endline ("c2x" ^ string_of_int c2x ^ "c2y" ^
       string_of_int c2y) in *)
    if c1x < c2x then
      for x = c1x to c2x do
        (* let () = print_endline (string_of_int x) in *)
        let () = change_to_ship grid id ship_length (x, c1y) in
        ()
      done
    else if c1x > c2x then
      for x = c2x to c1x do
        (* let () = print_endline (string_of_int x) in *)
        let () = change_to_ship grid id ship_length (x, c1y) in
        ()
      done
    else if c1y < c2y then
      for y = c1y to c2y do
        (* let () = print_endline (string_of_int y) in *)
        let () = change_to_ship grid id ship_length (c1x, y) in
        ()
      done
    else if c1y > c2y then
      for y = c2y to c1y do
        let () = change_to_ship grid id ship_length (c1x, y) in
        ()
      done
    else
      (*the ship is length 1 (the 2 coordinates are the same)*)
      let () = change_to_ship grid id ship_length (c1x, c1y) in
      ()
  else
    let () =
      print_endline
        ("The coordinates do not form a valid ship of the required length of "
       ^ string_of_int ship_length)
    in
    set_one_ship ship_length id grid

let rec set_ships_given_ids (ship_lengths : int list) (ship_ids : int list)
    (grid : t) =
  if List.length ship_lengths = 0 then ()
  else
    let () =
      print_endline
        "\n\
         Rules - Ship must be only vertical or horizontal, cannot be placed \
         outside of grid, and cannot overlap another ship."
    in
    let () =
      print_string
        ("\nYou have "
        ^ string_of_int (List.length ship_lengths)
        ^ " ships left to place of length(s): ")
    in
    let () =
      List.iter print_string
        (List.map (fun x -> string_of_int x ^ " ") ship_lengths)
    in

    let first_length = List.hd ship_lengths in
    let id = List.hd ship_ids in
    let () =
      print_string
        ("\nTo place your ship of length " ^ string_of_int first_length ^ ": ")
    in
    let () = set_one_ship first_length id grid in
    let () = print_endline "" in
    let () = print_grid grid in
    let new_ship_lengths = remove_first_element ship_lengths in
    let new_ship_ids = remove_first_element ship_ids in
    set_ships_given_ids new_ship_lengths new_ship_ids grid

let rec set_one_random_ship (ship_length : int) (id : int) (grid : t) =
  (*col number first, then row number*)
  let grid_length = Array.length grid in
  let horz_or_vert = Random.int 2 in
  if horz_or_vert = 0 then
    (*place a random horizontal ship*)
    let row_index = Random.int grid_length in
    let col_index = Random.int (grid_length - ship_length) in
    let c1 = (col_index, row_index) in
    let c2 = (col_index + ship_length - 1, row_index) in
    let is_valid, users_ship_length =
      validate_ship_given_index_coords c1 c2 grid
    in
    if is_valid = true && users_ship_length = ship_length then
      for i = col_index to col_index + ship_length - 1 do
        let () = change_to_ship grid id ship_length (i, row_index) in
        ()
      done
    else set_one_random_ship ship_length id grid
  else
    (*place a random vertical ship*)
    let col_index = Random.int grid_length in
    let row_index = Random.int (grid_length - ship_length) in
    let c1 = (col_index, row_index) in
    let c2 = (col_index, row_index + ship_length - 1) in
    let is_valid, users_ship_length =
      validate_ship_given_index_coords c1 c2 grid
    in
    if is_valid = true && users_ship_length = ship_length then
      for i = row_index to row_index + ship_length - 1 do
        let () = change_to_ship grid id ship_length (col_index, i) in
        ()
      done
    else set_one_random_ship ship_length id grid

let rec set_random_ships_given_ids (ship_lengths : int list)
    (ship_ids : int list) (grid : t) =
  if List.length ship_lengths = 0 then ()
  else
    let first_length = List.hd ship_lengths in
    let id = List.hd ship_ids in
    let () = set_one_random_ship first_length id grid in
    let new_ship_lengths = remove_first_element ship_lengths in
    let new_ship_ids = remove_first_element ship_ids in
    set_random_ships_given_ids new_ship_lengths new_ship_ids grid

let rec ask_user_to_set_ships ship_lengths ship_ids grid =
  print_endline
    "\n\
     You will now orient your ships.\n\
     Enter 'custom' to choose your own ship orientations, or enter 'random' to \
     randomly place your ships.";
  let entry = read_line () in
  if entry = "custom" then set_ships_given_ids ship_lengths ship_ids grid
  else if entry = "random" then
    let () = set_random_ships_given_ids ship_lengths ship_ids grid in
    print_grid grid
  else
    let () = print_endline "Your input is not valid." in
    ask_user_to_set_ships ship_lengths ship_ids grid

let set_ships (ship_lengths : int list) (grid : t) =
  let ship_ids = List.mapi (fun x _ -> x) ship_lengths in
  ask_user_to_set_ships ship_lengths ship_ids grid

let get_length size = List.length size

let num_ships get_length =
  let rec add i acc =
    if i <= get_length then add (i + 1) (i :: acc) else List.rev acc
  in
  add 1 []

let random_grid size grid =
  let num_ships = num_ships (get_length size) in
  set_random_ships_given_ids size num_ships grid;
  grid
