open Battleship.Grid
(**@authors Niti Goyal, Ginger McCoy, Naakai McDonald, Nidhi Soma *)

open Battleship

type game_state = {
  mutable current_player : Player.t;
  mutable opponent : Player.t;
}

let switch_player state =
  let temp = state.current_player in
  state.current_player <- state.opponent;
  state.opponent <- temp;
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    ("\nIt's now " ^ state.current_player.name ^ "'s turn.");
  print_endline ""

let rec play_turn loop state () =
  print_endline "Enter the coordinates to attack (e.g., A5):";
  let coord = read_line () in
  try
    let row, col = coordinates coord in
    if
      row < 0
      || row >= Array.length state.opponent.board
      || col < 0
      || col >= Array.length state.opponent.board.(0)
    then (
      print_endline "Invalid coordinates. Please try again.";
      play_turn loop state ())
    else
      let cell = state.opponent.board.(row).(col) in
      match cell with
      | Water ->
          change_state state.opponent.board coord;
          print_endline "\nMISS!\n";
          state.current_player.missed_turns <-
            state.current_player.missed_turns + 1;
          print_their_board state.opponent.board;
          switch_player state;
          loop state
      | Ship { id; _ } ->
          change_state state.opponent.board coord;
          print_endline "\nHIT!\n";
          if is_sunk coord id state.opponent.board then (
            print_endline "You sunk a ship!\n";
            state.current_player.num_ships_sunk <-
              state.current_player.num_ships_sunk + 1;
            sink_ship coord id state.opponent.board;
            if
              state.current_player.num_ships_sunk
              = List.length (get_ships (Array.length state.opponent.board))
            then (
              print_their_board state.opponent.board;
              print_endline
                ("\nCongratulations! You sank all of " ^ state.opponent.name
               ^ "'s ships!");
              exit 0))
          else ();
          print_their_board state.opponent.board;
          switch_player state;
          loop state
      | _ ->
          print_endline "You already attacked this position.";
          play_turn loop state ()
  with Failure _ ->
    print_endline "Invalid coordinates. Please try again.";
    play_turn loop state ()

let rec main_loop state =
  print_endline
    "\n\
     Enter 'other' to view the opponent's board, 'next' to switch player, \
     'show' to see your board, 'play' to start your turn, 'single' for one \
     player gameplay, or 'exit' to quit:";
  match read_line () with
  | "other" ->
      print_their_board state.opponent.board;
      main_loop state
  | "next" ->
      if state.current_player.is_ships_set then (
        switch_player state;
        main_loop state)
      else (
        print_endline "You must set your ships before switching players.";
        main_loop state)
  | "show" ->
      print_grid state.current_player.board;
      main_loop state
  | "play" -> begin
      if state.current_player.is_ships_set = false then (
        set_ships
          (get_ships (Array.length state.current_player.board))
          state.current_player.board;
        state.current_player.is_ships_set <- true;
        main_loop state)
      else if state.opponent.is_ships_set = false then (
        print_endline "Waiting for the opponent to set their ships.";
        main_loop state)
      else if Player.allowed_turn state.current_player then
        (*play the user's minigame, and play their turn if they win, otherwise
          switch to the next player*)
        let won_minigame = Player.play_mini_game state.current_player in
        if won_minigame = true then play_turn main_loop state () else ()
      else switch_player state;
      main_loop state
    end
  | "single" -> begin
      (* random_grid fxn, get_ship_ids fxn, random_guess fxn, feeling crazy then
         play mini games???*)
      if state.opponent.is_ships_set = false then (
        switch_player state;
        print_endline "Placing my ships now.";
        let size = Array.length state.opponent.board in
        let ship_ids = get_length (get_ships size) in
        set_random_ships_given_ids (get_ships size) (num_ships ship_ids)
          state.current_player.board;
        state.current_player.is_ships_set <- true;
        switch_player state;
        main_loop state)
      else if Player.allowed_turn state.current_player then (
        (*play the user's minigame, and play their turn if they win, otherwise
          switch to the next player*)
        let won_minigame = Player.play_mini_game state.current_player in
        if won_minigame = true then play_turn main_loop state () else ();
        main_loop state)
    end
  | "exit" ->
      print_endline "Exiting game.";
      exit 0
  | _ ->
      print_endline "Invalid command.";
      main_loop state

let enter_player_name num =
  print_endline
    ("Player " ^ string_of_int num
   ^ ", please enter your name (enter 'none' for single player):");
  read_line ()

let rec player_has_minigame num =
  print_endline
    ("Player " ^ string_of_int num
   ^ ", enter 'yes' if you want minigames to be associated with your turns, \
      'no' if not:");
  let input = read_line () in
  match input with
  | "yes" -> true
  | "no" -> false
  | _ ->
      print_endline "Invalid input";
      player_has_minigame num

let init_game size =
  let name1 = enter_player_name 1 in
  let name2 = enter_player_name 2 in
  let p1_has_game = player_has_minigame 1 in
  let p2_has_game = player_has_minigame 2 in
  let player1 =
    Battleship.Player.create_player name1 (create_board size) p1_has_game
  in
  let player2 =
    Battleship.Player.create_player name2 (create_board size) p2_has_game
  in
  { current_player = player1; opponent = player2 }

let rec enter_size () =
  print_endline "Please enter the grid size (5 to 26): ";
  let user_in = read_line () in
  try
    let size = int_of_string user_in in
    if size < 5 || size > 26 then (
      print_endline "Invalid size. The size must be between 5 and 26.";
      enter_size ())
    else init_game size
  with
  | Failure _ ->
      print_endline "Invalid input. Please enter a number.";
      enter_size ()
  | _ ->
      print_endline "Something went wrong. Shutting down...";
      exit 1

let () =
  let () =
    ANSITerminal.print_string [ ANSITerminal.red ]
      "\nWelcome to Battleship! \n\n"
  in
  let game_state = enter_size () in
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    ("\nIt's now " ^ game_state.current_player.name ^ "'s turn.\n");
  main_loop game_state
