let () = Random.self_init ()

(* AF: Each player is represented as a record of Player.t {name, board,
   ships_set, ships_sunk, missed turns}, with information specific to each
   player over the course of the game. *)
(*RI: is_ships_set must start as false when a player is initialized and
  num_ships_sunk and missed_turns must be nonnegative. *)

type t = {
  name : string;
  board : Grid.t;
  minigame : bool;
  mutable is_ships_set : bool;
  mutable num_ships_sunk : int;
  mutable missed_turns : int;
}

let create_player n b g =
  {
    name = n;
    board = b;
    is_ships_set = false;
    num_ships_sunk = 0;
    missed_turns = 0;
    minigame = g;
  }

(* if time, make it so counts misses in a row to be more fair*)
let allowed_turn player =
  let misses =
    List.fold_left ( + ) 0 (Grid.get_ships (Array.length player.board))
  in
  if player.missed_turns >= misses then
    let () =
      print_endline
        ("Penalty for " ^ string_of_int misses ^ " misses: skip 1 turn")
    in
    let () = player.missed_turns <- 0 in
    false
  else true

(* for more code - let user have three attempts*)
let guess_num n =
  let () = print_endline "Guess a number from 1 to 5!" in
  let input = read_line () in
  let users_num = int_of_string_opt input in
  match users_num with
  | None -> false
  | Some x ->
      if x = n then true
      else
        let () =
          print_endline ("Wrong - the number was " ^ string_of_int n ^ "")
        in
        false

let rec coin_flip flip_int =
  let () = print_endline "Let's flip a coin for your turn. Heads or Tails?" in
  let input = read_line () in
  if input = "Heads" || input = "heads" then
    if flip_int = 0 then true else false
  else if input = "Tails" || input = "tails" then
    if flip_int = 1 then true else false
  else
    let () = print_endline "Invalid guess. Try again" in
    coin_flip flip_int

(*helper function for multiplication game*)
let valid_mult_answer input num1 num2 = input = num1 * num2

let multiplication_game num1 num2 : bool =
  let () =
    print_endline
      ("What is " ^ string_of_int num1 ^ " * " ^ string_of_int num2 ^ ": ")
  in
  let input = read_line () in
  let users_num = int_of_string_opt input in
  match users_num with
  | None -> false
  | Some input -> if valid_mult_answer input num1 num2 then true else false

let remove_char (letter : string) (word : string) : string =
  let letter_index = String.index word letter.[0] in
  String.sub word 0 letter_index
  ^ String.sub word (letter_index + 1) (String.length word - 1 - letter_index)

let rec anagram_helper (reduced_word : string) (scrambled_list : string list) :
    string =
  if reduced_word = "" then List.fold_left ( ^ ) "" scrambled_list
  else
    let picked_str =
      String.sub reduced_word (Random.int (String.length reduced_word)) 1
    in
    anagram_helper
      (remove_char picked_str reduced_word)
      (picked_str :: scrambled_list)

let rec three_tries guess count correct =
  if guess = correct && count <= 2 then true
  else if guess <> correct && count > 2 then false
  else
    let () = print_endline ("Guess " ^ string_of_int (count + 1) ^ ": ") in
    let new_guess = read_line () in
    three_tries new_guess (count + 1) correct

let anagram_game () : bool =
  let words = BatList.of_enum (BatFile.lines_of "lib/dictionary.txt") in
  let word_choice = List.nth words (Random.int (List.length words)) in
  let scrambled_word = anagram_helper word_choice [] in
  let () = print_endline ("Unscramble this word: " ^ scrambled_word) in
  three_tries "" 0 word_choice

let rec rock_paper_scissors () : bool =
  print_endline "Choose a move : 'rock' , 'paper', 'scissors', 'random' ";
  print_endline "Rock!";
  print_endline "Paper!";
  print_endline "Scissors!";
  print_endline "SHOOT!";
  match read_line () with
  | ("rock" | "paper" | "scissors") as player_move -> (
      let computer_choices = [ "rock"; "paper"; "scissors" ] in
      let computer_move = List.nth computer_choices (Random.int 3) in
      Printf.printf "You chose :  %s. I chose %s." player_move computer_move;
      match (player_move, computer_move) with
      | "rock", "scissors" -> true
      | "paper", "rock" -> true
      | "scissors", "paper" -> true
      | "rock", "paper" -> false
      | "paper", "scissors" -> false
      | "scissors", "rock" -> false
      | _ ->
          print_endline "It's a tie!";
          rock_paper_scissors ())
  | _ ->
      print_endline "Invalid move. Try again.";
      rock_paper_scissors ()

let rec travel_game () : bool =
  print_endline "You enter a jungle.";
  print_endline "There is a fork in the road!";
  print_endline "Do you go 'left' or 'right'?";
  let num = Random.int 2 in
  let input1 = read_line () in
  match input1 with
  | "left" ->
      if num = 0 then
        let () =
          print_endline
            "You found a river. Do you build a 'raft' or 'swim' across?"
        in
        let input3 = read_line () in
        match input3 with
        | "raft" ->
            let () =
              print_endline
                "Boring. You passed from starvation while making the raft. \
                 Womp Womp."
            in
            false
        | "swim" ->
            let () =
              print_endline
                "Yay! You had a party with the alligators. You pass!"
            in
            false
        | _ ->
            let () = print_endline "You have to make a decision!! Womp womp." in
            false
      else
        let () =
          print_endline
            "You took the left road, but reached some quicksand. There's no \
             way to get across. Womp Womp."
        in
        false
  | "right" ->
      let num2 = Random.int 2 in
      if num2 = 0 then
        let () = print_endline "A right doesn't correct a wrong! Womp Womp" in
        false
      else
        let () =
          print_endline
            "Great luck! there was a rainbow at the end of the road, and you \
             found a pot of gold at the end!"
        in
        true
  | _ ->
      print_endline "Invalid decision. Try again.";
      travel_game ()

let result_feedback is_pass =
  let () =
    if is_pass then ANSITerminal.print_string [ ANSITerminal.green ] "Passed\n"
    else
      ANSITerminal.print_string [ ANSITerminal.red ] "Failed. Skipping turn. \n"
  in
  is_pass

let play_mini_game (player : t) : bool =
  if player.minigame = false then true
    (*if player has set their minigame option to False, then just return true*)
  else
    (*choose a random minigame to play, play it, and return whether user wins or
      not*)
    let num1 = Random.int 6 in
    (*num1 is a number between 0 to 3 inclusive*)
    match num1 with
    | 0 ->
        (*play guess__num game*) result_feedback (guess_num (Random.int 5 + 1))
    | 1 ->
        (*play flip coin game*)
        result_feedback (coin_flip (Random.int 2))
    | 2 ->
        (*play multiplication game*)
        result_feedback (multiplication_game (Random.int 11) (Random.int 11))
    | 3 -> (*play anagram minigame*) result_feedback (anagram_game ())
    | 4 -> result_feedback (rock_paper_scissors ())
    | 5 -> result_feedback (travel_game ())
    | _ -> true
