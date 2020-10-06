open Card
open State
open Command
open Ai
open Ui

(** [print_winner p_name] is the string representation of the player who won
    the game and text displaying the game has been won output to terminal. *)
let print_winner p_name = 
  ANSITerminal.(print_string [green] (p_name ^ " wins.\n")); 
  print_endline ("\n");
  print_endline ("\n");
  ANSITerminal.(print_string [green]("(*------------------------------*)\n")); 
  print_endline ("\n");
  print_endline ("\n")

(** [print_players players] is the string representation of the each players' 
    money and cards outputted into terminal. *)
let rec print_players (players : State.player list) = match players with 
  | [] -> print_endline ""
  | h :: t -> print_endline (player_name h ^ " money: " ^ 
                             string_of_int (player_money h) ^ " "); 
    let cards = player_cards h in
    print_endline (player_name h ^ " cards:\n" ^ 
                   make_card_reps [fst cards ; snd cards]);
    print_players t

(** [print_money players] is the string representation of each [players]'s 
    money outputted into terminal. *)
let rec print_money (players : State.player list) = match players with 
  | [] -> print_endline ""
  | h :: t -> print_endline (player_name h ^ " money: " ^ 
                             string_of_int (player_money h) ^ " \n"); 
    print_money t

(** [print_player players] is the string representation of the user's money, 
    user's cards, and players' money outputted into terminal. *)
let print_player (players : State.player list) = 
  let user = List.hd players in 
  print_endline ("Your money: " ^ string_of_int (player_money user));
  print_endline "Here are your cards: "; 
  let cards = player_cards user in 
  print_endline (make_card_reps [fst cards; snd cards]);
  print_endline "Opposing players and their money:";
  List.tl players |> print_money

(** [print_cards table_cards] is the string representation of [table_cards]
    outputted into terminal. *)
let rec print_cards table_cards = 
  print_endline (make_card_reps table_cards)

(** [unpack_helper a] is the value option [a] is holding. 
    Raises: Failure if [a] is None. *)
let unpack_helper = function
  | Some x -> x
  | None -> failwith "Imposible" 

(** [unpack_helper a] is the name of value option [a] is holding or "none" if 
    [a] is None. *)
let unpack_helper2 = function
  | Some x -> x.name
  | None -> "None"

(** [print_bidset bs] is the string representation of the players and bids in
    player options list [bs] output to terminal. *)
let print_bidset bs = 
  print_endline ("Set " ^ (bs |> fst |> unpack_helper2));
  print_endline ("Bid " ^ (bs |> snd |> string_of_int))

(** [print_state state p_name] is the string representation of a state of 
    gameplay [state] with the pot amount, the cards dealt on the table, 
    each player's information depending on which state the game is in, and 
    appropriate text for round and game winning conditions output to terminal.*)
let print_state state p_name = 
  print_endline ("Pot = " ^ string_of_int (pot state));

  if cards_on_table state |> List.length = 5 && fst (bid_set state) <> None 
     && p_name = player_name (state |> bid_set |> fst |> unpack_helper)
  then print_players (players_in_play state)

  else if List.hd (players_in_play state) |> player_name = "player1" 
  then print_player (players_in_play state)
  else  ANSITerminal.(print_string [magenta] "You lost. \n"); 

  print_bidset (bid_set state);
  print_endline ("The cards on the table are ");
  print_endline (cards_on_table state |> make_card_reps)

(** [game state] describes the current state. If 
    it is not the player's turn in the game, [game state] calls itself with a
    new [state] based on the result of the ai's turn. *)
let rec game (state : State.round) = 
  let current_player = player_name (current_bet state) in
  if List.length (players_in_game state) = 1 then (
    ANSITerminal.(print_string [green] (current_player ^ " WINS " ^ 
                                        "THE WHOLE GAME.\n")); Stdlib.exit 0)
  else
    let players = players_in_play state in
    print_state state current_player;
    if List.length players = 1 then (
      print_winner current_player;
      game (start_new_round state current_player))
    else if List.length (cards_on_table state) = 5 
         && fst (bid_set state) <> None 
         && current_player = player_name 
              (state |> bid_set |> fst |> unpack_helper) then (
      let win = choose_winner (List.hd (players)) 
          (List.tl (players)) (cards_on_table state) in
      print_winner (win |> player_name);
      game (start_new_round state (player_name win)))
    else if fst (bid_set state) <> None 
         && current_player = player_name 
              (state |> bid_set |> fst |> unpack_helper) then 
      game (deal_next_cards state)
    else if player_money (current_bet state) = 0 then 
      game (all_in_turn_update state)
    else if current_player = "player1" then 
      user_command state
    else 
      game (ai_turn state)

(** [user_command state] is the state of the game after a user enters a command.
    The user is prompted for a command, the command is validated 
    then calls itself [game state] based on that command or terminates the game. 
    If the command is invalid, the user is given a message stating the error, 
    then prompts for new command. *)
and user_command state = 
  print_endline ("What would you like to do?"); 
  print_endline("(Commands are: \"call\", \"check\", \"raise [x]\", \"fold\"," ^
                " \"all in\", and \"quit\")"); 
  match parse (read_line ()) with
  | Quit -> ANSITerminal.(print_string [red] "\nGoodbye and good game\n");
    Stdlib.exit 0
  | Fold -> game (fold_update state)
  | Raise x -> if valid_raise state x then
      game (raise_update state x) else
      ANSITerminal.(print_string [red] "\nYou don't have that type of 
      capital\n"); game state
  | Call -> if valid_call state then 
      game (call_update state) else
      ANSITerminal.(print_string [red] "\nYou don't have that type of capital or
      there is nothing to be called\n"); game state
  | Check -> if valid_check state then game (call_update state) else
      ANSITerminal.(print_string [red] "\nYou cannot check\n"); game state
  | Allin -> state |> all_in_update |> game
  | exception Empty -> 
    ANSITerminal.(print_string [red] "\nYou did not give any command.\n");
    game state
  | exception Malformed -> 
    ANSITerminal.(print_string [red] "\nYou gave an invalid command.\n");
    game state


(** [main ()] prompts for the game to play, then starts it. *)
let rec main () = 
  print_endline "How many people do you want to play against?";
  match int_of_string_opt (read_line ()) with
  | Some x -> if x > 0 then game (init_state (x+1)) else 
      ANSITerminal.(print_string [red] "\nYou didn't input a valid number.\n"); 
    Stdlib.exit 0
  | None ->  ANSITerminal.(print_string [red] "\nYou didn't input a number.\n"); 
    Stdlib.exit 0

(* Execute the game engine. *)
let () = main ()
