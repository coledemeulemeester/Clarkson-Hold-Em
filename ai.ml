open State
open Card

(** [prob_calc14 v1 v2] returns the probability of a head to head win if a 
    player has cards with values 14 and [v2] *)
let prob_calc14 v1 v2 = 
  if v1 = 14 then 
    if v2 = 2 then 52.947
    else if v2 = 3 then 53.855
    else if v2 = 4 then 54.733
    else if v2 = 5 then 55.742
    else if v2 = 6 then 55.870
    else if v2 = 7 then 57.170
    else if v2 = 8 then 58.374
    else if v2 = 9 then 59.450
    else if v2 = 10 then 61.568
    else if v2 = 11 then 62.535
    else if v2 = 12 then 63.509
    else if v2 = 13 then 64.469
    else 84.932 
  else failwith "imposibru"

(** [prob_calc13 v1 v2] returns the probability of a head to head win if a 
    player has cards with values [v1] >= 13 and [v2] *)
let prob_calc13 v1 v2 = 
  if v1 = 13 then 
    if v2 = 2 then 48.423
    else if v2 = 3 then 49.331
    else if v2 = 4 then 50.225
    else if v2 = 5 then 51.254
    else if v2 = 6 then 52.297
    else if v2 = 7 then 53.417
    else if v2 = 8 then 54.432
    else if v2 = 9 then 56.408
    else if v2 = 10 then 58.494
    else if v2 = 11 then 59.441
    else if v2 = 12 then 60.432
    else if v2 = 13 then 82.117  
    else 64.469 
  else prob_calc14 v1 v2

(** [prob_calc12 v1 v2] returns the probability of a head to head win if a 
    player has cards with values [v1] >= 12 and [v2] *)
let prob_calc12 v1 v2 = 
  if v1 = 12 then 
    if v2 = 2 then 45.110
    else if v2 = 3 then 46.025
    else if v2 = 4 then 46.925
    else if v2 = 5 then 47.959
    else if v2 = 6 then 48.997
    else if v2 = 7 then 49.904
    else if v2 = 8 then 51.931
    else if v2 = 9 then 53.862
    else if v2 = 10 then 55.947
    else if v2 = 11 then 56.906
    else if v2 = 12 then 79.632 
    else if  v2 = 13 then 60.432  
    else 63.509 
  else prob_calc13 v1 v2

(** [prob_calc11 v1 v2] returns the probability of a head to head win if a 
    player has cards with values [v1] >= 11 and [v2] *)
let prob_calc11 v1 v2 = 
  if v1 = 11 then 
    if v2 = 2 then 42.049
    else if v2 = 3 then 42.967
    else if v2 = 4 then 43.869
    else if v2 = 5 then 44.905
    else if v2 = 6 then 45.714
    else if v2 = 7 then 47.726
    else if v2 = 8 then 49.714
    else if v2 = 9 then 51.639
    else if v2 = 10 then 53.826
    else if v2 = 11 then 77.153 
    else if  v2 = 12 then 56.906 
    else if  v2 = 13 then 59.441 
    else 62.535
  else prob_calc12 v1 v2 

(** [prob_calc10 v1 v2] returns the probability of a head to head win if a 
    player has cards with values [v1] >= 10 and [v2] *)
let prob_calc10 v1 v2 = 
  if v1 = 10 then 
    if v2 = 2 then 39.239
    else if v2 = 3 then 40.155
    else if v2 = 4 then 41.055
    else if v2 = 5 then 41.857
    else if v2 = 6 then 43.848
    else if v2 = 7 then 45.830
    else if v2 = 8 then 47.818
    else if v2 = 9 then 49.816
    else if v2 = 10 then 74.660
    else if  v2 = 11 then 53.826 
    else if  v2 = 12 then 55.947   
    else if  v2 = 13 then 58.494 
    else 61.568  
  else prob_calc11 v1 v2

(** [prob_calc9 v1 v2] returns the probability of a head to head win if a 
    player has cards with values [v1] >= 9 and [v2] *)
let prob_calc9 v1 v2 = 
  if v1 = 9 then 
    if v2 = 2 then 36.517
    else if v2 = 3 then 37.428
    else if v2 = 4 then 38.086
    else if v2 = 5 then 38.741
    else if v2 = 6 then 42.103
    else if v2 = 7 then 44.072
    else if v2 = 8 then 46.068
    else if v2 = 9 then 71.666  
    else if  v2 = 10 then 49.816 
    else if  v2 = 11 then 51.639  
    else if  v2 = 12 then 53.862  
    else if  v2 = 13 then 56.408  
    else 59.450 
  else prob_calc10 v1 v2

(** [prob_calc8 v1 v2] returns the probability of a head to head win if a 
    player has cards with values [v1] >= 8 and [v2] *)
let prob_calc8 v1 v2 = 
  if v1 = 8 then 
    if v2 = 2 then 34.087
    else if v2 = 3 then 34.750
    else if v2 = 4 then 36.709
    else if v2 = 5 then 38.741
    else if v2 = 6 then 40.697
    else if v2 = 7 then 42.693
    else if v2 = 8 then 68.717 
    else if  v2 = 9 then 46.068 
    else if  v2 = 10 then 47.818  
    else if  v2 = 11 then 49.714 
    else if  v2 = 12 then 51.931  
    else if  v2 = 13 then 54.432  
    else 58.374 
  else prob_calc9 v1 v2

(** [prob_calc7 v1 v2] returns the probability of a head to head win if a 
    player has cards with values [v1] >= 7 and [v2] *)
let prob_calc7 v1 v2 = 
  if v1 = 7 then 
    if v2 = 2 then 31.710
    else if v2 = 3 then 33.718
    else if v2 = 4 then 35.662
    else if v2 = 5 then 37.675
    else if v2 = 6 then 39.654
    else if v2 = 7 then 65.725
    else if  v2 = 8 then 42.693
    else if  v2 = 9 then 44.072 
    else if  v2 = 10 then 45.830
    else if  v2 = 11 then 47.726
    else if  v2 = 12 then 49.904 
    else if  v2 = 13 then 53.417
    else 57.170
  else prob_calc8 v1 v2

(** [prob_calc6 v1 v2] returns the probability of a head to head win if a 
    player has cards with values [v1] >= 6 and [v2] *)
let prob_calc6 v1 v2 = 
  if v1 = 6 then 
    if  v2 = 2 then 31.079
    else if  v2 = 3 then 33.070
    else if  v2 = 4 then 35.003
    else if  v2 = 5 then 37.013
    else if  v2 = 6 then 62.700  
    else if  v2 = 7 then 39.654 
    else if  v2 = 8 then 40.697 
    else if  v2 = 9 then 42.103
    else if  v2 = 10 then 43.848
    else if  v2 = 11 then 45.714 
    else if  v2 = 12 then 48.997
    else if  v2 = 13 then 52.297
    else 55.870 
  else prob_calc7 v1 v2

(** [prob_calc5 v1 v2] returns the probability of a head to head win if a 
    player has cards with values [v1] >= 5 and [v2] *)
let prob_calc5 v1 v2 = 
  if v1 = 5 then 
    if  v2 = 2 then 31.194
    else if  v2 = 3 then 33.165
    else if  v2 = 4 then 35.075
    else if  v2 = 5 then 59.640  
    else if  v2 = 6 then 37.013 
    else if  v2 = 7 then 37.675  
    else if  v2 = 8 then 38.741 
    else if  v2 = 9 then 40.137  
    else if  v2 = 10 then 41.857 
    else if  v2 = 11 then 44.905 
    else if  v2 = 12 then 47.959  
    else if  v2 = 13 then 51.254 
    else 55.742 
  else prob_calc6 v1 v2

(** [prob_calc4 v1 v2] returns the probability of a head to head win if a 
    player has cards with values [v1] >= 4 and [v2] *)
let prob_calc4 v1 v2 = 
  if v1 = 4 then 
    if  v2 = 2 then 30.117
    else if  v2 = 3 then 32.066
    else if  v2 = 4 then 56.257
    else if  v2 = 5 then 35.075
    else if  v2 = 6 then 35.003 
    else if  v2 = 7 then 35.662 
    else if  v2 = 8 then 36.709 
    else if  v2 = 9 then 38.086
    else if  v2 = 10 then 41.055
    else if  v2 = 11 then 43.869
    else if  v2 = 12 then 46.925 
    else if  v2 = 13 then 50.225 
    else 54.733 
  else prob_calc5 v1 v2

(** [prob_calc3 v1 v2] returns the probability of a head to head win if a 
    player has cards with values [v1] >= 3 and [v2] *)
let prob_calc3 v1 v2 = 
  if v1 = 3 then 
    if  v2 = 2 then 29.239
    else if  v2 = 3 then 52.839  
    else if  v2 = 4 then 32.066
    else if  v2 = 5 then 33.165 
    else if  v2 = 6 then 33.070 
    else if  v2 = 7 then 33.718 
    else if  v2 = 8 then 34.750 
    else if  v2 = 9 then 37.428 
    else if  v2 = 10 then 40.155  
    else if  v2 = 11 then 42.967 
    else if  v2 = 12 then 46.025
    else if  v2 = 13 then 49.331
    else 53.855 
  else prob_calc4 v1 v2

(** [prob_calc2 v1 v2] returns the probability of a head to head win if a 
    player has cards with values [v1] >= 2 and [v2] *)
let prob_calc2 v1 v2 = 
  if v1 = 2 then 
    if v2 = 2 then 49.385
    else if  v2 = 3 then 29.239
    else if  v2 = 4 then 30.117
    else if  v2 = 5 then 31.194
    else if  v2 = 6 then 31.079
    else if  v2 = 7 then 31.710
    else if  v2 = 8 then 34.087  
    else if  v2 = 9 then 36.517 
    else if  v2 = 10 then 39.239  
    else if  v2 = 11 then 42.049 
    else if  v2 = 12 then 45.110 
    else if  v2 = 13 then 48.423
    else 52.947
  else prob_calc3 v1 v2 

(** [rng x n init] returns the sum of [x] random numbers from 0 to [n - 1] *)
let rec rng x n init = if x > 0 then rng (x-1) n (Random.int n + init) else 
    init

(** [get_flop_probability starting_hand num_players] returns the slightly 
    randomized adjusted win probability of a player given their starting hand 
    [starting_hand] and the number of players in play [num_players] *)
let get_flop_probability starting_hand num_players = 
  let c1val = starting_hand |> fst |> card_value in
  let c2val = starting_hand |> snd |> card_value in
  if c2val = c1val then 
    100 - (float_of_int (14 - c1val) |> sqrt |> int_of_float) else

    let prob = prob_calc2 c1val c1val in
    let adjusted = 
      if prob > 56. then 80 else
        (prob *. 100.) /. 
        (prob +. ((100. -.prob) *. 
                  ((float_of_int (num_players) -. 1.) *. 0.2) ** 0.05))
        |> int_of_float in
    if Random.bool () then adjusted + Random.int 20 - 10 
    else adjusted + Random.int 10 - 5

(** [pre_flop state] is the state of the game after the AI has chosen what 
    to do before any cards are on the table *)
let pre_flop state = 
  let current_player = state |> current_bet in
  let prob = get_flop_probability 
      (player_cards current_player) (List.length (players_in_play state)) in

  let random = rng 5 100 0 in
  if float_of_int random >= (float_of_int (prob * 5)) *. 1.2 then
    fold_update state
  else if float_of_int (rng 5 100 0) >= (float_of_int (prob * 5)) *. 1.2 then
    let max_raise = 
      (player_money (current_bet state) + get_player_bid (current_bet state)
       - snd (bid_set state)) |> float_of_int in
    let random = Random.int (int_of_float (max_raise ** 0.7)) in
    if random = 0 && valid_call state then call_update state else 
      raise_update state random

  else if valid_call state then call_update state 
  else all_in_update state

(** [hand_strength rank] is the determined strength associated with [rank] *)
let hand_strength rank = 
  match rank with
  | Royal_Flush -> 100
  | Straight_Flush -> 100
  | Four_of_Kind -> 100
  | Full_House -> 85
  | Flush -> 75
  | Straight -> 55
  | Three_of_kind -> 35
  | Two_pair -> 25
  | Pair -> 12
  | High_card -> 1

(** [one_away cards score card_ind] returns the potential hand strength of a 
    players hand, going through all possible card values and determining the 
    best hand it creates when paired with [cards] *)
let rec one_away cards score card_ind =
  if card_ind > 1 then 
    let temp = ({value = card_ind ; suit = Spade} :: cards) 
               |> make_hand |> get_rank |> hand_strength in 
    one_away cards (temp + score) (card_ind - 1)
  else score - (List.length cards * 12)

(** [get_cards players name state] returns a list containing the cards on the 
    table in [state] and the cards of [name] in list [players] *)
let rec get_cards players name state = 
  match players with 
  | [] -> failwith "no player with this name"
  | head :: tail -> if player_name head = name then 
      fst (player_cards head) :: snd (player_cards head) 
      :: cards_on_table state
    else get_cards tail name state

(** [raiser potential max_raise] determines the amount to be raised
    that is no more than [max_raise] *)
let raiser potential max_raise = 
  if max_raise < ((float_of_int potential) +. 50.0) then 
    int_of_float ((max_raise)** 0.5)
  else Random.int (potential) + 25

(** [bettor raise_thresh call_thresh adjusted potential max_raise state] 
    determines if the AI should raise, call, or fold given scaling parameters
    [raise_thresh], [call_thresh] which numerically make the AI aggressive or 
    conservative dependent on the state of the game. *)
let bettor raise_thresh call_thresh adjusted potential max_raise state = 
  let rand = float_of_int (rng 5 65 0) in 
  if rand > (adjusted *. raise_thresh) *. 5.0 then raise_update state 
      (raiser potential max_raise)
  else if (rand > (adjusted *. call_thresh) *. 5.0) &&
          valid_call state then call_update state
  else if valid_check state then call_update state
  else fold_update state

(** [flop_turn state no_players call money] returns the state of the game 
    after the AI has played its turn after the flop *)
let flop_turn state no_players call money = 
  let max_raise = (player_money (current_bet state) + get_player_bid 
                     (current_bet state)
                   - snd (bid_set state)) |> float_of_int in
  let current_player = state |> current_bet in 
  let potential = 2 * (one_away (get_cards (players_in_game state) 
                                   (player_name current_player) state) 0 14) in
  let adjusted = (float_of_int potential) /. (float_of_int no_players ** 0.8) in
  let ratio = if call = 0 then 6.0 else 
      float_of_int money /. float_of_int call in 

  if ratio < 1.0 then 
    if (Random.int 65) |> float_of_int <= adjusted then fold_update state
    else all_in_update state

  else if ratio >= 6.0 then 
    bettor 3.5 2.2 adjusted potential max_raise state
  else if ratio <= 3.0 then 
    bettor 4.1 2.75 adjusted potential max_raise state
  else
    bettor 3.8 2.5 adjusted potential max_raise state

(** [river_turn state no_players call money] returns the state of the game 
    after the AI has played its turn after the river *)
let river_turn state no_players call money = 
  let max_raise = (player_money (current_bet state) + get_player_bid 
                     (current_bet state)
                   - snd (bid_set state)) |> float_of_int in
  let current_player = state |> current_bet in 
  let potential = (one_away (get_cards (players_in_game state) 
                               (player_name current_player) state) 0 14) in
  let adjusted = (float_of_int potential) /. (float_of_int no_players ** 0.9) in 
  let ratio = if call = 0 then 6.0 else 
      float_of_int money /. float_of_int call in 

  if ratio < 1.0 then 
    if (Random.int 50) |> float_of_int <= adjusted then fold_update state
    else all_in_update state

  else if ratio >= 7.0 then 
    bettor 3.5 3.0 adjusted potential max_raise state
  else if ratio <= 3.0 then 
    bettor 3. 2.95 adjusted potential max_raise state
  else
    bettor 3.0 2.5 adjusted potential max_raise state

(** [turny_turner state no_players call money] returns the state of the game 
    after the AI has played its turn after the river *)
let turny_turner state no_players call money = 
  let hand = make_hand (get_cards (players_in_game state) 
                          (state |> current_bet |> player_name) state) in 
  let strength = hand_strength (get_rank hand) in
  let ratio = float_of_int money /. float_of_int call in 

  let max_raise = (player_money (current_bet state) + get_player_bid 
                     (current_bet state)
                   - snd (bid_set state)) |> float_of_int in
  if strength = 100 then all_in_update state 

  else if ratio < 1.0 then 
    if strength >= 35 || Random.int 100 >= 70 then all_in_update state 
    else fold_update state

  else if ratio >= 6.0 then 
    if strength <= 35 && valid_call state then call_update state
    else raise_update state (raiser 100 max_raise)

  else 
    let a = Random.int 3 in 
    if a = 0 && valid_raise state 4 then 
      raise_update state 4
    else if a = 1 && valid_call state then call_update state
    else fold_update state

(** [ai_turn state] returns the state after the AI has played its turn, either 
    raising, calling, checking, or fold *)
let ai_turn (state : State.round) = 
  Unix.sleep 1;
  let current_player = state |> current_bet in
  let no_cards = state |> cards_on_table |> List.length in 
  let check = snd (bid_set state) - (state |> current_bet |> get_player_bid) in 

  if no_cards = 0 then pre_flop state 
  else if no_cards = 3 then flop_turn state 
      (state |> players_in_play |> List.length) 
      check (player_money current_player)

  else if no_cards = 4 then river_turn state 
      (state |> players_in_play |> List.length) 
      check (player_money current_player)
  else turny_turner state (state |> players_in_play |> List.length) check 
      (player_money current_player)