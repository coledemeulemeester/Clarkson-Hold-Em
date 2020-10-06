(** Testing of the system *)

(* Test plan: We developed our test cases using the glass box testing method.
   We chose to develop our tests in this fashion because the round record in 
   the state module has many different fields that are affected differently
   by the different commands that the user may input. We wanted to ensure that
   all of these fields were being updated accordingly depending on the different
   conditions, so this requires knowing the implementation of the functions.
   All of the modules, apart from main, were automatically tested using OUnit 
   tests as they are the underlying modules that keep track of the state based 
   on each turn. The main module controls the flow of the game and prompts the 
   user for commands, so we tested this module manually through the command 
   line by running the 'make play' target. This test plan demonstrates the 
   correctness of our system because each module is tested individually and
   then the main module tests putting all of these pieces together to create
   a functioning game. *)

open OUnit2
open State
open Card

let empty_card = {value = 0; suit = Spade;}
let two_spades = {value= 2; suit= Spade;}
let three_spades = {value= 3; suit= Spade;}
let four_spades = {value= 4; suit= Spade;}
let five_spades = {value= 5; suit= Spade;}
let six_spades = {value= 6; suit= Spade;}
let seven_spades = {value= 7; suit= Spade;}
let eight_spades = {value= 8;suit= Spade;}
let nine_spades = {value= 9; suit= Spade;}
let ten_spades = {value= 10; suit= Spade;}
let jack_spades = {value= 11; suit= Spade;}
let queen_spades = {value= 12; suit= Spade;}
let king_spades = {value= 13; suit= Spade;}
let ace_spades = {value= 14; suit= Spade;}
let two_hearts = {value= 2; suit= Heart;}
let three_hearts = {value= 3; suit= Heart;}
let four_hearts = {value= 4; suit= Heart;}
let five_hearts = {value= 5; suit= Heart;}
let six_hearts = {value= 6; suit= Heart;}
let seven_hearts = {value= 7; suit= Heart;}
let eight_hearts = {value= 8;suit= Heart;}
let nine_hearts = {value= 9; suit= Heart;}
let ten_hearts = {value= 10; suit= Heart;}
let jack_hearts = {value= 11; suit= Heart;}
let queen_hearts = {value= 12; suit= Heart;}
let king_hearts = {value= 13; suit= Heart;}
let ace_hearts = {value= 14; suit= Heart;}
let two_diamonds = {value= 2; suit= Diamond;}
let three_diamonds = {value= 3; suit= Diamond;}
let four_diamonds = {value= 4; suit= Diamond;}
let five_diamonds = {value= 5; suit= Diamond;}
let six_diamonds = {value= 6; suit= Diamond;}
let seven_diamonds = {value= 7; suit= Diamond;}
let eight_diamonds = {value= 8;suit= Diamond;}
let nine_diamonds = {value= 9; suit= Diamond;}
let ten_diamonds = {value= 10; suit= Diamond;}
let jack_diamonds = {value= 11; suit= Diamond;}
let queen_diamonds = {value= 12; suit= Diamond;}
let king_diamonds = {value= 13; suit= Diamond;}
let ace_diamonds = {value= 14; suit= Diamond;}
let two_clubs = {value= 2; suit= Club;}
let three_clubs = {value= 3; suit= Club;}
let four_clubs = {value= 4; suit= Club;}
let five_clubs = {value= 5; suit= Club;}
let six_clubs = {value= 6; suit= Club;}
let seven_clubs = {value= 7; suit= Club;}
let eight_clubs = {value= 8;suit= Club;}
let nine_clubs = {value= 9; suit= Club;}
let ten_clubs = {value= 10; suit= Club;}
let jack_clubs = {value= 11; suit= Club;}
let queen_clubs = {value= 12; suit= Club;}
let king_clubs = {value= 13; suit= Club;}
let ace_clubs = {value= 14; suit= Club;}

(** pair_test*)
let pair_7_234 = {
  rank = Pair;
  type_card = seven_spades;
  kicker = [four_spades;three_spades;two_spades];
}
(** flush_test*)
let flush_7_234 = {
  rank = Flush;
  type_card = seven_spades;
  kicker = [seven_spades; seven_spades; four_spades;three_spades;two_spades];
}
(** 1st value in kicker*)
let pair_6_234 = {
  rank = Pair;
  type_card = six_spades;
  kicker = [four_spades;three_spades;two_spades];
}
(** 1st value in kicker*)
let pair_6_345 = {
  rank = Pair;
  type_card = six_spades;
  kicker = [five_spades;four_spades;three_spades];
}

(** high card*)
let high14_12_10_8_6 = {
  rank = High_card;
  type_card = ace_spades;
  kicker = [ace_spades;queen_diamonds;ten_spades;eight_spades;six_spades];
}
(** three of a kind*)
let three_6_45 = {
  rank = Three_of_kind;
  type_card = six_spades;
  kicker = [five_spades;four_spades;];
}
(** flush *)
let flush_6_45 = {
  rank = Flush;
  type_card = six_spades;
  kicker = [six_spades;six_spades;six_spades; five_spades;four_spades;];
}
let four_6_5 = {
  rank = Four_of_Kind;
  type_card = six_spades;
  kicker = [five_spades;];
}

(** first high card*)
let high13_12_10_8_6 = {
  rank = High_card;
  type_card = king_spades;
  kicker = [king_spades;queen_spades;ten_spades;eight_spades;six_spades];
}
(** 2nd value in kicker*)
let high14_13_10_8_6 = {
  rank = High_card;
  type_card = ace_spades;
  kicker = [ace_spades;king_spades;ten_spades;eight_spades;six_spades];
}
(** 3rd value in kicker*)
let high14_12_11_8_6 = {
  rank = High_card;
  type_card = ace_spades;
  kicker = [ace_spades;queen_diamonds;jack_spades;eight_spades;six_spades];
}
(** 4th value in kicker*)
let high14_12_10_9_6 = {
  rank = High_card;
  type_card = ace_spades;
  kicker = [ace_spades;queen_spades;ten_spades;nine_spades;six_spades];
}
(** 5th value in kicker*)
let high14_12_10_8_7 = {
  rank = High_card;
  type_card = ace_spades;
  kicker = [ace_spades;queen_spades;ten_spades;eight_spades;six_spades];
}

let full3_2 = {
  rank = Full_House;
  type_card = three_spades;
  kicker = two_spades:: [];
}
let full2_3 = {
  rank = Full_House;
  type_card = two_spades;
  kicker = three_spades::[];
}
let twopair3_2_5 = {
  rank = Two_pair;
  type_card = three_spades;
  kicker = two_spades::[five_spades];
}
let empty_hand = {rank = High_card; type_card = empty_card; kicker = []}
let cards_pair_7_234 = 
  [seven_spades; seven_spades; four_spades; three_spades; two_spades;]
let cards_pair_7_234two = 
  [seven_spades; seven_diamonds; four_spades; three_spades; two_spades;]
let cards_three_6_45 = 
  [six_spades; six_spades; six_spades; five_spades; four_spades;]
let cards_three_6_45two = 
  [six_spades; six_diamonds; six_hearts; five_spades; four_spades;]
let cards_four_6_5 = 
  [six_spades; six_spades; six_spades; six_spades; five_spades;]
let cards_high_14_12_10_8_6 = 
  [ace_spades; queen_spades; ten_spades; eight_spades; six_spades;]
let cards_high_14_12_10_8_6two = 
  [ace_spades; queen_diamonds; ten_spades; eight_spades; six_spades;]
let cards_full_3_2 = 
  [three_spades;three_spades; three_spades; two_spades; two_spades]
let cards_full_2_3 = 
  [three_spades;three_spades; two_spades; two_spades; two_spades]
let cards_2pair_3_2_5 = 
  [five_spades; three_spades;three_spades; two_spades; two_spades;]
let cards_2pair_3_2_5two = 
  [five_spades; three_spades;three_diamonds; two_spades; two_hearts;]
let cards_hflush_14_12_10_8_6 = {
  rank = Flush;
  type_card = ace_spades;
  kicker = cards_high_14_12_10_8_6;
}
let rf_spade = [ace_spades; king_spades; queen_spades; jack_spades; ten_spades;]
let cards_RF_14_13_12_11_10 = {
  rank = Royal_Flush;
  type_card = ace_spades;
  kicker = rf_spade;
}

let sf_r = [king_spades; queen_spades; jack_spades; ten_spades; nine_spades]
let cards_sF_13_12_11_10_9 = {
  rank = Straight_Flush;
  type_card = king_spades;
  kicker = sf_r;
}
let s_k_q_j_t_9 = 
  [king_spades; queen_hearts; jack_spades; ten_spades; nine_spades]
let cards_s_k_q_j_t_9 = {
  rank = Straight;
  type_card = king_spades;
  kicker = s_k_q_j_t_9;
}
let flush3_2_5 = {
  rank = Flush;
  type_card = five_spades;
  kicker = cards_2pair_3_2_5;
}

(* card_value
   card_suit
   compare
   to_string *)
let card_tests = [
  "card_value numbered" >:: 
  (fun _ -> assert_equal (card_value three_clubs) 3);
  "card_value face card" >:: 
  (fun _ -> assert_equal (card_value ace_spades) 14);
  "card_suit diamonds" >::
  (fun _ -> assert_equal (card_suit two_diamonds) Diamond);
  "card_suit heart" >::
  (fun _ -> assert_equal (card_suit nine_hearts) Heart);
  "compare different numbers" >:: 
  (fun _ -> assert_equal (compare ten_clubs four_spades) GT);
  "compare face vs number" >::
  (fun _ -> assert_equal (compare six_diamonds queen_spades ) LT);
  "compare with suit ranks" >::
  (fun _ -> assert_equal (compare five_hearts five_clubs) GT);
  "compare equal" >::
  (fun _ -> assert_equal (compare seven_diamonds seven_diamonds) EQ);
  "to_string" >:: 
  (fun _ -> assert_equal (to_string ace_clubs) "Ace of Club");

]

open Command
let command_tests = [
  "parse Call" >:: (fun _ -> assert_equal (parse "call") Call);
  "parse Call [sic]" >:: (fun _ -> assert_equal (parse "Call") Call);
  "parse Call" >:: (fun _ -> assert_equal (parse "caLl") Call);
  "parse Quit" >:: (fun _ -> assert_equal (parse "quit") Quit);
  "parse Raise" >:: (fun _ -> assert_equal (parse "raise 30") (Raise 30) );
  "parse Fold" >:: (fun _ -> assert_equal (parse "fold") Fold);
  "parse All in" >:: (fun _ -> assert_equal (parse "all in") Allin);
  "parse check" >:: (fun _ -> assert_equal (parse "check") Check);
  "parse empty" >:: (fun _ -> assert_raises Empty (fun () -> parse ""));
  "parse malformed" >:: 
  (fun _ -> assert_raises Malformed (fun () -> parse "asf"));


]

let four_diamonds_five_diamonds_str = 
  "\t┌─────────┐\t┌─────────┐\n"^ 
  "\t│4        │\t│5        │\n"^ 
  "\t│♦        │\t│♦        │\n"^ 
  "\t│    ♦    │\t│    ♦    │\n"^ 
  "\t│        ♦│\t│        ♦│\n"^ 
  "\t│       4 │\t│       5 │\n"^
  "\t└─────────┘\t└─────────┘"

let ten_spade_two_spade_three_club_str = 
  "\t┌─────────┐\t┌─────────┐\t┌─────────┐\n"^
  "\t│10       │\t│2        │\t│3        │\n"^
  "\t│♠        │\t│♠        │\t│♣        │\n"^
  "\t│    ♠    │\t│    ♠    │\t│    ♣    │\n"^
  "\t│        ♠│\t│        ♠│\t│        ♣│\n"^
  "\t│       10│\t│       2 │\t│       3 │\n"^
  "\t└─────────┘\t└─────────┘\t└─────────┘"

let queen_hearts_str = 
  "\t┌─────────┐\n"^
  "\t│Q        │\n"^
  "\t│♥        │\n"^
  "\t│    ♥    │\n"^
  "\t│        ♥│\n"^
  "\t│       Q │\n"^
  "\t└─────────┘"
open Ui
let ui_tests = [
  "make card reps" >:: 
  (fun _ -> assert_equal (make_card_reps [four_diamonds;five_diamonds;]) 
      four_diamonds_five_diamonds_str);
  "make card reps 2" >:: 
  (fun _ -> assert_equal (make_card_reps [ten_spades;two_spades;three_clubs;]) 
      ten_spade_two_spade_three_club_str);
  "make card reps 3" >:: 
  (fun _ -> assert_equal (make_card_reps [queen_hearts]) queen_hearts_str);
]

let hand_tests = [
  "pair" >:: (fun _ -> assert_equal (make_hand cards_pair_7_234two) pair_7_234);
  "flush_over_pair f" >:: (fun _ -> 
      assert_equal (make_hand cards_pair_7_234) flush_7_234);
  "threekind" >:: (fun _ -> 
      assert_equal (make_hand cards_three_6_45two) three_6_45);
  "flush_over_three" >:: (fun _ -> 
      assert_equal (make_hand cards_three_6_45) flush_6_45);
  "fourkind" >:: (fun _ -> 
      assert_equal (make_hand cards_four_6_5) four_6_5);
  "fourkind over flush" >:: (fun _ -> 
      assert_equal (make_hand cards_four_6_5) four_6_5);
  "high" >:: (fun _ -> 
      assert_equal (make_hand cards_high_14_12_10_8_6two) high14_12_10_8_6);
  "flush over high" >:: (fun _ -> 
      assert_equal(make_hand cards_high_14_12_10_8_6)cards_hflush_14_12_10_8_6);
  "fullhouse1" >:: (fun _ -> assert_equal (make_hand cards_full_3_2) full3_2);
  "fullhouse2" >:: (fun _ -> assert_equal (make_hand cards_full_2_3) full2_3);
  "fullhouse over flush" >:: (fun _ -> 
      assert_equal (make_hand cards_full_2_3) full2_3);
  "twopair" >:: (fun _ -> 
      assert_equal (make_hand cards_2pair_3_2_5two) twopair3_2_5);
  "flush overtwopair" >:: (fun _ -> 
      assert_equal (make_hand cards_2pair_3_2_5) flush3_2_5);
  "flush" >:: (fun _ -> 
      assert_equal(make_hand cards_high_14_12_10_8_6)cards_hflush_14_12_10_8_6);
  "rf make hand" >:: (fun _ -> 
      assert_equal (make_hand rf_spade) cards_RF_14_13_12_11_10);
  "sf make hand" >:: (fun _ -> 
      assert_equal (make_hand sf_r) cards_sF_13_12_11_10_9);
  "straight make hand" >:: (fun _ -> 
      assert_equal (make_hand s_k_q_j_t_9) cards_s_k_q_j_t_9);

]

let p1 = {
  name = "p1";
  money = 1000;
  bid = 0;
  cards = (ace_spades, ace_clubs);
}
let p2 = {
  name = "p2";
  money = 1000;
  bid = 0;
  cards = (king_spades, king_clubs);
}

let win_test_cards =
  [ace_spades ;jack_spades; ten_spades; two_hearts; two_clubs]

let win_test_sf_4 =
  [ace_spades ;jack_spades; ten_spades; two_hearts; two_clubs]

let p4kind = {
  name = "p4";
  money = 1000;
  bid = 0;
  cards = (two_clubs, two_diamonds);
}

let psf = {
  name = "p4";
  money = 1000;
  bid = 0;
  cards = (nine_spades, three_hearts);
}

let prf = {
  name = "p4";
  money = 1000;
  bid = 0;
  cards = (ace_spades, three_hearts);
}

let win_test_rf_sf_ =
  [king_spades ;queen_spades; jack_spades; ten_spades; two_diamonds;two_hearts]

let p_full_house = {
  name = "p4";
  money = 1000;
  bid = 0;
  cards = (ten_clubs, two_clubs);
}

let p_flush = {
  name = "p4";
  money = 1000;
  bid = 0;
  cards = (three_spades, two_spades);
}

let p_straight = {
  name = "p4";
  money = 1000;
  bid = 0;
  cards = (six_clubs, five_clubs);
}

let win_test_s_f =
  [eight_spades ;seven_spades; four_spades;]

let p_3kind = {
  name = "p4";
  money = 1000;
  bid = 0;
  cards = (seven_clubs, seven_diamonds);
}
let p_2pair = {
  name = "p4";
  money = 1000;
  bid = 0;
  cards = (eight_clubs, four_clubs);
}
let p_pair = {
  name = "p4";
  money = 1000;
  bid = 0;
  cards = (eight_clubs, two_clubs);
}
let p_high2 = {
  name = "p4";
  money = 1000;
  bid = 0;
  cards = (ace_clubs, two_clubs);
}
let p_high1 = {
  name = "p4";
  money = 1000;
  bid = 0;
  cards = (king_clubs, two_clubs);
}
let p_kicker = {
  name = "p4";
  money = 1000;
  bid = 0;
  cards = (king_clubs, three_clubs);
}
let winner_tests = [
  "winner no cards" >:: (fun _ -> assert_equal (choose_winner p1 [p2] []) p1);
  "winner typecard" >:: (fun _ -> assert_equal (choose_winner p1 [p2] []) p1);
  "winner kicker" >:: (fun _ -> 
      assert_equal (choose_winner p_high1 [p_kicker] win_test_s_f) p_kicker);
  "winner high" >::   (fun _ -> 
      assert_equal (choose_winner p_high1 [p_high2] win_test_s_f) p_high2);
  "winner pair" >::   (fun _ -> 
      assert_equal (choose_winner p_pair [p_high2] win_test_s_f) p_pair);
  "winner two pair" >::   (fun _ -> 
      assert_equal (choose_winner p_2pair [p_pair;] win_test_s_f) p_2pair);
  "winner three of kind" >::   (fun _ -> 
      assert_equal (choose_winner p_3kind [p_2pair;] win_test_s_f) p_3kind);
  "winner straight" >::   (fun _ -> 
      assert_equal (choose_winner p_straight [p_3kind;] win_test_s_f) 
        p_straight);
  "winner flush" >::   (fun _ -> 
      assert_equal (choose_winner p_flush [p_straight;] win_test_s_f) p_flush);
  "winner full house" >::   (fun _ -> 
      assert_equal (choose_winner p_full_house [p_flush;] win_test_rf_sf_) 
        p_full_house);
  "winner four of kind" >::   (fun _ -> 
      assert_equal (choose_winner p4kind [p_full_house;] win_test_rf_sf_) 
        p4kind);
  "winner straight flush" >::   (fun _ -> 
      assert_equal (choose_winner psf [p4kind;] win_test_rf_sf_) psf);
  "winner royal flush" >::   (fun _ -> 
      assert_equal (choose_winner p4kind [psf; prf] win_test_rf_sf_) prf);
  "winner royal flush order1" >::   (fun _ -> 
      assert_equal (choose_winner prf [psf; p4kind] win_test_rf_sf_) prf);
  "winner royal flush order2" >::   (fun _ -> 
      assert_equal (choose_winner p4kind [prf; psf] win_test_rf_sf_) prf);
]

let comp2 val1 val2 =
  match (Card.compare val1 val2) with
  | LT -> -1
  | EQ -> 0
  | GT -> 1


let init = init_state 2
let player_1 = List.hd (players_in_play init)
let player_2 = List.hd (List.tl (players_in_play init))

let init_state_tests = [
  "init deal" >:: (fun _ -> 
      assert_equal 52 
        (List.length (working_deck init @ cards_on_table init @ 
                      ([fst (player_cards player_1); 
                        snd (player_cards player_1)]) @
                      ([fst (player_cards player_2); 
                        snd (player_cards player_2)]))));

  "init pot test" >:: (fun _ -> assert_equal 75 (pot init));
  "init big blind" >:: 
  (fun _ -> assert_equal "player1" (init |> big_blind |> player_name));
  "init bid set" >:: 
  (fun _ -> assert_equal (Some player_1, 50) (bid_set init));
  "init little blind" >:: 
  (fun _ -> assert_equal "player2" (player_name (little_blind init)));
  "init players in play" >:: (fun _ -> assert_equal [player_1; player_2] 
                                 (players_in_play init));
  "init players in play2" >:: (fun _ -> assert_equal 2 
                                  (List.length (players_in_play init)));
  "init current player" >:: (fun _ -> assert_equal "player2" 
                                (init |> current_bet |> player_name))
]

let fold_state = fold_update init 
let players_play_fold = players_in_play fold_state

let fold_state_tests = [
  "fold players in play" >:: (fun _ -> assert_equal ["player1"] 
                                 [(List.hd players_play_fold) |> player_name]);
  "fold players in play2" >:: 
  (fun _ -> assert_equal 1 (List.length players_play_fold));
  "fold pot test" >:: (fun _ -> assert_equal 75 (pot fold_state));

  "fold big blind" >:: 
  (fun _ -> assert_equal ("player1") (fold_state |> big_blind |> player_name));
  "fold little blind" >:: 
  (fun _ -> assert_equal "player2" (player_name (little_blind fold_state)));
  "fold current player" >:: 
  (fun _ -> assert_equal "player1" (fold_state |> current_bet |> player_name))
]

let call_state = call_update init
let players_play_call = players_in_play call_state
let player_1_call = List.hd players_play_call
let player_2_call = List.hd (List.tl (players_play_call))

let call_state_tests = [
  "call players in play" >:: 
  (fun _ -> assert_equal [player_1_call; player_2_call] players_play_call);
  "call players in play2" >:: (fun _ -> assert_equal 2 
                                  (List.length players_play_call));
  "call pot test" >:: (fun _ -> assert_equal 100 (pot call_state));
  "call player 1 money" >:: 
  (fun _ -> assert_equal 50 (get_player_bid player_1_call))
]

let raise_state = raise_update init 30
let players_play_raise = players_in_play raise_state
let player_1_raise = List.hd players_play_raise
(* let player_2_raise = List.hd (List.tl (players_play_raise)) *)
let player_2_raise = List.nth players_play_raise 1

let raise_state_tests = [
  "raise players in play" >:: 
  (fun _ -> assert_equal [player_1_raise; player_2_raise] players_play_raise);
  "raise players in play2" >:: (fun _ -> assert_equal 2 
                                   (List.length players_play_raise));
  "raise pot test" >:: (fun _ -> assert_equal 130 (pot raise_state));
  "bid set snd" >:: (fun _ -> assert_equal (raise_state |> bid_set |> snd) 80);
  "raise player 2 bid" >:: 
  (fun _ -> assert_equal 80 (get_player_bid player_2_raise))
]

let all_in_update_state = all_in_update init
let players_play_all_in = players_in_play all_in_update_state
let player_1_all_in = List.hd players_play_all_in
let player_2_all_in = List.nth players_play_all_in 1

let all_in_tests = [
  "all in players in play" >:: 
  (fun _ -> 
     assert_equal [player_1_all_in; player_2_all_in] players_play_all_in);
  "all in players in play2" >:: (fun _ -> assert_equal 2 
                                    (List.length players_play_all_in));
  "all in pot test" >:: (fun _ -> assert_equal 1050 (pot all_in_update_state));
  "all in bid set snd" >:: 
  (fun _ -> assert_equal (all_in_update_state |> bid_set |> snd) 1000);
  "all in player 2 bid" >:: 
  (fun _ -> assert_equal 1000 (get_player_bid player_2_all_in))
]

let start_new_round_state = start_new_round init "player2"
let players_play_new_round = players_in_play start_new_round_state
let player_1_new = List.hd players_play_new_round
let player_2_new= List.nth players_play_new_round 1

let start_new_round_tests = [
  "new round players in play" >:: 
  (fun _ -> 
     assert_equal [player_1_new; player_2_new] players_play_new_round);
  "new round players in play2" >:: (fun _ -> assert_equal 2 
                                       (List.length players_play_new_round));
  "new round pot test" >:: 
  (fun _ -> assert_equal 75 (pot start_new_round_state));
  "new round bid set snd" >:: 
  (fun _ -> assert_equal (start_new_round_state |> bid_set |> snd) 50);
  "new round player 2 money" >:: 
  (fun _ -> assert_equal 1000 (player_money player_2_new))

]

let validate_tests = [
  "valid call on init" >:: (fun _ -> assert_equal true (valid_call init));
  "valid raise on init" >:: (fun _ -> assert_equal true (valid_raise init 5));
  "invalid raise on init" >:: 
  (fun _ -> assert_equal false (valid_raise init 10000));
  "invalid check on init" >:: (fun _ -> assert_equal false (valid_check init));
  "valid check on call state" >:: 
  (fun _ -> assert_equal true (valid_check call_state));
  "invalid call on call state" >::
  (fun _ -> assert_equal false (valid_call call_state));
]

let suite = "search test suite" >::: List.flatten [
    card_tests;
    init_state_tests;
    fold_state_tests;
    call_state_tests;
    raise_state_tests;
    validate_tests;
    hand_tests;
    ui_tests;
    command_tests;
    all_in_tests;
    winner_tests;
    start_new_round_tests;
  ]

let _ = run_test_tt_main suite