(** 
   Representation of dynamic game state.

   This module represents the state of a game as it is being played,
   including:
   - the working deck (the cards that have not been dealt yet)
   - cards on the table (the cards shown on the table)
   - little blind (the [player] representing the little blind)
   - big blind (the [player] representing the big blind)
   - current bet (the [player] whose turn it is)
   - bid set (the [player] who set the highest bid and the amount of that bid)
   - players in game (the players left in the game)
   - players in play (the players left in the current round)
   - pot (the total money currently in the betting pool)
     and functions that cause the state to change.
*)

(** The type representing a player in a game of poker. 
    Contains: a player's name, their money remaining,
    and their cards. *)
type player = {
  name : string; 
  money : int;
  bid : int;
  cards : Card.card * Card.card; 
}

(** The type representing all possible hand combinations. *)
type hand_type = Royal_Flush | Straight_Flush | Four_of_Kind | Full_House 
               | Flush | Straight | Three_of_kind | Two_pair | Pair | High_card

(** The type representing a hand combination of five cards. 
    Contains: 
    - a hand combination' rank
    - the face_value of the rank
    - the kicker cards not included in rank. *)
type hand = {
  rank : hand_type;
  type_card : Card.card;
  kicker : Card.card list;
}        

(** [player_name p] is the name for a player [p]. *)
val player_name : player -> string

(** [player_money p] is the money for a player [p]. *)
val player_money : player -> int

(** [player_bid p] is the bid for a player [p]. *)
val get_player_bid : player -> int

(** [player_cards p] is the cards for a player [p]. *)
val player_cards : player -> Card.card * Card.card

(** The abstract type of values representing the game state. *)
type round 

(** [working_deck st] is the working_deck for a state [st]. *)
val working_deck : round -> Card.card list

(** [cards_on_table st] is the cards_on_table for a state [st]. *)
val cards_on_table : round -> Card.card list

(** [little_blind st] is the little_blind for a state [st]. *)
val little_blind : round -> player

(** [big_blind st] is the big_blind for a state [st]. *)
val big_blind : round -> player

(** [current_bet st] is the current_bet for a state [st]. *)
val current_bet : round -> player

(** [bid_set st] is the bid_set for a state [st]. *)
val bid_set : round -> player option * int

(** [players_in_play st] is the players_in_play for a state [st]. *)
val players_in_play : round -> player list

(** [players_in_game st] is the players_in_game for a state [st]. *)
val players_in_game : round -> player list

(** [pot st] is the pot for a state [st]. *)
val pot : round -> int

(** [init_state x] is the initial state of the game. In that state, there
    are [x] number of players total with each player satrting with 1000 in 
    money. They are each dealt two cards from a shuffled deck of all playing 
    cards. The little blind is first player and the big blind is the second 
    player. All of the players are in the game and the pot is equal to the 
    amount put in by the big blind and the little blind. The bid set is no 
    player with an amount of the big blind. *)
val init_state : int -> round

(** [valid_raise state raise_amt] returns if raising [raise_amt] is allowed by
    the current player. *)
val valid_raise : round -> int -> bool

(** [valid_call state] returns if calling is allowed by the current player. *)
val valid_call : round -> bool

(** [valid_check state] returns if checking is allowed by the current player. *)
val valid_check : round -> bool

(** [call_update state] is the new state after the current player has 
    entered a call command. The current player's bet is adjusted 
    so that they have the same amount of money in the pot as the highest bidder. 
    The pot is adjusted accordingly. It is then the next player's turn. *)
val call_update: round -> round

(** [raise_update state] is the new state after the current player has 
    entered a raise command. The current player's bet so that they are the
    now the bid set player. The amount raised is added to the amount that they
    needed to call. The pot is adjusted accordingly. 
    It is then the next player's turn. *)
val raise_update: round -> int -> round

(** [fold_update state] is the new state after the current player has 
    entered a fold command. The player is removed from players in round.
    It is then the next player's turn. *)
val fold_update: round -> round

(** [all_in_update state] is the new state after the current player has 
    entered an all in command. The player's bid is adjusted so that they
    all of their money is in the pot. The pot is adjusted accordingly. 
    Whether that all in is a raise must be determined for bid set.
    It is then the next player's turn. *)
val all_in_update: round -> round

(** [all_in_turn_update state] is the new state when the current player
    is already all in. Since they have no move to make, it is the next player's
    turn.  *)
val all_in_turn_update: round -> round

(** [start_new_round state] is the new state when the current round is
    completed and a new round must begin. The blinds are rotated to the next
    players in the list. The pot goes to the winner of the round and is then 
    set back to the amount colledcted from the big blind and little blind. If
    a player has no more money, they are removed from the game. New cards 
    are dealt to every player in the round and the deck is reshuffled. 
*)
val start_new_round: round -> string -> round

(** [deal_next_cards state] is the new state when the current bidding round is
    completed and the next set of cards needs to be dealt to the table. *)
val deal_next_cards: round -> round

(** [choose_winner winner players_tl table] is the player who has the best hand
    among [players_tl]. It is [winner] unless a player in [players_tl] has a 
    higher hand rank. *)
val choose_winner: player -> player list -> Card.card list -> player   

(** [get_rank hand] is the rank of [hand]. *)
val get_rank : hand -> hand_type

(** [make_hand deck] is the hand for cards in [deck]. It is the best hand 
    combination of cards made up from the cards in [deck]. *) 
val make_hand : Card.card list -> hand


