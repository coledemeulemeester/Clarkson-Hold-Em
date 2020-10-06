open Card

type player = {
  name : string; 
  money : int;
  bid : int;
  cards : Card.card * Card.card; 
}

let player_name p = 
  p.name

let player_money p = 
  p.money

let get_player_bid p = 
  p.bid

let player_cards p = 
  p.cards

type round = {
  working_deck : card list;
  cards_on_table : card list;

  little_blind: player; 
  big_blind: player;
  current_bet: player;
  bid_set: player option * int;

  players_in_game : player list;
  players_in_play : player list;
  pot : int;
}

let working_deck st = 
  st.working_deck

let cards_on_table st = 
  st.cards_on_table

let little_blind st = 
  st.little_blind

let big_blind st = 
  st.big_blind

let current_bet st = 
  st.current_bet

let bid_set st = 
  st.bid_set

let players_in_play st =
  st.players_in_play

let players_in_game st =
  st.players_in_game

let pot st = 
  st.pot


(** [unpack_helper a] is the name of the value [a] is holding. 
    If [a] is None, returns "None" *)
let print_bid_set = function
  | Some x -> x.name
  | None -> "None"

(** [print_player player] is the printed message containing the attributes of 
    [player]*)
let print_player player = 
  print_endline "Current bet:";
  print_endline player.name;
  print_endline (string_of_int player.money);
  print_endline (string_of_int player.bid)

(** [gen_deck_help lis num suit] is [lis] with all combinations of a card of 
    face_value [num] and suit [suit] appended to it. *)
let rec gen_deck_help (lis : card list) num suit : card list= 
  (** run through all nums of a suit*)
  if num = 1 then begin match suit with
    | Spade -> gen_deck_help lis 14 Heart 
    | Heart -> gen_deck_help lis 14 Diamond
    | Diamond -> gen_deck_help lis 14 Club
    | Club -> lis end
  else gen_deck_help ({value = num; suit = suit;} :: lis) (num-1) suit

(** [gen_deck] is a list of each card in a deck of cards. *)
let gen_deck : card list = 
  gen_deck_help [] 14 Spade

(** [shuffle lis] is list [lis] in random order. *)
let shuffle lis =
  QCheck.Gen.(generate1 (shuffle_l lis))

(** [lis_remove_player p_name lis] is player list [lis] without player 
    [p_name]. *)
let rec lis_remove_player p_name lis = 
  match lis with
  | [] -> failwith "player not found"
  | h :: t -> if h.name = p_name then t else h :: lis_remove_player p_name t

(** [deck_help num deck] is [deck] without the top [num] number of 
    cards. *)
let rec deck_help num deck = if num > 0 then
    deck_help (num-1) (List.tl deck) else deck

(** [gen_players player_count init deck] is a generated list of [player_count] 
    players, each having cards from [deck] *)
let rec gen_players player_count init deck= 
  if player_count = 1 then 
    gen_players (player_count-1) 
      ({name = "player" ^ string_of_int player_count; 
        money = 950; bid = 50; 
        cards = (List.hd deck, List.nth deck 1)} :: init) 
      (List.tl (List.tl(deck)))
  else if player_count = 2 then 
    gen_players (player_count-1) 
      ({name = "player" ^ string_of_int player_count; 
        money = 975; bid = 25; 
        cards = (List.hd deck, List.nth deck 1)} :: init) 
      (List.tl (List.tl(deck)))
  else if player_count > 0 then 
    gen_players (player_count-1) 
      ({name = "player" ^ string_of_int player_count; 
        money = 1000; bid = 0; 
        cards = (List.hd deck, List.nth deck 1)} :: init) 
      (List.tl (List.tl(deck)))
  else init

let init_state player_count = 
  let deck = shuffle gen_deck in
  let players = gen_players player_count [] deck in
  {
    cards_on_table = [];
    players_in_game = players;
    players_in_play = players;
    working_deck = deck_help (player_count*2) deck;

    pot = 75;
    little_blind= List.nth players 1;
    big_blind= List.hd players;

    current_bet = List.nth players 1;
    bid_set= Some (List.hd players), 50
  }

(* ---------------------------------------------------------------- *)

(** game play functions *)

(** [next_player players_in_play players_in_play2 player_name] is the player in 
    list [players_in_play] subsequent the player [player_name], looping around
     to the front of the list. *)
let rec next_player players_in_play players_in_play2 player_name =
  match players_in_play with 
  | [] -> failwith "Unmatched player"
  | h1 :: h2 :: t -> if h1.name = player_name then h2 else next_player (h2 :: t) 
        players_in_play2 player_name
  | h :: t -> List.hd players_in_play2

(** [player_bid p1 bid lis] is the list of players resulting from a bid of [bid]
    from player [p1] in list [lis]. *)
let rec player_bid p1 bid lis = 
  match lis with
  | [] -> failwith "player not found"
  | h :: t -> if h.name = p1 then 
      {name = h.name; 
       money = h.money - bid; 
       bid = h.bid + bid; 
       cards = h.cards} :: t
    else h :: player_bid p1 bid t 

let valid_raise state raise_amt = 
  player_money (current_bet state) >= 
  (raise_amt + (snd (bid_set state) - get_player_bid (current_bet state)))

let valid_call state = 
  (player_money (current_bet state) >= 
   snd (bid_set state) - (state |> current_bet |> get_player_bid))
  && not ((snd (bid_set state)) - (state |> current_bet |> get_player_bid) = 0)

let valid_check state = 
  snd (bid_set state) - (current_bet state |> get_player_bid) = 0

let call_update st = 
  let current_player = current_bet st |> player_name in
  let player_call = (snd(bid_set st) - (st |> current_bet |> get_player_bid)) in
  if player_call = 0 then print_endline (current_player ^ " checked")
  else print_endline (current_player ^ " called");
  let new_bids = player_bid current_player player_call st.players_in_play in
  let new_bids2 = player_bid current_player player_call st.players_in_game in
  let next_player = next_player st.players_in_play st.players_in_play
      current_player in
  {
    working_deck = st.working_deck;
    cards_on_table = st.cards_on_table;

    little_blind= st.little_blind;
    big_blind= st.big_blind;
    current_bet= next_player;
    bid_set= (if fst (bid_set st) = None 
              then Some (st.current_bet)
              else st |> bid_set |> fst), snd st.bid_set;

    players_in_game = new_bids2;
    players_in_play = new_bids;
    pot = st.pot + player_call;
  }

let raise_update st x = 
  let amount_raise = x + snd (bid_set st) - 
                     (st |> current_bet |> get_player_bid) in
  let current_player = current_bet st |> player_name in
  print_endline (current_player ^ " raised " ^ string_of_int x);
  let new_bids = player_bid current_player amount_raise st.players_in_play in
  let new_bids2 = player_bid current_player amount_raise st.players_in_game in
  let next_player = next_player st.players_in_play st.players_in_play 
      current_player in
  {
    working_deck = st.working_deck;
    cards_on_table = st.cards_on_table;

    little_blind= st.little_blind;
    big_blind= st.big_blind;
    current_bet= next_player;
    bid_set= Some st.current_bet, x + (snd st.bid_set);

    players_in_game = new_bids2;
    players_in_play = new_bids;
    pot = st.pot + amount_raise;
  }

let all_in_update st = 
  let current_player = current_bet st |> player_name in
  print_endline (current_player ^ " went all in");
  let money = st |> current_bet |> player_money in
  let all_in = 
    snd(bid_set st) - ((st |> current_bet |> get_player_bid) + money) in
  let next_player = next_player st.players_in_play st.players_in_play 
      current_player in
  let new_bids = player_bid current_player money st.players_in_play in
  let new_bids2 = player_bid current_player money st.players_in_game in
  {
    working_deck = st.working_deck;
    cards_on_table = st.cards_on_table;

    little_blind= st.little_blind;
    big_blind= st.big_blind;
    current_bet= next_player;
    bid_set= 
      if all_in <= 0 then Some st.current_bet, money + (snd st.bid_set) - (st |> current_bet |> get_player_bid)  
      else st.bid_set;

    players_in_game = new_bids2;
    players_in_play = new_bids;
    pot = st.pot + money;
  }

let all_in_turn_update st =
  let next_player = next_player st.players_in_play st.players_in_play 
      (current_bet st |> player_name) in
  {
    working_deck = st.working_deck;
    cards_on_table = st.cards_on_table;

    little_blind= st.little_blind;
    big_blind= st.big_blind;
    current_bet= next_player;
    bid_set= 
      if fst st.bid_set = None then Some st.current_bet, snd st.bid_set 
      else st.bid_set;

    players_in_game = st.players_in_game;
    players_in_play = st.players_in_play;
    pot = st.pot;
  }

(** [find_player p_name players] returns a player with name [p_nane] in 
    [players] *)
let rec find_player p_name (players : player list) = 
  match players with 
  | [] -> failwith "player does not exist"
  | head :: tail -> 
    if head.name = p_name then head
    else find_player p_name tail

(** [new_player_round pot deck players winner init] returns a list of players 
    ready for a new round*) 
let rec new_player_round pot deck players winner init = 
  match players with 
  | [] -> init
  | head :: tail -> 
    if head.name = winner 
    then (new_player_round pot ((List.tl (List.tl(deck)))) tail winner 
            (init @ [{name = winner ; 
                      money = head.money + pot ; 
                      bid = 0 ; 
                      cards = (List.hd deck, List.nth deck 1)}]))

    else if head.money = 0 then 
      new_player_round pot deck tail winner init

    else new_player_round pot ((List.tl (List.tl(deck)))) tail winner 
        (init @ [{name = head.name ; 
                  money = head.money ; 
                  bid = 0 ; 
                  cards = (List.hd deck, List.nth deck 1)}])

let fold_update state = 
  let current_player = current_bet state |> player_name in
  print_endline (current_player ^ " folded");
  let next_player = next_player state.players_in_play state.players_in_play 
      current_player in
  print_endline ("Next player:" ^ (player_name next_player));
  let removed = lis_remove_player current_player state.players_in_play in
  {
    working_deck = state.working_deck;
    cards_on_table = state.cards_on_table;

    little_blind = state.little_blind;
    big_blind = state.big_blind;
    current_bet = next_player;
    bid_set = state.bid_set;

    players_in_game = state.players_in_game;
    players_in_play = removed;
    pot = state.pot
  }

(** [set_blinds players big small init] returns a list of players where the big 
    and small blinds are set for the next round *)
let rec set_blinds players big small init = 
  match players with
  | [] -> List.rev init
  | head :: tail -> if player_name head = big then 
      set_blinds tail big small ({name = head.name ;
                                  money = if head.money < 50 then 0 
                                    else head.money - 50 ;
                                  bid = if head.money < 50 then head.money 
                                    else 50 ;
                                  cards = head.cards} :: init)
    else if player_name head = small then 
      set_blinds tail big small ({name = head.name ;
                                  money = if head.money < 25 then 0 
                                    else head.money - 25 ;
                                  bid = if head.money < 25 then head.money 
                                    else 25 ;
                                  cards = head.cards} :: init)
    else set_blinds tail big small (head :: init)

let start_new_round st winner = 
  let deck = shuffle gen_deck in
  let new_players = new_player_round st.pot deck st.players_in_game winner [] in
  let big = next_player new_players new_players (st.big_blind |> player_name) in
  let small = next_player new_players new_players (big |> player_name) in
  let newer_players = set_blinds new_players (player_name big) 
      (player_name small) [] in
  {
    working_deck = deck_help ((List.length new_players)*2) deck;
    cards_on_table = [];

    little_blind= small;
    big_blind= big;
    current_bet= if List.length newer_players = 2 then 
        (find_player (player_name small) newer_players) 
      else 
        next_player new_players new_players (small |> player_name);
    bid_set= Some big, 50;

    players_in_game = newer_players;
    players_in_play = newer_players;
    pot = 75;
  } 

(** [reshuffle st] returns a state with a reshuffled deck*) 
let reshuffle st =
  QCheck.Gen.(generate1 (shuffle_l st.cards_on_table))

(** [top_n_cards acc lis] is the first [acc] elements in [lis] *)
let rec top_n_cards acc lis = 
  if acc = 0 then []
  else List.hd lis :: top_n_cards (acc-1) (List.tl lis)

(** [deal_flop st] is the state resulting from a deal of the Flop in a round 
    of poker. *)
let deal_flop st = 
  {
    working_deck = deck_help 3 st.working_deck;
    cards_on_table = top_n_cards 3 st.working_deck @ st.cards_on_table;

    little_blind= st.little_blind;
    big_blind= st.big_blind;
    current_bet= st.current_bet;
    bid_set= None, snd (bid_set st);

    players_in_game = st.players_in_game;
    players_in_play = st.players_in_play;
    pot = st.pot;
  }

(** [deal_one st] is the state resulting from a deal of the River or Turn in a 
    round of poker. *)
let deal_one st = 
  {
    working_deck =  deck_help 1 st.working_deck;
    cards_on_table = top_n_cards 1 st.working_deck @ st.cards_on_table;

    little_blind= st.little_blind;
    big_blind= st.big_blind;
    current_bet= st.current_bet;
    bid_set= None, snd st.bid_set;

    players_in_game = st.players_in_game;
    players_in_play = st.players_in_play;
    pot = st.pot;
  }

let deal_next_cards st = 
  if List.length (cards_on_table st) = 0 then deal_flop st
  else deal_one st

(* ----------------------------------------------------------------*)

type hand_type = Royal_Flush | Straight_Flush | Four_of_Kind | Full_House 
               | Flush | Straight | Three_of_kind | Two_pair | Pair | High_card

type hand = {
  rank : hand_type;
  type_card : card;
  kicker : card list;
}           

let get_rank hand = hand.rank

(** [compare_help c1 c2] is -1 if c1 is [LT] c2, 0 if c1 is [EQ] c2, and 1 if 
    c1 is [GT] c2. Changes Card's compare back into normal compare. *)
let compare_help c1 c2 =
  match Card.compare c1 c2 with
  | LT -> -1
  | EQ -> 0
  | GT -> 1

(** [max_card c1 c2] returns the card with maximum card value. Returns [c1] if 
    there is a tie *)
let max_card c1 c2 = 
  if Card.card_value c1 >= Card.card_value c2 then c1 else c2

(** [max_card c1 c2] returns the card with minimum card value. Returns [c1] if 
    there is a tie *)
let min_card c1 c2 = 
  if Card.card_value c1 <= Card.card_value c2 then c1 else c2

(** [get_highest_n_cards n lis out] is the [n] number of cards in [lis] with the
    highest face_values sorted from low to high. 
    Requires: [lis] sorted high to low. *)
let rec get_highest_n_cards n lis out =
  if n = 0 then out else match lis with 
    | [] -> out (** shouldnt ever call this when n is longer than list*)
    | h :: t ->get_highest_n_cards (n-1) (t) (h :: out)

(** [three_kind_three_full a hnd] is a hand with rank Full_House, type_card 
    as the greater of [a] and the type_card of [hnd], and kicker as the 
    lesser of [a] and type_card of [hnd]. *)
let three_kind_three_full a hnd = 
  {rank = Full_House; 
   type_card = max_card hnd.type_card a; 
   kicker = (min_card hnd.type_card a) :: [];}

(** [three_kind_pair_full a hnd] is a hand with rank Full_House, type_card [a], 
    and kicker as the type_card of [hnd]. *)
let three_kind_pair_full a hnd = 
  {rank = Full_House; 
   type_card = a; 
   kicker = hnd.type_card::[];}

(** [pair_three_full a hnd] is a hand with rank Full_House, type_card 
    as the type_card of [hnd], and kicker as [a]. *)
let pair_three_full a hnd = 
  {rank = Full_House; 
   type_card = hnd.type_card ; 
   kicker = a:: [];}

(** [pair_pair_two_kind a hnd newouts] is a hand with rank Two_pair, type_card 
    as the larger of [a] and the type_card of [hnd], and kicker as the highest 
    cards in [newouts]. *)
let pair_pair_two_kind a hnd newouts = 
  {rank = Two_pair; 
   type_card = max_card hnd.type_card a; 
   kicker = (min_card hnd.type_card a) :: [newouts |> List.hd];}

(** [three_kind_make a hnd newouts] is a hand with rank Three_of_kind, type_card 
    [a], and kicker as the 2 highest cards in [newouts]. *)
let three_kind_make a hnd newouts = 
  {rank = Three_of_kind; 
   type_card = a; 
   kicker = get_highest_n_cards 2 newouts [] |> List.rev} 

(** [pair_make a hnd newouts] is a hand with rank Pair, type_card [a], and 
    kicker as the 3 highest cards in [newouts]. *)
let pair_make a hnd newouts = 
  {rank = Pair; 
   type_card = a; 
   kicker = get_highest_n_cards 3 newouts [] |> List.rev;}

(** [rec match_help hnd lis outs] is either [hnd] if there are no repeated cards
    or a hand with rank Four_of_Kind, Full_House, Three_of_kind, Two_pair, or 
    Pair; type_card of the highest face_value of the highest rank; and kicker
    with the highest remaining cards not used in the rank combination. In the
    case where a hand combination uses multiple sets of repeated cards, kicker 
    begins with the card values of the highest secondary set.
    Requires: [hnd] is the correct representation for the combination of cards
      put into the function,
      [lis] is ordered from high to low. *)
let rec match_help hnd lis outs = 
  match lis with
  | [] -> hnd 
  | a :: b :: c :: d :: t -> (** when has at least 4 elements*)
    four_branch a b c d t hnd outs
  | a :: b :: c :: t ->  (** when has 3 elements*) 
    three_branch a b c t hnd outs
  | a ::b :: t -> (** when has 2 elements*)
    two_branch a b t hnd outs
  | h :: t -> hnd (** when has 1 element*)

(** [four_branch a b c d t hnd outs] is the type of hand when there are
    at least 4 cards to be looked at. If all four cards have the same 
    card value, then it is a hand with rank Four_of_Kind, type_card [a],
    and kicker with the highest card among [t] and [outs].  
    If two or three cards have the same card value, it is the appropriate hand
    based on [hnd] and [t], else it checks for matches starting with [b] as
    the first card in [lis]. *)
and four_branch a b c d t hnd outs = 
  let ca = Card.card_value a in
  if ca = Card.card_value b (** if four match *)
  && ca = Card.card_value c 
  && ca = Card.card_value d then 
    let newouts = outs@t|>List.sort compare_help |> List.rev in
    {rank = Four_of_Kind; 
     type_card = a; 
     kicker = [newouts |> List.hd]}
  else if ca = Card.card_value b && ca = Card.card_value c then 
    begin (** if three match *)
      let newouts = d::outs@t|>List.sort compare_help |> List.rev in
      three_match_help a hnd newouts end
  else if ca = Card.card_value b then begin (** if two match *)
    let newouts = c::d::outs@t|>List.sort compare_help |> List.rev in
    two_match_help a hnd newouts end
  else match_help hnd (b::c::d::t) (a::outs) 

(** [three_branch a b c t hnd outs] is the type of hand when there are
    at least 3 cards to be looked at. Based on the design of function
    [match_help], it will only be called when there are exactly three
    cards in [lis]. 
    If two or three cards have the same card value, it is the appropriate 
    hand based on [hnd] and [t], else it checks for matches starting with 
    [b] as the first card in [lis].*)
and three_branch a b c t hnd outs = 
  let ca = Card.card_value a in
  let newouts = outs@t|>List.sort compare_help |> List.rev in
  if ca = Card.card_value b && ca = Card.card_value c then 
    three_match_help a hnd newouts
  else if ca = Card.card_value b then begin (** if two match *)
    let newouts = c::outs@t|>List.sort compare_help |> List.rev in
    two_match_help a hnd newouts end
  else match_help hnd (b::c::t) (a::outs)

(** [two_branch a b t hnd outs] is the type of hand when there are
    at least 2 cards to be looked at. Based on the design of function
    [match_help], it will only be called when there are exactly two
    cards in [lis]. 
    If two cards have the same card value, it is the appropriate 
    hand based on [hnd] and [t], else it checks for matches starting with 
    [b] as the first card in [lis].*)
and two_branch a b t hnd outs =
  let ca = Card.card_value a in
  let newouts = outs@t|>List.sort compare_help |> List.rev in
  if ca = Card.card_value b then two_match_help a hnd newouts
  else match_help hnd (b::t) (a::outs) 

(** [three_match_help a hnd newouts] is the hand that can be made when
    three of a card are in play. It can be hands with rank Full_House or 
    Three_of_kind. *)
and three_match_help a hnd newouts = 
  if hnd.rank = Three_of_kind then three_kind_three_full a hnd
  (** still has full house bc a combo can only use 5 cards *)
  else if hnd.rank = Pair then three_kind_pair_full a hnd
  else match_help (three_kind_make a hnd newouts) newouts []

(** [two_match_help a hnd newouts] is the hand that can be made when
    three of a card are in play. It can be hands with rank Full_House, 
    Two_Pair, or Pair. *)
and two_match_help a hnd newouts =
  if hnd.rank = Three_of_kind then pair_three_full a hnd
  else if hnd.rank = Pair then pair_pair_two_kind a hnd newouts
  else match_help (pair_make a hnd newouts) newouts []

(** [card_highest_val card lis] is the card with the highest face_value in 
    [lis]. If [lis] is empty, it is [card]. *)
let rec card_highest_val card lis = 
  match lis with
  | h :: t -> if Card.card_value h > Card.card_value card 
    then card_highest_val h t
    else card_highest_val card t
  | [] -> card

(** [straight_helper a b c d e rank] is the hand representing 
    a type of Straight with rank [rank], type_card [a], 
    and kicker with [a], [b], [c], [d], [e]*)
let straight_helper a b c d e rank = 
  {rank = rank; 
   type_card = a; 
   kicker = a :: b :: c :: d :: e :: []}

(** [make_straight lis hnd] is a hand combination with rank Royal_Flush, 
    Straight_Flush, or Straight if the cards in [lis] can make those hand 
    combinations respectively. The hand has type_card as the highest card in 
    the straight and kicker as the five cards in the straight ordered from high
    to low. If the cards in [lis] cannot make a straight, it is [hnd]. *)
let rec make_straight lis hnd = 
  match lis with 
  | a :: b :: c :: d :: e :: t -> 
    if Card.card_value a = (Card.card_value e + 4) &&
       Card.card_value a = (Card.card_value d + 3) &&
       Card.card_value a = (Card.card_value c + 2) &&
       Card.card_value a = (Card.card_value b + 1) then 
      if hnd.rank = Flush && Card.card_value a = 14 then 
        straight_helper a b c d e Royal_Flush
      else if hnd.rank = Flush then 
        straight_helper a b c d e Straight_Flush
      else 
        straight_helper a b c d e Straight
    else make_straight (b :: c :: d :: e :: t) hnd
  | _ -> hnd

(** [num_in_suit num suit lis] is the number of cards in [lis] with suit [suit]. 
    It is [num] if [lis] is empty. *)
let rec num_in_suit num suit lis = 
  match lis with 
  | [] -> num
  | h :: t -> if Card.card_suit h = suit then num_in_suit (num+1) suit t
    else num_in_suit num suit t

(** [make_flush lis hnd] is a hand combination with rank Flush, type_card as the
    highest card in the flush, and kicker as the five highest cards of the flush 
    suit. If the cards in [lis] cannot make a flush, it is [hnd]. *) 
let make_flush lis hnd = 
  if num_in_suit 0 Spade lis >= 5 
  || num_in_suit 0 Heart lis >= 5 
  || num_in_suit 0 Diamond lis >= 5 
  || num_in_suit 0 Club lis >= 5 
  then {rank = Flush; 
        type_card = List.hd lis; 
        kicker = get_highest_n_cards 5 lis []|> List.rev; }
  else hnd

(** [make_high_card lis] is the hand combination representing a hand with rank
    High_Card, the type_card as the highest card in a hand, and kicker as the 
    highest 5 cards in a hand. *)
let make_high_card lis = 
  {rank = High_card;
   type_card = List.hd lis ;
   kicker = get_highest_n_cards 5 lis [] |> List.rev;}

let make_hand deck = 
  let sort_deck = deck |> List.sort compare_help |> List.rev in
  (* hnd either Royal, SF, F, S, or H*)
  let hnd = 
    make_high_card sort_deck 
    |> make_flush sort_deck 
    |> make_straight sort_deck in
  let hnd2 = match_help hnd sort_deck [] in
  if hnd.rank = Royal_Flush 
  || hnd.rank = Straight_Flush then hnd
  else if hnd2.rank = Four_of_Kind 
       || hnd2.rank = Full_House then hnd2
  else if hnd.rank = Flush 
       || hnd.rank = Straight then hnd
  else if hnd2.rank = Three_of_kind 
       || hnd2.rank = Two_pair 
       || hnd2.rank = Pair then hnd2
  else hnd

(** [kicker_compare l1 l2] is 1 if the head of [l1] has a higher value than 
    [l2], -1 if [l2] is higher than [l1], and 0 if [l1] and [l2] are both 
    empty. *)
let rec kicker_compare l1 l2 = 
  match l1, l2 with
  | [], [] -> 0
  | h1 :: t1, h2::t2 -> let c = compare_help h1 h2 in 
    if c = 0 then kicker_compare t1 t2 else c
  | _, _ -> failwith "kicker_compare error"

(** [type_card_compare hnd1 hnd2] is 1 if [hnd1] has a higher type_card than 
    [hnd2], -1 when [hnd2] is higher than [hnd1], and [kicker_compare l1 l2] 
    on the kickers of the 2 hands when [hnd1] and [hnd2] have the same 
    type_card. *)
let type_card_compare hnd1 hnd2 = 
  let c = compare_help hnd1.type_card hnd2.type_card in
  if c = 0 then kicker_compare hnd1.kicker hnd2.kicker else c

(** [hand_rank_compare hnd1 hnd2] returns the number representing the superior 
    hand between [hnd1] and [hnd2]*)
let hand_rank_compare hnd1 hnd2 =
  match hnd1.rank, hnd2.rank with
  | Royal_Flush,Royal_Flush -> type_card_compare hnd1 hnd2
  | Royal_Flush, _ -> 1
  | _, Royal_Flush -> -1
  | Straight_Flush, Straight_Flush -> type_card_compare hnd1 hnd2
  | Straight_Flush, _ -> 1
  | _, Straight_Flush -> -1
  | Four_of_Kind, Four_of_Kind -> type_card_compare hnd1 hnd2
  | Four_of_Kind, _ -> 1
  | _, Four_of_Kind -> -1
  | Full_House, Full_House -> type_card_compare hnd1 hnd2
  | Full_House, _ -> 1
  | _, Full_House -> -1
  | Flush, Flush -> type_card_compare hnd1 hnd2
  | Flush, _ -> 1
  | _, Flush -> -1
  | Straight, Straight -> type_card_compare hnd1 hnd2
  | Straight, _ -> 1
  | _, Straight -> -1
  | Three_of_kind, Three_of_kind -> type_card_compare hnd1 hnd2
  | Three_of_kind, _ -> 1
  | _, Three_of_kind -> -1
  | Two_pair, Two_pair -> type_card_compare hnd1 hnd2
  | Two_pair, _ -> 1
  | _, Two_pair -> -1
  | Pair, Pair -> type_card_compare hnd1 hnd2
  | Pair, _ -> 1
  | _, Pair -> -1
  | High_card, High_card -> type_card_compare hnd1 hnd2

(** [player_hand_compare p1 p2 deck] returns a number representing the player 
    with the better hand of [p1] and [p2] when combined with the cards on the 
    table*)
let player_hand_compare p1 p2 deck = 
  let hnd1 = fst p1.cards :: snd p1.cards :: deck 
             |> List.sort compare_help 
             |> List.rev 
             |> make_hand in
  let hnd2 = fst p2.cards :: snd p2.cards :: deck 
             |> List.sort compare_help 
             |> List.rev 
             |> make_hand in
  hand_rank_compare hnd1 hnd2

(** [choose_winner winner players_tl table] is the player in [players_tl] 
    with the best hand. Returns previous player in case of a tie *)
let rec choose_winner winner players_tl table = 
  match players_tl with 
  | [] -> winner
  | head :: tail -> let w = player_hand_compare winner head table in 
    if w > 0 then choose_winner winner tail table 
    else if w < 0 then choose_winner head tail table
    (* else failwith "tie" *)
    else choose_winner head tail table