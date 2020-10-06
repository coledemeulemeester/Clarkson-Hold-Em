type order = LT | EQ | GT

type suit_type = Club | Diamond | Heart | Spade

type face_value = int

type card = {
  value: face_value; 
  suit: suit_type;
}

let card_value c =
  c.value 

let card_suit c = 
  c.suit

let compare (c1 : card) (c2 : card) = 
  let x = compare c1.value c2.value in
  if x > 0 then GT
  else if x < 0 then LT
  else match c1.suit, c2.suit with
    | Spade, Spade -> EQ
    | Spade, _ -> GT
    | _, Spade -> LT
    | Heart, Heart -> EQ
    | Heart, _ -> GT
    | _, Heart -> LT
    | Diamond, Diamond -> EQ
    | Diamond, _ -> GT
    | Club, Club -> EQ
    | _ , _ -> LT

(** [face_value_string c] is the numerical value of a card formatted as a 
    string or the kind of face card or an Ace. *)
let face_value_string c = 
  if c.value = 14 then "Ace" 
  else if c.value = 13 then "King"
  else if c.value = 12 then "Queen"
  else if c.value = 11 then "Jack"
  else string_of_int c.value

let to_string c =
  (face_value_string c) ^ " of " ^ begin match c.suit with 
    | Spade -> "Spades" 
    | Heart -> "Hearts"
    | Diamond -> "Diamond"
    | Club -> "Club" 
  end
