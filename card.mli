(** 
    Representation of playing cards for poker.
*)


(** The type [order] represents the concepts of strictly-less-than, 
    equal-to, and strictly greater-than. *)
type order = LT | EQ | GT

(** The type [suit_type] represents the suit of a card. This is either
    Club, Diamond, Heart, or Spade. *)
type suit_type = Club | Diamond | Heart | Spade

(** The type [face_value] represents the integer value of a card. 
    For non-face cards, that integer is the card number. For face-cards, 
    [face_value] is 11 for Jack, 12 for Queen, 13 for King, 14 for Ace. *) 
type face_value = int

(** The type of values representing cards. *) 
type card = {
  value: face_value; 
  suit: suit_type;
}

(** [card_value a] is the face value of [a]. *)
val card_value : card -> int 

(** [card_suit a] is the suit of [a]. *)
val card_suit : card -> suit_type

(** [compare c1 c2] is [GT] if [c1] is a higher value than [c2] or [LT] if [c2] 
    is higher than [c1]. If the face_value of [c1] and [c2] is the same, then
    the order of suit values is Spades, Hearts, Diamands, Clubs in order from 
    highest to lowest. *)
val compare : card -> card -> order

(** [to_string a] is the string representation of [a]. This will include
    the rank and suit of the card. For face cards, the word of the face should 
    be printed, ex. "Jack" instead of 11. *)
val to_string : card -> string





