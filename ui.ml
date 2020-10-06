open Card

(** Strings to represent different segments of each card. *)
let top = "\t┌─────────┐" 
let bbody = "\t│█████████│" 
let bottom = "\t└─────────┘" 

(** [char_of_val c] is the string representation of the value for [c]. *)
let char_of_val c = 
  let v = Card.card_value c in
  if v = 14 then "A "
  else if v = 13 then "K "
  else if v = 12 then "Q "
  else if v = 11 then "J "
  else if v = 10 then "10"
  else string_of_int v ^ " "

(** [char_of_suit c] is the string representation of the suit for [c]. *)
let char_of_suit c = 
  match Card.card_suit c with
  | Spade -> "♠"
  | Diamond -> "♦"
  | Heart -> "♥"
  | Club -> "♣"

(** [card_rep c] is a string list of each line for the representation of [c]. *)
let card_rep c = 
  let r1 = top in
  let r2 = "\t│"^char_of_val c^"       │" in
  let r3 = "\t│"^char_of_suit c^"        │" in
  let r4 = "\t│    "^char_of_suit c^"    │" in
  let r5 = "\t│        "^char_of_suit c^"│" in
  let r6 = "\t│       "^char_of_val c^"│" in
  let r7 = bottom in
  [r1;r2;r3;r4;r5;r6;r7]

(** [make_reps acc lis] is a string list list, where each element is [card_rep]
    for a card, and the entire list is the representations of each card in 
    [lis]. *)
let rec make_reps acc lis = 
  match lis with 
  | h :: t -> make_reps (card_rep h :: acc) t
  | [] -> acc

(** [string_of_reps r1 r2 r3 r4 r5 r6 r7 lis] is a string representing each
    card in [lis]. *)
let rec string_of_reps r1 r2 r3 r4 r5 r6 r7 lis = 
  match lis with
  | [] -> r1 ^ "\n" 
          ^ r2 ^ "\n" 
          ^ r3 ^ "\n" 
          ^ r4 ^ "\n" 
          ^ r5 ^ "\n" 
          ^ r6 ^ "\n" 
          ^ r7
  | h :: t -> begin match h with
      | x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: empty -> 
        string_of_reps (x1^r1) (x2^r2) (x3^r3) (x4^r4) (x5^r5) (x6^r6) (x7^r7) t
      | _ -> failwith "string of reps failure"
    end

let make_card_reps lis = 
  make_reps [] lis |> string_of_reps "" "" "" "" "" "" ""