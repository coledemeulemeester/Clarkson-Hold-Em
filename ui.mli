(**
   Formatting for ASCII art in command line.
*)

(** [make_card_reps lis] is the string representation of the cards in [lis]
    to be printed to the command line for aesthetic. 
*)
val make_card_reps : Card.card list -> string