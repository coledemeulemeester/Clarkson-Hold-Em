(** 
   Controls computer moves in game.
*)

(** [ai_turn state] returns a [State.round] with the computer's turn 
    completed. Based on the cards the computer is dealt, it will determine 
    the probability of winning the round and based on that probability, it will
    either call, check, fold, or raise. *)
val ai_turn: State.round -> State.round