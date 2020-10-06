type amount = int
type command = 
  | Call
  | Quit
  | Raise of amount
  | Fold
  | Allin
  | Check

exception Empty

exception Malformed

(** [find_amount x] returns a copy of [x] with any empty strings removed.  *)
let rec find_amount = function
  | [] -> []
  | h :: t ->
    if (h = "") = false then h :: find_amount t
    else find_amount t

(** [is_valid_int string_int] returns true if [string_int] can be cast to 
    an integer. *)
let is_valid_int string_int = 
  if List.length string_int = 1 then 
    let x = int_of_string_opt (List.hd (string_int)) 
    in begin match x with 
      | Some a -> true
      | None -> false end else false

(**  [find_command lis] is the [command] that can be extracted
     from [lis]. Also verifies that the command is not malformed. If command
     is Raise, then calls [find_amount lis] to find the amount to raise. *)
let rec find_command = function
  | [] -> raise Empty
  | h :: t -> let lowercase = String.lowercase_ascii h in
    if lowercase = "" then raise Empty
    else if lowercase = "raise" && (t != []) && is_valid_int (find_amount t) 
    then Raise (int_of_string (List.hd (find_amount t)))
    else if lowercase = "quit" && t = [] then Quit
    else if lowercase = "fold" && t = [] then Fold
    else if lowercase = "call" && t = [] then Call
    else if lowercase = "check" && t = [] then Check
    else if lowercase = "all" && t = ["in"] then Allin
    else raise Malformed

let parse s = 
  let words = String.trim s in
  words |> String.split_on_char ' ' |> find_command
