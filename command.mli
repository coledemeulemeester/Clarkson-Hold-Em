(**
   Parsing of player commands.
*)

(** The type [amount] represents the amount of money that is associated with 
    the command verb. *)
type amount = int 

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an amount. *)
type command = 
  | Call
  | Quit
  | Raise of amount
  | Fold
  | Allin
  | Check

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the amount. The one 
    exception is if the verb is "all" in which case, it must be followed by the
    word "in".

    Examples: 
    - [parse "   raise      40   "] is [Raise 40]
    - [parse "quit"] is [Quit]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is considered malformed if the verb is not one of the following: "call", 
    "quit", "raise", "fold", "all in", or "check". It is also malformed if
    the verb "raise" is not followed by an amount (as a string) or if
    the verb is not "raise" and is followed by an amount (as a string). *)
val parse : string -> command