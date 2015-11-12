open Board
open Draft

(* Given a board state, produce a string as an output that can be utilized as
the player command that would be utilized in order to execute the action
dictated by the AI algorithm. *)
val MakeTurn : Board -> string

(* Generates a deck of cards using the same method as the method described for a
player draft. The AI is given the coice between three cards. It chooses one of
the three to add to its deck. This process is repeated until the deck is
completed. The resulting deck is then returned. *)
val AIDraft : unit -> deck