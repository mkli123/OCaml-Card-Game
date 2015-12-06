open Board
open Draft
open Deck
open Hero

(* Given a board state, produce a string as an output that can be utilized as
the player command that would be utilized in order to execute the action
dictated by the AI algorithm. *)
let makeTurn bS =
    for i = 0 to 6 do
        inputAttack bS (i,(100 * (1 + pturn)));
    done
    for j = 0 to 9 do
        inputPCard bS j None;
    done
    inputHPow bS None;
    inputEnd


(* Generates a deck of cards using the same method as the method described for a
player draft. The AI is given the coice between three cards. It chooses one of
the three to add to its deck. This process is repeated until the deck is
completed. The resulting deck is then returned. *)
let aiDraft heros cards = 
    let h = Random.int (Array.length heros) in
    let d = ref [] in
    for i = 0 to 29 do
        d := (get_rand_card cards)::(!d);
    done;
    (h,(!d))