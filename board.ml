open deck
open hero
open ai
open draft
open command

(* keep track of current mana available to player at a given time
 * max is the mana you start with next turn
 *)
type mana  = {
	max     : int ref;
	current : int ref;
}

(* mode defines whether or not it is player vs player or player vs ai *)
type mode   = |VSai |PVP

(* board keeps track of all the different aspects of the game at the current
 * state.
 * mode keeps track of game mode
 * p___Hero keeps track of which hero each player is using
 * p___HP is the current hitpoints of player
 * p___Hand is the list of cards in a player's hand
 * p___Board is the cards currently on each side of the board
 * p___Deck is each respective player's current deck
 * hUsed is whether or not the player used their hero power that turn
 * p___Mana is the mana for each player
 * turn is 0 or 1 depending on who's player's turn it is
 *)
type board = {
	mode      : mode;
	pOneHero  : hero;
	pTwoHero  : hero;
	pOneHP    : int ref;
	pTwoHP    : int ref;
	pOneHand  : card list ref;
	pTwoHand  : card list ref;	
	pOneBoard : card list ref;
	pTwoBoard : card list ref;
	pOneDeck  : deck ref;
	pTwoDeck  : deck ref;
	hUsed     : bool ref;
	pOneMana  : mana ref;
	pTwoMana  : mana ref;
	turn      : int ref;
}


(* ******************************************************************** *)

(* takes in deck ref and updates deck
	returns hand, updates the deck ref 
	to reflect n draws
*)
let draw_player d n = 
	let rec d1 n dk h acc =
		match draw dk with
		| None                    -> ([],[])
		| Some(_,_) when acc >= n -> (dk,h)
		| Some(x,y) -> d1 n x (y::h) (acc + 1)
	in
	let x = (d1 n (!d) [] 0) in
	d := fst x;
	snd x

(* Need to figure out how to check who goes first *)
let makeBoard (h1,d1) (h2,d2) m =
	let rand = Random.bool () in
	let pOneD = if rand then d1 else d2 in
	let pTwoD = if rand then d2 else d1 in
	{
	mode      = m;
	pOneHero  = if rand then h1 else h2;
	pTwoHero  = if rand then h2 else h1;
	pOneHP    = ref 30;
	pTwoHP    = ref 30;
	pOneHand  = ref (draw_player pOneD 3);
	pTwoHand  = ref (draw_player pTwoD 4);
	pOneBoard = ref make 7 (empty_card ());
	pTwoBoard = ref make 7 (empty_card ());
	pOneDeck  = pOneD;
	pTwoDeck  = pTwoD;
	hUsed     = ref false;
	pOneMana  = {max = ref 0; current = ref 0;};
	pTwoMana  = {max = ref 0; current = ref 0;};
	turn      = ref 0;
	}

let printBoard boardState : unit =
	todo kevin

let inputAttack boardSate input : board =
	todo Mike

let inputEnd boardState input : board =
	todo kevin

let inputHPow boardState input : board =
	todo kevin

let inputPCard boardState input : board =
	todo mike

let inputLookH boardState : unit =
	let player = if (boardState.turn mod 2) = 0 then 
		boardState.pTwoHand 
	else
		boardState.pOneHand in 
	Printf.printf "Your hand contains:\n";
	let helper = function
	|[]->()
	|h::t-> Printf.printf "%s" (card_string h);helper t in
	helper player

let inputConcede boardState : unit =
	let player = boardState.turn mod 2 in
	let playernum = if player = 0 then "Player 1" else "Player 2" in
	Printf.printf "%s Wins!" playernum

let inputGameHelp () : unit =
	Printf.printf "Type attack # # where the first # is 
		your minion and the second # is the opponents minion.\n";
	Printf.printf "This uses your card to attack the opponents minion.\n";
	Printf.printf "Type end to complete your turn. \n";
	Printf.printf "Type Hpow # if your hero power can target someone\n";
	Printf.printf "If it doesn't target just type Hpow.\n";
	Printf.printf "Type pcard # to play the card # in your hand.\n";
	Printf.printf "Type lookh to see your hand.\n";
	Printf.printf "Type concede to give up! The 
		game ends and your opponent wins.\n";
	Printf.printf "Type help to... I think you know what typing help does."

let menu () = 
	todo all

let actualGame (h1,d1) (h2,d2) m =
	let init = makeBoard (h1,d1) (h2,d2) m in
	let rec repl boardState : board =
		match boardState.mode with
		| PVP -> 
	in