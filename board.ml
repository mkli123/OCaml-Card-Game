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
	pOneHand  = ref (snd (draw_player pOneD 3);
	pTwoHand  = ref (snd (draw_player pTwoD 4);
	pOneBoard = ref [];
	pTwoBoard = ref [];
	pOneDeck  = pOneD;
	pTwoDeck  = pTwoD;
	hUsed     = ref false;
	pOneMana  = ref 0;
	pTwoMana  = ref 0;
	turn      = ref 0;
	}











