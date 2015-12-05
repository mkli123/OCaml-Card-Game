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
	pOneMana  : mana;
	pTwoMana  : mana;
	turn      : int ref;
}

(* [makeBoard] takes in the two hero deck tuples and creates the 
 * starting board state
 *)
val makeBoard      : (hero * deck) -> (hero * deck) -> mode -> board

(* [actualGame] acts as a main funciton that starts the command parsing
 *)
val actualGame     : Unit -> Unit