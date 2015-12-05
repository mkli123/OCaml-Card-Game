
(** Type of a card
 *  Spell is a spell, while Protoss,Zerg, and Terran cards are minions
 *)
type ctyp =
    | Protoss
    | Zerg
    | Terran
    | Spell

(** etype is the type of the effect where None means no
 * deathrattle or battlecry. Battlecry effects trigger upon
 * being played and Deathrattle triggers upon death of card
 *)
type etype =
    | None
    (* effect happens when card is played *)
    | BattleCry   of etype
    (* effect happens when card leaves board *)
    | DeathRattle of etype
    (* draw a certain number of cards where bool represents self or opponent *)
    | Draw        of bool * int
    (* buff all friendly cards hp and atk respectively *)
    | AoeF        of int * int
    (* buff all enemy cards hp and atk respectively *)
    | AoeE        of int * int
    (* buff all cards on the board hp and atk respectively *)
    | AoeA        of int * int
    (* buff all cards of type ctype on your side of board *)
    | BType       of ctyp * int * int
    (* buff one select minion *)
    | BOne        of int * int

(** The effect of a given card can have different types
 *
 *
 *)
type effect = {
    description : string;
    efftype     : etype;

}


(** A record representing all the information about a card.
 *  It contains the anme and basic stats of the card; hp, attack, effect
 *  The stealth and taunt fields are bools of whether a card has
 *  stealth or taunt at any given time.
 *  The effect
 *)
type card =
{
    name  : string;
    hp    : int ref;
    atk   : int ref;
    effect: effect;
    stealth: bool;
    taunt : bool;
    ctype : ctyp;
    cost  : int;
}

(* card library with all possible cards *)
type clist  = card array
(* a list of cards to simulate the deck *)
type deck = card list

(* [draw] [deck] returns a tuple with the rest of the deck and the top card
 * removed.
 *)
val draw : deck   -> (deck * card) option

(* [shuffle] [deck] shuffles the deck and returns a randomized card list
 *)
val shuffle: deck -> deck

val empty_card :unit -> card

val card_string: card -> string