(**
 * A hero_power is a unique ability that each hero has. Some examples of
 * hero_powers are "deal one damage to an enemy" and "draw a card at the
 * cost of some health"
 *)
type hero_power = | Mage | Paladin | Priest | Warlock
 (* A hero is the character you play as and has a unique ability
 *)

type hero = {
	name   :  string;
	power  :  hero_power;
}



(**
 * hlist is a list of all heros
 *)
type hlist = hero array

val hero_string : hero -> string