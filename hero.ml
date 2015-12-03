(* Types *)
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

(* ********************************************************* *)
let make_basic_heroes () =
	[|
	{name = "Jaina";power = Mage};
	{name = "Uther";power = Paladin};
	{name = "Anduin";power = Priest};
	{name = "Gul'dan";power = Warlock};
	|]

let hero_string h = 
	"Hero: "^
	h.name^
	match h.power with
	| Mage    -> 
		"; Hero Power: FIREBALL costs 2 mana and inflicts 1 damage to target."
	| Paladin -> 
		"; Hero Power: RECRUIT costs 2 mana summons a 1 / 1 Terran."
	| Priest  -> 
		"; Hero Power: HEAL costs 2 mana and heals a unit for 2HP."
	| Warlock -> 
		"; Hero Power: LIFETAP costs 2 health and draws a card."