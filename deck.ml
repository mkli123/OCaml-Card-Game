(* TYPES **********************************************************************)

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
    (* draw a certain number of cards where bool represents self or opponent *)
    | Draw        of int
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
    stealth: bool ref;
    taunt : bool;
    ctype : ctyp;
    cost  : int;
}

(* card library with all possible cards *)
type clist  = card array
(* a list of cards to simulate the deck *)
type deck = card list


(******************************************************************************)

(* let cardlist_csv_name = Sys.argv.(1)*)

let split s =
    Core.Std.String.split s ' '
(* get_ctype s extracts card type from the string value *)
let get_ctype s =
    match s with
    | "protoss" -> Protoss
    | "zerg"    -> Zerg
    | "terran"  -> Terran
    | "spell"   -> Spell
    | _         -> Spell

(* parse_effect s extracts the etype from a string input *)
let rec parse_effect s =
    let lows = String.trim (String.lowercase s) in
    let infolst = split lows in
    match List.hd infolst with
    | "none"      -> None
    | "draw" ->
        Draw(int_of_string  (List.nth infolst 1))
    | "aoef" ->
        AoeF(int_of_string  (List.nth infolst 1),
             int_of_string  (List.nth infolst 2))
    | "aoee" ->
        AoeE(int_of_string  (List.nth infolst 1),
             int_of_string  (List.nth infolst 2))
    | "aoea" ->
        AoeA(int_of_string  (List.nth infolst 1),
             int_of_string  (List.nth infolst 2))
    | "btyp" ->
        let ct = List.nth infolst 1 in
        BType(get_ctype ct,
              int_of_string  (List.nth infolst 2),
              int_of_string  (List.nth infolst 3))
    | "bone" ->
        BOne(int_of_string  (List.nth infolst 1),
             int_of_string  (List.nth infolst 2))
    | _     -> failwith "Incorrect effect format"

let rec make_library clst cinfo =
    match cinfo with
    | []   -> Array.of_list clst
    | h::t ->
        let new_card =
            {name = (List.nth h 0);
            cost = int_of_string  (List.nth h 1);
            hp = ref (int_of_string  (List.nth h 2));
            atk = ref (int_of_string (List.nth h 3));
            effect = {description = (List.nth h 4);
                      efftype = parse_effect (List.nth h 5)};
            stealth = ref (bool_of_string  (String.lowercase (List.nth h 6)));
            taunt   = bool_of_string  (String.lowercase (List.nth h 7));
            ctype   = get_ctype (List.nth h 8);}
        in
        let new_clst = new_card::clst in
        make_library new_clst t

let empty_card () =
    {name = "emptycard";hp = ref 0; atk = ref 0;
     effect = {description=""; efftype=None}; stealth = ref false;
     taunt = false; ctype = Zerg;cost = -2 }

let import_cardlist f =
    let card_info = List.tl (Csv.load f) in
    make_library [] card_info

let draw d : (deck *card) option =
    match d with
    | []   -> None
    | h::t -> Some(t,h)

let shuffle d =
    let rec shuf d c =
        let n = Array.length d in
        if c = 0 then d
        else begin
            let r1 = Random.int n in
            let r2 = Random.int n in
            let temp = d.(r1) in
            d.(r1) <- d.(r2);
            d.(r2) <- temp;
            shuf d (c - 1)
        end
    in
    Array.to_list (shuf (Array.of_list d) 50)

let card_string c =
    if c.ctype = Spell then begin "Cost: "^
        (string_of_int c.cost)^"; Name: "^c.name^c.effect.description^"\n"
    end else
        "Cost: "^
        (string_of_int c.cost)^
        "; Name: "^
        c.name^
        "; Attack: "^
        (string_of_int !(c.atk))^
        "; HP: "^
        (string_of_int !(c.hp))^
        "; "^
        c.effect.description^
        "\n"
