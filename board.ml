open Deck
open Hero
(* open Ai *)
open Draft
open Command

(* keep track of current mana available to player at a given time
 * max is the mana you start with next turn
 *)
type mana  = {
    max     : int ref;
    current : int ref;
}

(* mode defines whether or not it is player vs player or player vs ai *)
type mode   = |VSai of bool |PVP

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
    pOneBoard : card option array;
    pTwoBoard : card option array;
    pOneDeck  : deck ref;
    pTwoDeck  : deck ref;
    hUsed     : bool ref;
    pOneMana  : mana;
    pTwoMana  : mana;
    turn      : int ref;
}


(* ******************************************************************** *)

let (>>=) v f =
  match v with
  | Some x -> f x
  | None   -> None

(* takes in deck ref and updates deck
	returns hand, updates the deck ref
	to reflect n draws
	TODO: ADD CARD BURN EDIT SPEC TO TAKE IN HAND

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
    let pOneD = if rand then ref d1 else ref d2 in
    let pTwoD = if rand then ref d2 else ref d1 in
    {
    mode      = m;
    pOneHero  = if rand then h1 else h2;
    pTwoHero  = if rand then h2 else h1;
    pOneHP    = ref 30;
    pTwoHP    = ref 30;
    pOneHand  = ref (draw_player pOneD 3);
    pTwoHand  = ref (draw_player pTwoD 4);
    pOneBoard = Array.make 7 (None : card option);
    pTwoBoard = Array.make 7 (None : card option);
    pOneDeck  = pOneD;
    pTwoDeck  = pTwoD;
    hUsed     = ref false;
    pOneMana  = {max = ref 0; current = ref 0;};
    pTwoMana  = {max = ref 0; current = ref 0;};
    turn      = ref 0;
    }
(*returns the first possible place to put a new minion. 8 if full*)
let indexOpening pBoard: int =
	let count = ref 8 in let _ =
	for x = 0 to Array.length pBoard - 1
	do
		if pBoard.(x) = (None:card option) then
		if x<(!count) then count:=x
	done in
	!count

let draw_card bS n : unit =
    let pTurn = ((!(bS.turn)) mod 2) in
    let chk = pTurn = 1 in
    let draw_a_card bSt =
        let plyrHP = if chk then bSt.pOneHP else bSt.pTwoHP in
        let plyrHand = if chk then !(bSt.pOneHand) else !(bSt.pTwoHand) in
        let plyrHandRef = if chk then bSt.pOneHand else bSt.pTwoHand in
        let plyrDeck = if chk then !(bSt.pOneDeck) else !(bSt.pTwoDeck) in
        let plyrDeckRef = if chk then bSt.pOneDeck else bSt.pTwoDeck in
        match plyrDeck with
        | [] -> plyrHP := (!plyrHP) - 2;
                Printf.printf "No cards remaining in your deck! -2 HP";
        | h::t when (List.length plyrHand) > 9 ->
            Printf.printf "Too many cards in hand!\nBurned card: %s"
                (card_string (List.hd (draw_player plyrDeckRef 1)));
        | h::t -> plyrHandRef := plyrHand@(draw_player plyrDeckRef 1);
    in
    for x = 0 to n do
        draw_a_card bS;
    done

let printBoard bS : unit =
    let pTurn = (!(bS.turn)) mod 2 in
    let chk = pTurn = 1 in
    let plyr = if chk then bS.pOneHero else bS.pTwoHero in
    let plyrHP = if chk then !(bS.pOneHP) else !(bS.pTwoHP) in
    let plyrHand = if chk then !(bS.pOneHand) else !(bS.pTwoHand) in
    let plyrB = if chk then bS.pOneBoard else bS.pTwoBoard in
    let opp = if chk then bS.pTwoHero else bS.pOneHero in
    let oppHP = if chk then !(bS.pTwoHP) else !(bS.pOneHP) in
    let oppHand = if chk then !(bS.pTwoHand) else !(bS.pOneHand) in
    let oppB = if chk then bS.pTwoBoard else bS.pOneBoard in
    let heroPower = if !(bS.hUsed) then "Unavailable" else "Available" in
    let prntCard (c:card option) i =
        match c with
        | None    -> ()
        | Some(k) -> Printf.printf "(%i) %s" i (card_string k);
    in
    let prntArray a i =
        prntCard a.(0) i;
        prntCard a.(1) (i + 1);
        prntCard a.(2) (i + 2);
        prntCard a.(3) (i + 3);
        prntCard a.(4) (i + 4);
        prntCard a.(5) (i + 5);
        prntCard a.(6) (i + 6);
    in
    let rec prntList l i =
        match l with
        | []   -> ()
        | h::t -> Printf.printf "(%i) %s" i (card_string h);
                  prntList t (i + 1);
    in
    Printf.printf "Enemy Hand: %i\n\n" (List.length oppHand);
    Printf.printf "Enemy Hero: (%i)" (100 * (1 + pTurn));
    Printf.printf " %s\n" (hero_string opp);
    Printf.printf "Enemy HP: %i\n" oppHP;
    Printf.printf "Enemy Board:\n";
    prntArray oppB 10;
    Printf.printf "\nYour Board:\n";
    prntArray plyrB 0;
    Printf.printf "Your HP: %i\n" plyrHP;
    Printf.printf "Your Hero: (%i)" (100 * (2 - pTurn));
    Printf.printf " %s\n" (hero_string plyr);
    Printf.printf "Hero Power: %s\n" heroPower;
    Printf.printf "\nYour Hand:\n";
    prntList plyrHand 0

(* returns array with stealth minions removed*)
let rm_stealth (brd: card option array) =
    let f (x:card option) =
        match x with
        | None    -> (None : card option)
        | Some(c: card) -> if !(c.stealth) then (None:card option) else Some(c)
    in
    Array.map f brd

let get_atk_c (co:card option) =
    match co with
    |None    -> 0
    |Some(x) -> !(x.atk)

let get_hp_c (co:card option) =
    match co with
    |None    -> 0
    |Some(x) -> !(x.hp)

let inputAttack boardState (c,e) : unit =
    let t = ref false in
    let f (x:card option) =
        match x with
        | None       -> (None : card option)
        | Some(card) -> if card.taunt && !(card.stealth) then Some(card)
                        else if card.taunt then( t:= true; Some(card))
                        else (None:card option) in
    (* sets t to true if there is a taunt and returns array of only
        taunted stuff then *)
    let check_taunt brd = let x = Array.map f brd in
        if !t then x else rm_stealth brd in

    let invalid_board () = Printf.printf "Invalid Attack...\n"; in
    try
        if (!(boardState.turn)) mod 2 = 1 then(
            let m = boardState.pOneBoard.(c) in
            let t_only = check_taunt boardState.pTwoBoard in
            if boardState.pOneBoard.(c) = None then invalid_board ()
            else
                if e = 200 && (not !t) then begin
                    if boardState.pOneBoard.(c) = None then invalid_board ()
                    else( boardState.pTwoHP := !(boardState.pTwoHP) - get_atk_c m;
                         match boardState.pOneBoard.(c) with |Some r -> r.stealth:=false
                         |None -> ())
                end
                else if t_only.(e) = None then invalid_board ()
                else let d1 = get_atk_c m in
                     let d2 = get_atk_c t_only.(e) in
                     let h1 = get_hp_c m in
                     let h2 = get_hp_c t_only.(e) in
                    match boardState.pOneBoard.(c),boardState.pTwoBoard.(e) with
                     | Some(x),Some(y) -> x.hp := h1 - d2; y.hp := h2 - d1;
                                          x.stealth := false;
                                          if !(x.hp) <= 0 then
                                          boardState.pOneBoard.(c) <- None;
                                          if !(y.hp) <= 0 then
                                          boardState.pTwoBoard.(e) <- None;
                     | _               -> invalid_board ()
            )else
            let m = boardState.pTwoBoard.(c) in
            let t_only = check_taunt boardState.pOneBoard in
            if boardState.pTwoBoard.(c) = None then invalid_board ()
            else
                if e = 100 && (not !t) then(
                    if boardState.pTwoBoard.(c) = None then invalid_board ()
                    else(boardState.pOneHP := !(boardState.pOneHP) - get_atk_c m;
                        match boardState.pOneBoard.(c) with |Some r -> r.stealth:=false
                         |None -> ())
                )
                else if t_only.(e) = None then invalid_board ()
                else let d1 = get_atk_c m in
                     let d2 = get_atk_c t_only.(e) in
                     let h1 = get_hp_c m in
                     let h2 = get_hp_c t_only.(e) in
                    match boardState.pTwoBoard.(c),boardState.pOneBoard.(e) with
                     | Some(x),Some(y) -> x.hp := h1 - d2; y.hp := h2 - d1;
                                          x.stealth := false;
                                          if !(x.hp) <= 0 then
                                          boardState.pTwoBoard.(c) <- None;
                                          if !(y.hp) <= 0 then
                                          boardState.pOneBoard.(e) <- None;
                     | _               -> invalid_board ()

    with
    | _ -> invalid_board ()


let inputEnd bS : unit =
    bS.turn := (!(bS.turn)) + 1;
    draw_card bS 1;
    bS.hUsed := false;
    let pTurn = (!(bS.turn)) mod 2 in
    let chk = pTurn = 1 in
    let plyrMana = if chk then bS.pOneMana else bS.pTwoMana in
    if (!(plyrMana.max)) < 10 then plyrMana.max := (!(plyrMana.max)) + 1;
    plyrMana.current := !(plyrMana.max)

let foption (c:card option) =
  match c with
  |Some(c)-> c
  |None -> empty_card()


let inputHPow boardState input : unit =
    if !(boardState.hUsed) = true then
        Printf.printf "You have already used your hero power this turn!\n"
    else
        let mana = if (!(boardState.turn) mod 2) = 0 then
        boardState.pTwoMana.current
        else
        boardState.pOneMana.current in
        if !mana <= 0 then Printf.printf "You don't have enough mana.\n"
        else
            let player = if (!(boardState.turn) mod 2) = 0 then
            boardState.pTwoHero.power else boardState.pOneHero.power in
            let moded = (!(boardState.turn) mod 2) in
            match player,input with
            |Mage,Some x -> if x = 100 then (if moded = 0 then( boardState.pTwoHP:=(!(boardState.pTwoHP)-1);boardState.pTwoMana.current:=
                                (!(boardState.pTwoMana.current)-2) )else boardState.pOneHP:=(!(boardState.pOneHP)-1);boardState.pOneMana.current:=
                                (!(boardState.pOneMana.current)-2)) else if x>=0 && x<7 then( let brd = (if moded = 0 then boardState.pTwoBoard else boardState.pOneBoard) in
                                let no_stealth = rm_stealth brd in (if no_stealth.(x) = None then
                                        Printf.printf "That's an invalid Target. \n" else (if !((foption brd.(x)).hp)-1 = 0 then brd.(x)<-None else (foption brd.(x)).hp:= (!((foption brd.(x)).hp) - 1));
                                        (if moded = 0 then boardState.pTwoMana.current:=(!(boardState.pTwoMana.current)-2) else
                                            boardState.pOneMana.current:=(!(boardState.pOneMana.current)-2))))
                                else if x>=10 && x<17 then ( let brd = (if moded = 0 then boardState.pOneBoard else boardState.pTwoBoard) in
                                let no_stealth = rm_stealth brd in (if no_stealth.(x-10) = None then
                                        Printf.printf "That's an invalid Target. \n" else (if !((foption brd.(x-10)).hp)-1 = 0 then brd.(x-10)<-None else (foption brd.(x-10)).hp:= (!((foption brd.(x-10)).hp) - 1));
                                        (if moded = 0 then boardState.pTwoMana.current:=(!(boardState.pTwoMana.current)-2) else
                                            boardState.pOneMana.current:=(!(boardState.pOneMana.current)-2))))
                                else if x = 200 then (if moded = 0 then( boardState.pOneHP:=(!(boardState.pOneHP)-1);boardState.pTwoMana.current:=
                                (!(boardState.pTwoMana.current)-2) )else boardState.pTwoHP:=(!(boardState.pTwoHP)-1);boardState.pOneMana.current:=
                                (!(boardState.pOneMana.current)-2)) else Printf.printf "Invalid Target.\n"
            |Paladin, _ -> let index = indexOpening (if moded=0 then boardState.pTwoBoard else boardState.pOneBoard) in
                            if index = 8 then
                                Printf.printf "Your board is full!\n"
                            else let new_card =
                            {name = "Silver Hand Recruit";
                            cost = 1;
                            hp = ref 1;
                            atk = ref 1;
                            effect = {description = "";
                                      efftype = None};
                            stealth = ref false;
                            taunt   = false;
                            ctype   = Zerg;} in
                            if (!(boardState.turn) mod 2) = 0 then(
                            boardState.pTwoBoard.(index) <- Some(new_card);
                            boardState.pTwoMana.current:=
                                (!(boardState.pTwoMana.current)-2))
                            else( boardState.pOneBoard.(index) <- Some(new_card))
            |Priest, Some x -> if x = 100 then (if moded = 0 then( boardState.pTwoHP:=(!(boardState.pTwoHP)+2);boardState.pTwoMana.current:=
                                (!(boardState.pTwoMana.current)-2) )else boardState.pOneHP:=(!(boardState.pOneHP)+2);boardState.pOneMana.current:=
                                (!(boardState.pOneMana.current)-2)) else if x>=0 && x<7 then( let brd = (if moded = 0 then boardState.pTwoBoard else boardState.pOneBoard) in
                                let no_stealth = rm_stealth brd in (if no_stealth.(x) = None then
                                        Printf.printf "That's an invalid Target. \n" else (foption brd.(x)).hp:= (!((foption brd.(x)).hp) + 2));
                                        (if moded = 0 then( boardState.pTwoMana.current:=((!(boardState.pTwoMana.current))-2) )else(
                                            boardState.pOneMana.current:=((!(boardState.pOneMana.current))-2))))
                                else if x>=10 && x<17 then ( let brd = (if moded = 0 then boardState.pOneBoard else boardState.pTwoBoard) in
                                let no_stealth = rm_stealth brd in (if no_stealth.(x-10) = None then
                                        Printf.printf "That's an invalid Target. \n" else (foption brd.(x-10)).hp:= (!((foption brd.(x-10)).hp) + 2));
                                        (if moded = 0 then( boardState.pTwoMana.current:=(!(boardState.pTwoMana.current)-2) )else(
                                            boardState.pOneMana.current:=(!(boardState.pOneMana.current)-2))))
                                else if x = 200 then (if moded = 0 then( boardState.pOneHP:=(!(boardState.pOneHP)+2);boardState.pTwoMana.current:=
                                (!(boardState.pTwoMana.current)-2) )else boardState.pTwoHP:=(!(boardState.pTwoHP)+2);boardState.pOneMana.current:=
                                (!(boardState.pOneMana.current)-2)) else Printf.printf "Invalid Target.\n"
            |Warlock, _ -> draw_card boardState 1; (if moded = 0 then( boardState.pTwoHP:=(!(boardState.pTwoHP)-2);boardState.pTwoMana.current:=
                                (!(boardState.pTwoMana.current)-2) )else( boardState.pOneHP:=(!(boardState.pOneHP)-2);boardState.pOneMana.current:=
                                (!(boardState.pOneMana.current)-2)))
            |_,_ -> Printf.printf "Invalid Hpow command.\n"


(* takes in [ind] index of card you want, [lst] hand you draw from
    acc is the start index
    returns: (card,newhand)*)
let rec get_crd ind lst acc =
    match lst with
    | _ when acc > 10    -> (empty_card (),[])
    | c::t when ind = acc ->
        let fil x = c <> x in
        (c,List.filter fil lst)
    | h::t                -> get_crd ind lst (acc + 1)
    |_ -> (empty_card (),[])

(* Applies buff of h and a to input board which is card array *)
let buff (brd: card option array) h a =
    let rec buff_c n =
        match brd.(n) with
            | None when n < 7     -> buff_c (n + 1);
            | Some(c)  when n < 7 -> c.hp := !(c.hp) + h;
                                    c.atk := !(c.atk) + a;
                                    if !(c.hp) <= 0 then brd.(n)<- None;
                                    buff_c (n + 1);
            | _ -> ()
    in
    buff_c 0
(* applies buff to all types on your side *)
let buff_type (brd:card option array) t h a =
     let rec buff_c n =
        match brd.(n) with
            | None when n < 7     -> buff_c (n + 1);
            | Some(c)  when n < 7 && (c.ctype = t)-> c.hp := !(c.hp) + h;
                                    c.atk := !(c.atk) + a;
                                    if !(c.hp) <= 0 then brd.(n)<- None;
                                    buff_c (n + 1);
            | _ -> ()
    in
    buff_c 0

(* takes in who's turn it is as a bool, true is p1
    then tries to buff that thing by either causing dmg or w.e *)
(* let buff_one who bs (ind: int option) x y =
    if who then
    (* for p1 *)
        match ind with
        | None    -> (None: unit option)
        | Some(c) -> match c with
                    | 100 -> if x < 0 && y = 0
                            then (bs.pOneHP:= !(bs.pOneHP) - x; Some(()))
                            else (None: unit option)
                    | 200 -> if x < 0 && y = 0
                            then (bs.pTwoHP:= !(bs.pTwoHP) - x; Some(()))
                            else None
                    | n   -> try
                                if (n < 7 && n >= 0) then(
                                match bs.pOneBoard.(n) with
                                | None -> (None: unit option)
                                | Some(d) ->
                                    if !(d.stealth) then (None:unit option) else(
                                    d.atk := !(d.atk) + y;
                                    d.hp := !(d.hp) + x;
                                    if (!(d.hp) <= 0)
                                    then( bs.pOneBoard.(n) <- None ; (Some () : unit option))))

                            else( if (n >= 10 && n < 17) then
                                match bs.pTwoBoard.(n-10) with
                                | None -> (None: unit option)
                                | Some(d) ->
                                    if !(d.stealth) then None else(
                                    d.atk := !(d.atk) + y;
                                    d.hp := !(d.hp) + x;
                                    if !(d.hp) <= 0
                                    then( bs.pTwoBoard.(n) <- None ; Some(()))))

                            with | _ -> None

    else
    (* for p2 *)
        match ind with
        | None    -> None
        | Some(c) ->begin match c with
                    | 100 -> if x < 0 && y = 0
                            then (bs.pTwoHP:= !(bs.pTwoHP) - x; Some(()))
                            else None
                    | 200 -> if x < 0 && y = 0
                            then (bs.pOneHP:= !(bs.pOneHP) - x; Some(()))
                            else None
                    | n   -> try
                                if (n < 7 && n >= 0) then
                                match bs.pTwoBoard.(n) with
                                | None -> None
                                | Some(d) ->
                                    if !(d.stealth) then None else
                                    d.atk := !(d.atk) + y;
                                    d.hp := !(d.hp) + x;
                                    if !(d.hp) <= 0
                                    then bs.pTwoBoard.(n) <- None ; Some(())

                            else if (n >= 10 && n < 17) then(
                                match bs.pOneBoard.(n-10) with
                                | None -> None
                                | Some(d) ->
                                    if !(d.stealth) then None else(
                                    d.atk := !(d.atk) + y;
                                    d.hp := !(d.hp) + x;
                                    if !(d.hp) <= 0
                                    then (bs.pOneBoard.(n) <- None ; Some(()))))

                            with | _ -> None
            end *)
let buff_one who bs (ind: int option) x y =
    if who then
    (* for p1 *)
        match ind with
        | None    -> false
        | Some(c) -> match c with
                    | 100 -> if x < 0 && y = 0
                            then (bs.pOneHP:= !(bs.pOneHP) - x; true)
                            else false
                    | 200 -> if x < 0 && y = 0
                            then (bs.pTwoHP:= !(bs.pTwoHP) - x; true)
                            else false
                    | n   -> try
                                if (n < 7 && n >= 0) then(
                                match bs.pOneBoard.(n) with
                                | None -> false
                                | Some(d) ->
                                    if !(d.stealth) then (false) else(
                                    d.atk := !(d.atk) + y;
                                    d.hp := !(d.hp) + x;
                                    if (!(d.hp) <= 0)
                                    then( bs.pOneBoard.(n) <- None); true))
                            else( if (n >= 10 && n < 17) then(
                                match bs.pTwoBoard.(n-10) with
                                | None -> false
                                | Some(d) ->
                                    if !(d.stealth) then false else(
                                    d.atk := !(d.atk) + y;
                                    d.hp := !(d.hp) + x;
                                    if !(d.hp) <= 0
                                    then( bs.pTwoBoard.(n) <- None); true))
                                else false)
                            with | _ -> false
    else
    (* for p2 *)
        match ind with
        | None    -> false
        | Some(c) ->begin match c with
                    | 100 -> if x < 0 && y = 0
                            then (bs.pTwoHP:= !(bs.pTwoHP) - x; true)
                            else false
                    | 200 -> if x < 0 && y = 0
                            then (bs.pOneHP:= !(bs.pOneHP) - x; true)
                            else false
                    | n   -> try
                                if (n < 7 && n >= 0) then(
                                match bs.pTwoBoard.(n) with
                                | None -> false
                                | Some(d) ->
                                    if !(d.stealth) then (false) else(
                                    d.atk := !(d.atk) + y;
                                    d.hp := !(d.hp) + x;
                                    if (!(d.hp) <= 0)
                                    then( bs.pTwoBoard.(n) <- None); true))
                            else( if (n >= 10 && n < 17) then(
                                match bs.pOneBoard.(n-10) with
                                | None -> false
                                | Some(d) ->
                                    if !(d.stealth) then false else(
                                    d.atk := !(d.atk) + y;
                                    d.hp := !(d.hp) + x;
                                    if !(d.hp) <= 0
                                    then( bs.pOneBoard.(n) <- None); true))
                                else false)
                            with | _ -> false
            end


let inputPCard bS (x,op) : unit =
    let invalid_i () = Printf.printf "Invalid play...\n"; in
    let who = !(bS.turn) mod 2 = 1 in
    let brd = if who then bS.pOneBoard else bS.pTwoBoard in
    let ind = indexOpening brd
    in
    (* check if card is in hand *)
    let hnd       = if who then bS.pOneHand else bS.pTwoHand in
    let temp_hand = !hnd in
    let pMana = if who then bS.pOneMana else bS.pTwoMana in
    if x >= List.length (!hnd) then invalid_i () else
    let new_hand = get_crd x temp_hand 0 in
    let play = fst new_hand in
    if play.cost > !(pMana.current) then invalid_i () else
    if who then
        match play.ctype,play.effect.efftype with
        | Spell,eff ->
            let mana_use () = pMana.current := !(pMana.current) - play.cost in(
            match eff with
            | None      -> mana_use ();
            | Draw(x) -> mana_use ();
                        draw_card bS x;
            | AoeF(x,y) -> mana_use ();
                        buff brd x y;
            | AoeE(x,y) -> mana_use ();
                        buff bS.pTwoBoard x y;
            | AoeA(x,y) -> mana_use ();
                        buff brd x y;
                        buff bS.pTwoBoard x y;
            | BType(ct,x,y) -> mana_use ();
                        buff_type brd ct x y;
            | BOne (x,y) ->
                try
                    if (buff_one who bS op x y) then () else invalid_i ()
                with | _ -> invalid_i ())

        | _ ,eff    ->
            let mana_use () = pMana.current := !(pMana.current) - play.cost; in
            (match eff with
            | None when ind <= 6 ->
                mana_use (); brd.(ind) <- Some(play);
            | Draw(x) when ind <= 6 ->
                mana_use (); draw_card bS x; brd.(ind) <- Some(play);
            | AoeF(x,y) when ind <= 6 ->
                mana_use (); buff brd x y; brd.(ind) <- Some(play);
            | AoeE(x,y) when ind <= 6 ->
                mana_use (); buff bS.pTwoBoard x y; brd.(ind) <- Some(play);
            | AoeA(x,y) when ind <= 6 ->
                mana_use (); buff brd x y; buff bS.pTwoBoard x y;
                brd.(ind) <- Some(play);
            | BType(ct,x,y) when ind <= 6 ->
                mana_use (); buff_type brd ct x y; brd.(ind) <- Some(play);
            | BOne (x,y) when ind <= 6 ->
                mana_use (); brd.(ind) <- Some(play);
                (try
                 if (buff_one who bS op x y) then () else invalid_i ()
                with | _ -> invalid_i ();)

            | _ -> invalid_i ();)
(* p2 turn *)
     else
        match play.ctype,play.effect.efftype with
        | Spell,eff ->
            let mana_use () = pMana.current := !(pMana.current) - play.cost; in
            begin match eff with
            | None      -> mana_use ();
            | Draw(x) -> mana_use ();
                        draw_card bS x;
            | AoeF(x,y) -> mana_use ();
                        buff brd x y;
            | AoeE(x,y) -> mana_use ();
                        buff bS.pOneBoard x y;
            | AoeA(x,y) -> mana_use ();
                        buff brd x y;
                        buff bS.pOneBoard x y;
            | BType(ct,x,y) -> mana_use ();
                        buff_type brd ct x y;
            | BOne (x,y) ->
                try
                    if (buff_one who bS op x y) then () else invalid_i ()
                with | _ -> invalid_i ()
            end
        | _ ,eff    ->
            let mana_use () = pMana.current := !(pMana.current) - play.cost in
            match eff with
            | None when ind <= 6 ->
                mana_use (); brd.(ind) <- Some(play)
            | Draw(x) when ind <= 6 ->
                mana_use (); draw_card bS x; brd.(ind) <- Some(play)
            | AoeF(x,y) when ind <= 6 ->
                mana_use (); buff brd x y; brd.(ind) <- Some(play)
            | AoeE(x,y) when ind <= 6 ->
                mana_use (); buff bS.pOneBoard x y; brd.(ind) <- Some(play)
            | AoeA(x,y) when ind <= 6 ->
                mana_use (); buff brd x y; buff bS.pOneBoard x y;
                brd.(ind) <- Some(play)
            | BType(ct,x,y) when ind <= 6 ->
                mana_use (); buff_type brd ct x y; brd.(ind) <- Some(play)
            | BOne (x,y) when ind <= 6 ->
                mana_use (); brd.(ind) <- Some(play);
                (try
                    if (buff_one who bS op x y) then () else invalid_i ()
                with | _ -> invalid_i ();)
            | _ -> invalid_i ()


let inputLookH boardState : unit =
	let player = if (!(boardState.turn) mod 2) = 0 then
		!(boardState.pTwoHand)
	else
		!(boardState.pOneHand) in
	let rec helper = function
	|[]->()
	|h::t-> Printf.printf "%s" (card_string h);helper t in
  Printf.printf "Your hand contains:\n";
	helper player

let inputConcede boardState : unit =
	let player = !(boardState.turn) mod 2 in
	let playernum = if player = 0 then "Player 1" else "Player 2" in
	Printf.printf "%s Wins!" playernum; exit 0

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

let actualGame (h1,d1) (h2,d2) m =
    let init = makeBoard (h1,d1) (h2,d2) m in

    let _ = inputEnd init in
    let rec repl bS : unit =
        printBoard bS;
        if (!(bS.pOneHP)) <= 0 then( Printf.printf "Player 2 wins!"; exit 0)
        else if (!(bS.pTwoHP)) <= 0 then( Printf.printf "Player 1 wins!"; exit 0);
        match bS.mode with
        | PVP
        | VSai(false) ->(
            match parse_game () with
            | Attack(x,y) -> inputAttack bS (x,y);
                             repl bS
            | End         -> inputEnd bS;
                             repl bS
            | HPow(x)     -> inputHPow bS x;
                             repl bS
            | PCard(x,y)  -> inputPCard bS (x,y);
                             repl bS
            | LookH       -> inputLookH bS;
                             repl bS
            | Concede     -> inputConcede bS;
                             repl bS
            | Help        -> inputGameHelp ();
                             repl bS)
        | VSai(true) -> failwith "todo"
    in
    repl init


let rec menu cardlist herolist deck1 deck2 pvp=
let user_input = parse_menu() in
match user_input with
|Draft ->(*  if pvp=PVP then *)( Printf.printf "Player 1 will Draft now.\n"; let d1 = build_deck 30 herolist cardlist in
            Printf.printf "Player 2 will Draft now.\n"; let d2 = build_deck 30 herolist cardlist in Printf.printf "Have Fun! \n"; actualGame d1 d2 pvp)
(*         else (Printf.printf "You will now Draft.\n"; let d1 = build_deck 30 herolist cardlist in let d2 = aiDraft herolist cardlist in actualGame d1 d2 pvp) *)
|Start -> actualGame deck1 deck2 pvp
|Exit -> Printf.printf "Thanks for playing! \n";exit 0
|Help ->( Printf.printf "Type Draft to initiate a draft.\n";
         Printf.printf "Type Start to play with predefined decks.\n";
         Printf.printf "Type Exit to quit the game. \n";
       menu cardlist herolist deck1 deck2 pvp)


