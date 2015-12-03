
(* let cardlist_csv_name = Sys.argv.(1) *)

(* get_val opt extracts the value from option some *)
let get_val opt = 
    match opt with
    | None      -> ""
    | Some(str) -> str

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
    let lows = String.lowercase s in
    let infolst = String.split lows ' ' in
    match get_val List.hd with
    | "none"      -> None
    | "battlecry" -> 
        let spc = (String.index lows ' ' ) + 1 in
        let len = (String.length str) - spc in  
        BattleCry(parse_effect (String.sub lows spc len))
    | "deat" -> 
        let spc = (String.index lows ' ' ) + 1 in
        let len = (String.length str) - spc in
        DeathRattle(parse_effect (String.sub lows spc len))
    | "draw" -> 
        Draw(bool_of_string (get_val (List.nth infolst 1)),
            int_of_string (get_val (List.nth infolst 2)))
    | "aoef" ->
        AoeF(int_of_string (get_val (List.nth infolst 1)),
             int_of_string (get_val (List.nth infolst 2)))
    | "aoee" ->
        AoeE(int_of_string (get_val (List.nth infolst 1)),
             int_of_string (get_val (List.nth infolst 2)))
    | "aoea" ->
        AoeA(int_of_string (get_val (List.nth infolst 1)),
             int_of_string (get_val (List.nth infolst 2)))
    | "btyp" -> 
        let ct = get_val List.nth infolst 1 in
        BType(get_ctype ct,
              int_of_string (get_val (List.nth infolst 2)),
              int_of_string (get_val (List.nth infolst 3)))
    | "bone" ->
        BOne(int_of_string (get_val (List.nth infolst 1)),
             int_of_string (get_val (List.nth infolst 2)))


let rec make_library clst cinfo =
    match cinfo with
    | []   -> Array.of_list clst
    | h::t ->
        let new_card =
            {name = get_val (List.nth h 0);
            cost = int_of_string (get_val (List.nth h 1));
            hp = int_of_string (get_val (List.nth h 2));
            atk = get_val (List.nth h 3);
            effect = {description = get_val (List.nth h 4);
                      efftype = parse_effect (List.nth h 5)};
            stealth = bool_of_string (get_val (List.nth h 6));
            taunt   = bool_of_string (get_val (List.nth h 7));
            ctype   = get_ctype (List.nth h 8);}
        in
        let new_clst = new_card::clst in
        make_library new_clst t



let import_cardlist f =
    let card_info = get_val (List.tl (csv.load f)) in
    make_library [] card_info

let draw d =
    match d with
    | []   -> None
    | h::t -> Some(h,t)

let shuffle d =
    failwith "todo"
