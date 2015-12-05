open Board
open Deck
open Hero

let pvp a = failwith "todo"

let _ = let a = Sys.argv.(1) in let cardlist = import_cardlist "testcards.csv" in let herolist = make_basic_heroes () in let deck1 = Array.to_list (import_cardlist "deck1.csv") in let hero1 = Mage
in let deck2 = Array.to_list (import_cardlist "deck2.csv") in let hero2 = Paladin in menu cardlist herolist (hero1,deck1) (hero2,deck2) PVP;failwith "add ai functionality"