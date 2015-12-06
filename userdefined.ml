open Board
open Deck
open Hero

let pvp a = if a = "pvp" then PVP else VSai(true)

let _ = let a = Sys.argv.(2) in let cardlist = import_cardlist (Sys.argv.(1)) in let herolist = make_basic_heroes () in let deck1 = Array.to_list (import_cardlist "testcards.csv") in let hero1 = herolist.(0)
in let deck2 = Array.to_list (import_cardlist "testcards.csv") in let hero2 = herolist.(1) in menu cardlist herolist (hero1,deck1) (hero2,deck2) (pvp a)