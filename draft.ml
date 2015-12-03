open Deck
open Hero

(**
* [get_new_rand numbers range] returns a random number not in [numbers]
*
* Given a list of already used numbers, it chooses a new random number 
* in range
*)
let rec get_new_rand numbers range = 
	let rand = Random.int range in
	if List.mem rand numbers then 
	get_new_rand numbers range
	else
	rand
(**
* [get_rand_hero collection] returns a random hero chosen from [collection]
*
* Given a collection of all possible heros, it chooses one at random and 
* returns that hero. 
*)
let get_rand_hero heros : (hero*hero*hero) = 
	let h_size = Array.length heros in 
	let rand_num = Random.int h_size in 
	let rand_lst = [rand_num] in 
	let rand_num2 = get_new_rand rand_lst h_size in 
	let rand_lst2 = rand_num2::rand_lst in
	let rand_num3 = get_new_rand rand_lst2 h_size in
	(heros.(rand_num),heros.(rand_num2),heros.(rand_num3))


(**
* [select_card choice] returns a user chosen hero from [choice]
* [choice] is 3 cards chosen using get_rand_hero
*
* Allows the user to choose a hero to be their draft hero
*)
let rec select_hero (hero1,hero2,hero3) : hero =
	let hero1str = hero_string hero1 in
	let hero2str = hero_string hero2 in 
	let hero3str = hero_string hero3 in
	let _ = Printf.printf "Type in the number attached to the hero you want: \n";
	Printf.printf "1 %s \n" hero1str;
	Printf.printf "2 %s \n" hero2str;
	Printf.printf "3 %s \n" hero3str; in
	try let usrinp = read_int() in 
	if usrinp = 1 then hero1 else
	if usrinp = 2 then hero2 else
	if usrinp = 3 then hero3 else
	select_hero (hero1,hero2,hero3)
	with
	|_ = Printf.prinf "Type in a valid int"; select_hero (hero1,hero2,hero3)

(**
* [get_rand_card collection] returns a random card chosen from [collection]
*
* Given a collection of all possible cards, it chooses one at random and 
* returns that card. 
*)
let get_rand_card cards : (card*card*card) =
	let c_size = Array.length cards in 
	let rand_num = Random.int h_size in 
	let rand_lst = [rand_num] in 
	let rand_num2 = get_new_rand rand_lst h_size in 
	let rand_lst2 = rand_num2::rand_lst in
	let rand_num3 = get_new_rand rand_lst2 h_size in
	(cards.(rand_num),cards.(rand_num2),cards.(rand_num3))

(**
* [select_card choice] returns a user chosen card from [choice]
* [choice] is 3 cards chosen using get_rand_card
*
* Allows the user to choose a card to be added to their draft deck 
*)
let rec select_card (card1,card2,card3) : hero =
	let card1str = card_string card1 in
	let card2str = card_string card2 in 
	let card3str = card_string card3 in
	let _=Printf.printf "Type in the number attached to the card you want:\n";
	Printf.printf "1 %s \n" card1str;
	Printf.printf "2 %s \n" card2str;
	Printf.printf "3 %s \n" card3str; in
	try let usrinp = read_int() in 
	if usrinp = 1 then card1 else
	if usrinp = 2 then card2 else
	if usrinp = 3 then card3 else
	select_hero (card1,card2,card3)
	with
	|_ = Printf.prinf "Type in a valid int"; select_card (card1,card2,card3)
(**
* [build_deck size] runs [select_card] until a full deck is created
*
* This is the main function that allows the draft to take place. 
* The user selects from 3 random cards until his deck is built.  
*)
let build_deck size heros cards: (hero * deck) = 
	let hero_choices = get_rand_hero heros in 
	let chosen_hero = select_hero hero_choices in
	let rec helper size cards = 
		if size = 0 then [] else
		let card_choices = get_rand_card cards in 
		let chosen_card = select_card card_choices in
		chosen_card::(helper (size-1) cards) in
	let yourdeck = helper size cards in 
	(chosen_hero,yourdeck)


	

