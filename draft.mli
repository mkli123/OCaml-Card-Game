open Deck
open Hero

(**
* [get_rand_hero collection] returns a random hero chosen from [collection]
*
* Given a collection of all possible heros, it chooses one at random and 
* returns that hero. 
*)
val get_rand_hero  : hero array -> hero

(**
* [select_card choice] returns a user chosen hero from [choice]
* [choice] is 3 cards chosen using get_rand_hero
*
* Allows the user to choose a hero to be their draft hero
*)
val select_hero  : (hero * hero * hero) -> hero

(**
* [get_rand_card collection] returns a random card chosen from [collection]
*
* Given a collection of all possible cards, it chooses one at random and 
* returns that card. 
*)
val get_rand_card : card array -> card

(**
* [select_card choice] returns a user chosen card from [choice]
* [choice] is 3 cards chosen using get_rand_card
*
* Allows the user to choose a card to be added to their draft deck 
*)
val select_card   : (card * card * card) -> card

(**
* [build_deck size] runs [select_card] until a full deck is created
*
* This is the main function that allows the draft to take place. 
* The user selects from 3 random cards until his deck is built.  
*)
val build_deck    : int -> (hero * deck)
