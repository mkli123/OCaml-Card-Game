(**
 * [menu_command] is the options users can decide between at the main menu
 * Draft is a mode in which a user chooses one of 3 cards until they have 
 * a full deck
 * Start begins a basic game mode in which previously defined decks are 
 * used. 
 * Exit leaves the game
 * Help provides a list of possible commands
 *)
type menu_command = 
|Draft
|Start
|Exit
|Help

(**
 * [game_command] is the options users can decide between in-game
 * Attack is the command to attack card [int] with card [int]
 * End concludes the user's turn 
 * HPow uses the hero power to either target card [int] or no card
 * PCard plays the card [int] from the users hand
 * LookH allows the user to look at their hand
 * Concede allows the player to give up when they think they can't win
 * Help provides a list of possible commands
 *)
type game_command = 
|Attack of (int * int)
|End
|HPow of int option
|PCard of int * int option
|LookH
|Concede
|Help

(**
 * [parse_menu] parses the user input at the menu and returns a menu command
 *)
val parse_menu : unit -> menu_command

(**
 * [parse_game] parses the user input in-game and returns a menu command
 *)
val parse_game : unit -> game_command