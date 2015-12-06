# CS3110-CardGame-ProjectTeam
Project Group for 3110

In order to run and compile our project, please run
"cs3110 compile -t -p core predefined.ml"
then
"cs3110 run predefined.ml -- pvp" if you want to play against a human
or
"cs3110 run predefined.ml -- VSai" if you want to play against the ai

If you want to use your own properly formatted .csv with cards in it,
(see testcards.csv for reference) you can run
"cs3110 compile -t -p core userdefined.ml"
then
"cs3110 run userdefined.ml -- testcards.csv pvp" if you want to play against a human
or
"cs3110 run userdefined.ml -- testcards.csv VSai" if you want to play against the ai