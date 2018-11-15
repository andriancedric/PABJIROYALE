start :- 
	write(' ______   _     _ ______   ______ '), nl,
	write('(_____ \\ | |   | (____  \\ / _____)'), nl,
	write(' _____) )  |   | |____)  ) /  ___ '), nl,
	write('|  ____/ | |   | |  __  (| | (___)'), nl,
	write('| |      | |___| | |__) \\ (____/ '), nl,
	write('|_|      \\_______|______/ (_____/ '), nl,
	write('Welcome to PUBG Royale.'), nl,
	write('Choose your destiny, here.'), nl,
	write('BEWARE!'), nl,
	write('Never do one wrong move.'), nl,
	write('And you will be remembered as a victor.'), nl,
	write('Only the fittest, survive.'), nl, nl,

	repeat,
	input, 
	read(X),
	do(X).

input :- write('>>').

help :- 
	write('Available commands : '), nl,
	write('1. start. -- Start the game.'), nl,
	write('2. help. -- Show availabe commands.'), nl,
	write('3. quit. -- Quit the game.'), nl,
	write('4. look. -- Look everything around you (showing 3x3 map).'), nl,
	write('5. n. s. e. or w. -- Move (north, south, east, or west).'), nl,
	write('6. map. -- Look at the map and detect enemies.'), nl,
	write('7. take(Object). -- Pick up an object beside you.'), nl,
	write('8. drop(Object). -- Drop an object from you.'), nl,
	write('9. use(Object). -- Use an object.'), nl,
	write('10. attack. -- Attack an enemy beside you'), nl,
	write('11. status. -- Show yout status.'), nl,
	write('12. save(File_Name). -- Save your game to a file.'), nl,
	write('13. load(File_Name). -- Load previously saved game.'), nl.

quit :- 
	write('Thank you for playing.'), nl,
	write('We are waiting for the next victor!'), nl,
	write('PUBGRoyale v.0.1, 26 November 2018'), nl,
	write('CONTRIBUTORS : '), nl,
	write('1. Andrian Cedric'), nl,
	write('2. Kevin Sendjaja'), nl,
	write('3. Hansen'), nl,
	write('4. Abel Stanley'), nl.

do(help) :- help, repeat, input, read(X), do(X).
do(quit) :- quit, !.
do(_) :- write('Invalid command'), nl, repeat, input, read(X), do(X).



