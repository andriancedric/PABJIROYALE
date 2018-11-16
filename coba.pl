/*Deklarasi data dinamis*/
:- dynamic(game_on/1).	game_on(false).
:- dynamic(game_set/1). game_set(false).

:- dynamic(player_health/1).	player_health(0).




/*Inisialisasi*/
value(player_hp,100,200).

initPlayer :-
	/*Status*/
	player_health(Health),
	write('status ok'),nl,
	/*Hapus nilai*/
	retract(player_health(Health)),
	write('retract ok'),nl,
	/*Inisialisasi random num*/
	value(player_hp,Init_hp_min,Init_hp_max),
	write('set value ok'),nl,
	/*randomize,*/
	write('randomize ok'),nl,
	random(Init_hp_min,Init_hp_max,Hp_baru),
	write('random ok'),nl,
	/*asserta random num ke status*/
	asserta(player_health(Hp_baru)),
	write('assert ok'),nl.

init :-
	/*Game belum di-start*/
	game_on(false),
	!,
	write('start. dulu boi...'),nl.

init :-
	/*Game sudah di-start dan (asumsi) data sudah diinisialisasi*/
	game_on(true),
	!,
	game_set(true),
	!,
	write('BEGIN'),nl.



/*new game & load game*/
new_game :-
	/*belum start*/
	game_on(false),
	!,
	write('start. dulu :"v'),nl.

new_game :-
	/*sudah start*/
	game_on(true),
	!,
	write('loading...'),nl,
	initPlayer.


/*Deklarasi modifikasi*/
modif_player_health(Modif) :-
	player_health(X),
	retract(player_health(X)),
	ModHealth is X + Modif,
	asserta(player_health(ModHealth)).


/*Save*/
save(File_Name) :-
	open(File_Name,write,Stream),

	player_health(Health),

	write(Stream,Health),	write(Stream,'.'),nl(Stream),

	write('Saved'),nl,
	close(Stream).
/*Main*/
start :-
	/*belum start*/
	game_on(false),
	!,
	retract(game_on(false)),
	asserta(game_on(true)),
	/*write('start sukses').*/
	title,
	write('load. to load game, new_game. to start from the beginning').

start :-
	/*sudah start*/
	game_on(true),
	write('fokus mas'),nl.

title :- 
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
	write('Only the fittest, survive.'), nl, nl.

	/*repeat,
	input, 
	read(X),
	do(X),
	X == 'quit'.*/

input :- write('>> ').

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

status :- 
	player_health(Health),
	format("Health : ~p~n",[Health]).

quitgame :- 
	save('pubg.txt'),
	write('Thank you for playing.'), nl,
	write('We are waiting for the next victor!'), nl,
	write('PUBGRoyale v.0.1, 26 November 2018'), nl,
	write('CONTRIBUTORS : '), nl,
	write('1. Andrian Cedric'), nl,
	write('2. Kevin Sendjaja'), nl,
	write('3. Hansen'), nl,
	write('4. Abel Stanley'), nl.

do(help) :- help, !.
do(quit) :- quitgame, !.
do(_) :- write('Invalid command'),nl, !.
