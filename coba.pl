/*Deklarasi data dinamis*/
:- dynamic(game_on/1).	game_on(false).
:- dynamic(game_set/1). game_set(false).

:- dynamic(player_health/1).	player_health(0).
:- dynamic(player_weapon/1).	player_weapon(bare_hand).
:- dynamic(player_position/2).	player_position(0,0).
:- dynamic(world_width/1).		world_width(0).
:- dynamic(world_length/1).		world_length(0).
/*:- dynamic(ammo/1).				ammo(0).*/

/*Deklarasi map*/
map_width(12).
map_height(12).


/*Inisialisasi*/
value(player_hp,100,200).
value(player_weapon,0,25).

initPlayer :-
	/*Status*/
	map_width(MW),
	map_height(MH),
	MW1 is MW - 1,
	MH2 is MH - 1,
	write('map set'),
	player_health(Health),
	player_weapon(Weapon),
	write('status ok'),nl,
	/*Hapus nilai*/
	retract(player_health(Health)),
	retract(player_weapon(Weapon)),
	write('retract ok'),nl,
	/*Inisialisasi random num*/
	value(player_hp,Init_hp_min,Init_hp_max),
	write('set value ok'),nl,
	random(Init_hp_min,Init_hp_max,Hp_baru),
	write('random ok'),nl,
	/*asserta random num ke status*/
	asserta(player_health(Hp_baru)),
	asserta(player_weapon(bare_hand)),
	write('assert ok'),nl.

initMap :-
	/*Generate map*/
	map_height(MH),
	initMap_row(MH),
	create_border, create_forest, create_river, create_hill.

initMap_row(Row) :-
	/*Inisialisasi plain map*/
	Row == 0,
	!.

initMap_row(Row) :-
	/*Rekursif*/
	map_width(MW),
	create_new_map(Row, 1, MW),
	!,
	New_row is Row - 1,
	initMap_row(New_row).

create_new_map(_,_,Dist) :-
	Dist == 0,
	!.

create_new_map(Row, Col, Dist) :-
	map_height(MH),
	Row =< MH,
	map_width(MW),
	Col =< MW,
	New_row is Row,
	New_col is Col + 1,
	New_dist is Dist - 1,
	asserta(map(plain, Row, Col)),
	!,
	create_new_map(New_row, New_col, New_dist),
	!.

init :-
	/*Game belum di-start*/
	game_on(false),
	!,
	write('start. dulu boi...'),nl.

init :-
	/*Game sudah di-start dan (asumsi) data sudah diinisialisasi*/
	game_on(true),
	!,
	game_set(false),
	!,
	retract(game_set(false)),
	asserta(game_set(true)),

	initMap,
	save('pubg.txt'),
	write('initialized...').

init :-
	game_on(true),
	!,
	game_set(true),
	!,
	write('BEGIN').

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
	initPlayer,
	init.

load :-
	/*belum start*/
	game_on(false),
	!,
	write('start dulu'),nl.

load :-
	/*sudah start*/
	game_on(true),
	!,
	loadd('pubg.txt'),
	init.


/*Deklarasi modifikasi*/
modif_player_health(Modif) :-
	player_health(X),
	retract(player_health(X)),
	ModHealth is X + Modif,
	asserta(player_health(ModHealth)).

modif_player_weapon(Modif) :-
	player_weapon(X),
	retract(player_weapon(X)),
	asserta(player_weapon(Weapon_baru)).


/*Save*/
save(File_Name) :-
	open(File_Name,write,Stream),

	player_health(Health),
	player_weapon(Weapon),

	write(Stream,Health),	write(Stream,'.'),nl(Stream),
	write(Stream,Weapon),	write(Stream,'.'),nl(Stream),

	write('Saved'),nl,
	close(Stream).

/*Load*/
loadd(File_Name) :-
	write('loading...'),nl,
	open(File_Name,read,Stream),

	player_health(Health),
	player_weapon(Weapon),
	write('unpacking resources...'),nl,

	retract(player_health(Health)),
	retract(player_weapon(Weapon)),
	write('retract success...'),nl,
	read(Stream, Hp_baru),
	read(Stream, Weapon_baru),
	write('reading data success...'),nl,
	asserta(player_health(Hp_baru)),
	asserta(player_weapon(Weapon)),
	write('assert success...'),nl,
	
	write('loaded'),nl,
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

/*DISPLAY*/
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
	/*player_weapon(Weapon),*/
	format("Health : ~p~n",[Health]),
	format("Weapon : ~p~n",[Weapon]).

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

/*MAP*/
write_map(Row,Col) :-
	/*plain*/
	map(plain, Row, Col),
	!,
	format(' - ',[]).

write_map(Row,Col) :-
	/*border*/
	map(border, Row, Col),
	!,
	format(' # ',[]).

write_all_map(Row, Col) :-
	/*basis*/
	map_width(MW),
	map_height(MH),
	Row == MH,
	Col == MW,
	!,
	write_map(Row, Col),
	!.

write_all_map(Row, Col) :-
	/*write col on first row*/
	map_width(MW),
	write_map(Row, Col),
	Col < MW,
	New_col is Col + 1,
	!,
	write_all_map(Row, New_col).

write_all_map(Row, Col) :-
	/*write col on next row*/
	map_width(MW),
	Col == MW,nl,nl,
	New_row is Row + 1,
	!,
	write_all_map(New_row, 1).


do(help) :- help, !.
do(quit) :- quitgame, !.
do(_) :- write('Invalid command'),nl, !.