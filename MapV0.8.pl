:- dynamic(gamemap/1).
:- dynamic(deathclock/1). deathclock(0).
:- dynamic(threatlvl/1). threatlvl(0).
:- dynamic(player_position/2).  player_position(2,2).
:- dynamic(player_weapon/1). player_weapon(watergun).
:- dynamic(player_bag/2).  player_bag(5,[]).
:- dynamic(mapEff_row/1). mapEff_row(10).
:- dynamic(mapEff_col/1). mapEff_col(10).
:- dynamic(map_row/1). map_row(12).
:- dynamic(map_col/1). map_col(12).

:-dynamic(enemy/2). % enemy(Type, HP, Weapon, X, Y)
:-dynamic(enemynumber/1). enemynumber(0).

:- dynamic(minimaps/1). minimaps([[[]]]).
:- dynamic(gamemap/1).
gamemap([
    ['X','X','X','X','X','X','X','X','X','X','X','X'],
    ['X','-','-','-','-','-','-','-','-','-','-','X'],
    ['X','-','-','-','-','-','-','-','-','-','-','X'],
    ['X','-','-','-','-','-','-','-','-','-','-','X'],
    ['X','-','-','-','-','-','-','-','-','-','-','X'],
    ['X','-','-','-','-','-','-','-','-','-','-','X'],
    ['X','-','-','-','-','-','-','-','-','-','-','X'],
    ['X','-','-','-','-','-','-','-','-','-','-','X'],
    ['X','-','-','-','-','-','-','-','-','-','-','X'],
    ['X','-','-','-','-','-','-','-','-','-','-','X'],
    ['X','-','-','-','-','-','-','-','-','-','-','X'],
    ['X','X','X','X','X','X','X','X','X','X','X','X']
    ]).

/*Object*/
init_object :-
    	asserta(object(weapon,gun,3,3)),
    	asserta(object(weapon,pan,4,4)),
    	asserta(object(weapon,ak47,3,5)),
    	asserta(object(armor,light,4,5)),
    	asserta(object(armor,heavy,6,2)),
    	asserta(object(medicine,tolakangin,5,5)),
    	asserta(object(medicine,panadol,4,3)),
    	asserta(object(medicine,aspirin,5,6)).

%SWI-GNU PROLOG COMPATIBILITY RULES :----------------------------------

      nth0(0, [Head|Tail], Head, Tail) :- !.

      nth0(N, [Head|Tail], Elem, [Head|Rest]) :-
          nonvar(N),
          M is N-1,
          nth0(M, Tail, Elem, Rest).

      nth0(N, [Head|Tail], Elem, [Head|Rest]) :-  % Clause added KJ 4-5-87
          var(N),                 % to allow mode
          nth0(M, Tail, Elem, Rest),      % nth0(-,+,+,?).
          N is M+1.


      nth1(1, [Head|Tail], Head, Tail) :- !.

      nth1(N, [Head|Tail], Elem, [Head|Rest]) :-
          nonvar(N),
          M is N-1,
          nth1(M, Tail, Elem, Rest).

      nth1(N, [Head|Tail], Elem, [Head|Rest]) :-  % Clause added KJ 4-5-87
          var(N),                 % to allow mode
          nth1(M, Tail, Elem, Rest),      % nth1(-,+,+,?).
          N is M+1.

%(END) SWI-GNU PROLOG COMPATIBILITY RULES :----------------------------------



%Matrix Rules :

  %RIBBON OF CHAR 'X' (FOR TOP AND BOTTOM OF MAP) -----------------------
    xribbon(0) :- nl.
    xribbon(N) :-
      write('X'), NextN is N-1, xribbon(NextN).

  %BUILD MATRIX       -----------------------

    make_num_matrix(N, Matrix) :-
        make_matrix(N, N, Matrix).

    % HELP : make_matrix(NUMBER OF ROWS, COLS, INIT.VAL, RESULT)
    make_matrix(_, N, _, []) :-
        N =< 0,
        !.
    make_matrix(M, N, X, [R|Rs]) :-
        make_list(M,X, R),
        N2 is N - 1,
        make_matrix(M, N2, X, Rs).

    make_list(N,_, []) :-
        N =< 0,
        !.

    make_list(N,X, [X|Rest]) :-
        N > 0,
        N2 is N - 1,
        make_list(N2,X, Rest).

  %MAKE MINI MAPS ----------------------------------------------

    initMM(Rows,Cols) :- makeMiniMaps(Rows,Cols,Result), retractall(minimaps(_)), asserta(minimaps(Result)).

    %IMPORTANT LESSON : WHEN CREATING LIST, HAVE TO TERMINATE LAST RESULT WITH []
    makeMiniMaps(Rows,_,[]) :- map_row(X), Rows >X.
    makeMiniMaps(Rows,Cols,[H|T]) :-
                                     makeMiniMapsR(Rows, Cols, H),
                                     NRows is Rows +1,
                                     makeMiniMaps(NRows, Cols, T).


    makeMiniMapsR(_,Cols,[]):- map_col(Y), Cols >Y.
    makeMiniMapsR(Rows,Cols,[MM|R]) :-
                                       makeMiniMap(Rows,Cols,MM),
                                       NCols is Cols+1,
                                       makeMiniMapsR(Rows,NCols,R).

    %IMPORTANT : X & Y represents ROW AND COLUMNS! not X and Y as in cartesian graph!!!
      % HELP >>> {index X, index Y (from big map), MiniMatrix}
      makeMiniMap(X,Y,MResult) :- gamemap(M), at(M,X,Y,Val), Val == 'X', make_matrix(3,3,'X',MResult).
      makeMiniMap(X,Y,MM5) :- gamemap(M),
                              make_matrix(3,3,'-',MM),
                              Yleft is Y-1, Yright is Y+1, Xdown is X+1, Xup is X-1,
                              ((at(M,X,Yright,Val), Val == 'X') -> makecolX(MM,3,MM2); append([],MM,MM2)),
                              ((at(M,Xup,Y,Val), Val == 'X')-> replace_row(MM2,1,'X',MM3); append([],MM2,MM3)),
                              ((at(M,X,Yleft,Val), Val == 'X')->makecolX(MM3,1,MM4);append([],MM3,MM4)),
                              ((at(M,Xdown,Y,Val), Val == 'X')-> replace_row(MM4,3,'X',MM5);append([],MM4,MM5)).
                              %DEBUGGER :printRows(MM5,1).

  %CHECK FOR ENEMY ! : ----------------------------------------------
      positionmatchE :- player_position(X,Y), enemy(_,_,_,X2,Y2), (X == X2, Y == Y2) ,!.
      %SPECIFIC MATCH :
      positionmatchES(X,Y) :-  enemy(_,_,_,X2,Y2), (X == X2, Y == Y2) ,!.
      checkEnemy(MM,MM10):- gamemap(M),player_position(X,Y), Yleft is (Y-1), Yright is (Y+1), Xdown is (X+1), Xup is (X-1),
                              ((at(M,X,Yright,Val), Val == 'E') -> placeEnemyMM(MM,2,3,MM2); append([],MM,MM2)),
                              ((at(M,Xup,Y,Val), Val == 'E')-> placeEnemyMM(MM2,1,2,MM3); append([],MM2,MM3)),
                              ((at(M,X,Yleft,Val), Val == 'E')->placeEnemyMM(MM3,2,1,MM4);append([],MM3,MM4)),
                              ((at(M,Xdown,Y,Val), Val == 'E')-> placeEnemyMM(MM4,3,2,MM5);append([],MM4,MM5)),
                              (positionmatchE -> placeEnemyMM(MM5,2,2,MM6);append([],MM5,MM6)),
                              ((at(M,Xup,Yright,Val), Val == 'E') -> placeEnemyMM(MM6,1,3,MM7); append([],MM6,MM7)),
                              ((at(M,Xdown,Yright,Val), Val == 'E')-> placeEnemyMM(MM7,3,3,MM8); append([],MM7,MM8)),
                              ((at(M,Xup,Yleft,Val), Val == 'E')->placeEnemyMM(MM8,1,1,MM9);append([],MM8,MM9)),
                              ((at(M,Xdown,Yleft,Val), Val == 'E')-> placeEnemyMM(MM9,3,1,MM10);append([],MM9,MM10)).

    checkEnemyF(MM,MM10):-    player_position(X,Y), Yleft is (Y-1), Yright is (Y+1), Xdown is (X+1), Xup is (X-1),
                            ((positionmatchES(X,Yright))-> placeEnemyMM(MM,2,3,MM2); append([],MM,MM2)),
                            ((positionmatchES(Xup,Y))-> placeEnemyMM(MM2,1,2,MM3); append([],MM2,MM3)),
                            ((positionmatchES(X,Yleft))->placeEnemyMM(MM3,2,1,MM4);append([],MM3,MM4)),
                            ((positionmatchES(Xdown,Y))-> placeEnemyMM(MM4,3,2,MM5);append([],MM4,MM5)),
                            (positionmatchE -> placeEnemyMM(MM5,2,2,MM6);append([],MM5,MM6)),
                            ((positionmatchES(Xup,Yright)) -> placeEnemyMM(MM6,1,3,MM7); append([],MM6,MM7)),
                            ((positionmatchES(Xdown,Yright))-> placeEnemyMM(MM7,3,3,MM8); append([],MM7,MM8)),
                            ((positionmatchES(Xup,Yleft))->placeEnemyMM(MM8,1,1,MM9);append([],MM8,MM9)),
                            ((positionmatchES(Xdown,Yleft))-> placeEnemyMM(MM9,3,1,MM10);append([],MM9,MM10)).

      placeValidityMM(MM,X,Y,Z) :- at(MM,X,Y,Val), Val == Z.
      look :- minimaps(MM), player_position(X,Y), at(MM,X,Y,MM2), ((checkEnemyF(MM2, Result)-> printRows(Result,1)) ; printRows(MM2,1)).

  %CHANGE MINI MAP : ----------------------------------------------
      placeRandomEnemyMM(MM,Mresult) :-
                            randomize, %USE RANDOMIZE TO RESET SEED.
                            random(1, 4, X),
                            random(1, 4, Y),
                            ( (placeValidityMM(MM,X,Y,'-'))  ->
                              ( changeMM(MM,X,Y,'E',Mresult));
                              placeRandomEnemyMM(MM,Mresult)).

      placeEnemyMM(MM,X,Y,Mresult) :-
                              placeValidityMM(MM,X,Y,'-'),
                              changeMM(MM,X,Y,'E',Mresult).

      changeMM(MM,X,Y,A,M2) :- replace_row_col(MM,X,Y,A,M2). /*retract(minimaps(M)), asserta(minimaps(M2))*/

  %PRINTING MAP GROUP ----------------------------------------------
        %HELPER RULES :
        %printRows(LIST, rowidx)
          printRows([], _).
          printRows([H|T], R) :-
            printRow(H, R, 1),
            Rpp is R + 1,
            printRows(T, Rpp).

          printRow([], _, _) :- nl.
          printRow([H|T], R, C) :-
            write(H),
            write(' '),
            Cpp is C + 1,
            printRow(T, R, Cpp).

  map :- gamemap(M), printRows(M, 1).

  %EDITING MAP GROUP -----------------------
    %REPLACE ONE ELEMENT IN MATRIX :

        %HELPER RULES :
        replace_nth(N,I,V,O) :-
        nth1(N,I,_,T),
        nth1(N,O,V,T).

        replace_row_col(M,Row,Col,Cell,N) :-
            nth1(Row,M,Old),
            replace_nth(Col,Old,Cell,Upd),
            replace_nth(Row,M,Upd,N).


    changemap(M,X,Y,A,M2) :- gamemap(M), replace_row_col(M,X,Y,A,M2) , retract(gamemap(M)), asserta(gamemap(M2)).


    %REPLACE ALL ELEMENT IN ROW OR COLUMN :
      %NOTE : INDEX STARTS FROM 1 !
        % REPLACE A ROW :

          rowN([H|T],1,[X|T]) :- fill_row(H,'X',X),!, rowN(T,1,T).
          rowN([H|T],I,[H|T]) :-
              I1 is I-1,
              rowN(T,I1,T).
          rowN([],_,[]).

          fill_row([],_,[]).
          fill_row([_|T],A,[A|R]) :- fill_row(T,A,R).

          %replace_row([],_,_,[]).
          replace_row([H|T],N,A,[Q|R]) :-
                                        N == 1,
                                        NextN is N-1,
                                        fill_row(H,A,Q),
                                        replace_row(T,NextN,_,R).
          replace_row([H|T],N,A,[H|R]) :- N > 1, NextN is N-1, replace_row(T,NextN,A,R).
          replace_row(L, _, _, L).

        % REPLACE A COL :

          %makecolX(List,Index,NewList)
          makecolX([],_,_).
          makecolX([H|T], Index, [H2|R]) :-
                  replace_col(H, Index, 'X', H2),
                  makecolX(T, Index, R).

          %replace(List,Index,Value,NewList)

          replace_col([_|T], 1, X, [X|T]).
          replace_col([H|T], I, X, [H|R]):-
                  I > 1,
                  NI is I-1,
                  replace_col(T, NI, X, R), !.
          replace_col(L, _, _, L).



  %GET MATRIX VALUE -----------------------

    at(Mat, Row, Col, Val) :- nth1(Row, Mat, ARow), nth1(Col, ARow, Val).

  %BOOM BOOM AREA (ADVANCES DEATHZONE BY 1 INCREMENT) -----------------------
    %youdie(threatlvl)

    youdie :- threatlvl(X),
                NX is X+1, gamemap(M),
                replace_row(M,(1+NX),'X',M2),
                makecolX(M2,(1+NX),M3),
                replace_row(M3,(12-NX),'X',M4),
                makecolX(M4,(12-NX),M5),
                retractall(gamemap(M)),
                asserta(gamemap(M5)),
                retract(threatlvl(X)),
                asserta(threatlvl(NX)).

  %RANDOM ENEMY PLACEMENT GROUP ----------------------------------------------


    %HELPER RULES :
      %repeat.
      %repeat :- repeat.

      placeValidity(X,Y,Z) :- gamemap(M), at(M,X,Y,Val), Val == Z.

	enemyTypeList(['S','M','L']).

    placeRandomEnemy :- map_row(R), map_col(C), threatlvl(LVL),
                          Low is 1+LVL, High is R-LVL,
                          Low2 is 2+LVL, High2 is C-LVL,
                          repeat,
                          randomize, %USE RANDOMIZE TO RESET SEED.
                          random(Low, High, X),
                          random(Low2, High2, Y),
                          ( (moveValidity(X,Y,'-'))  ->
                            fail;
						  (
							enemynumber(N), Newnumber is N+1, retract(enemynumber(N)), asserta(enemynumber(Newnumber)),
							  (
								randomize,
								random(0,3,Idx),
								enemyTypeList(L),
								nth0(Idx,L,EnemyType),(
									( (EnemyType == 'S') -> HP is 15 ;true ),
									( (EnemyType == 'M') -> HP is 35 ;true ),
									( (EnemyType == 'L') -> HP is 75 ;true )
									),
								asserta(enemy(EnemyType,HP,_,X,Y)),
								changemap(_,X,Y,'E',M2),  retractall(gamemap(_)), asserta(gamemap(M2))))
							).


	%RANDOM PLAYER PLACEMENT GROUP ----------------------------------------------

	placeRandomPlayer :- map_row(R), map_col(C), threatlvl(LVL),
                          Low is 1+LVL, High is R-LVL,
                          Low2 is 2+LVL, High2 is C-LVL,
                          repeat,
                          randomize, %USE RANDOMIZE TO RESET SEED.
                          random(Low, High, X),
                          random(Low2, High2, Y),
                          ( (moveValidity(X,Y,'-'))  ->
                            fail;
                          ( changemap(_,X,Y,'P',M2), retract(player_position(_,_)), asserta(player_position(X,Y)) , retractall(gamemap(_)), asserta(gamemap(M2)))).

  %MOVEMENT GROUP ----------------------------------------------
    %MOVE PLAYER :
        moveValidity(X,Y,Z) :- gamemap(M), at(M,X,Y,Val), Val \= Z.

        n:- player_position(X,Y), NX is X-1,
              moveValidity(NX,Y,'X'),
              changemap(_,X,Y,'-',M2),
              changemap(M2,NX,Y,'P',_),
              retractall(player_position(_,_)),
              asserta(player_position(NX,Y)).

        e:- player_position(X,Y), NY is Y+1,
              moveValidity(X,NY,'X'),
              changemap(_,X,Y,'-',M2),
              changemap(M2,X,NY,'P',_),
              retractall(player_position(_,_)),
              asserta(player_position(X,NY)).

        s:- player_position(X,Y), NX is X+1,
              moveValidity(NX,Y,'X'),
              changemap(_,X,Y,'-',M2),
              changemap(M2,NX,Y,'P',_),
              retractall(player_position(_,_)),
              asserta(player_position(NX,Y)).

        w:- player_position(X,Y), NY is Y-1,
              moveValidity(X,NY,'X'),
              changemap(_,X,Y,'-',M2),
              changemap(M2,X,NY,'P',_),
              retractall(player_position(_,_)),
              asserta(player_position(X,NY)).

     %MOVE ENEMY :

        ne(Enemytype,HP,W,X,Y):-  NX is X-1,
              moveValidity(NX,Y,'X'),
              changemap(_,X,Y,'-',M2),
              changemap(M2,NX,Y,'E',_),
              retract(enemy(Enemytype,HP,W,X,Y)),
              asserta(enemy(Enemytype,HP,W,NX,Y)).

        ee(Enemytype,HP,W,X,Y):- NY is Y+1,
              moveValidity(X,NY,'X'),
              changemap(_,X,Y,'-',M2),
              changemap(M2,X,NY,'E',_),
              retract(enemy(Enemytype,HP,W,X,Y)),
              asserta(enemy(Enemytype,HP,W,X,NY)).

        se(Enemytype,HP,W,X,Y):-  NX is X+1,
              moveValidity(NX,Y,'X'),
              changemap(_,X,Y,'-',M2),
              changemap(M2,NX,Y,'E',_),
              retract(enemy(Enemytype,HP,W,X,Y)),
              asserta(enemy(Enemytype,HP,W,NX,Y)).

        we(Enemytype,HP,W,X,Y):-  NY is Y-1,
              moveValidity(X,NY,'X'),
              changemap(_,X,Y,'-',M2),
              changemap(M2,X,NY,'E',_),
              retract(enemy(Enemytype,HP,W,X,Y)),
              asserta(enemy(Enemytype,HP,W,X,NY)).

        enemyMoveList([ne,ee,se,we]).

        enemyRandomMove:-
              repeat,
              forall(enemy(Enemytype,HP,W,X,Y), (
					(randomize,
					random(0,3,Idx),
					enemyMoveList(L),
					nth0(Idx,L,Move),
					call(Move,Enemytype,HP,W,X,Y) -> true ; fail)
					)
				).

         %DEBUGGING :
          printEnemy:-
              forall(enemy(_,_,_,X,Y), (write(X), write(','), write(Y), nl)).

% ATTACK : --------------------------------------------------------------------------------------------

    dealDMG(HP, W, HP2) :- ((W = 'watergun') -> (randomize, random(1,10,DMG), HP2 is HP-DMG) ; true),
                           ((W = 'YoYo') -> (randomize, random(1,21,DMG), HP2 is HP-DMG); true),
                           ((W = 'Da Piss Tall') -> (randomize, random(1,100,DMG), HP2 is HP-DMG) ; true).

    attack :- player_weapon(W), player_position(X,Y), positionmatchES(X,Y), enemy(T,HP,W,X,Y) , dealDMG(HP,W,HP2), write('Leftover enemy HP : '), write(HP2),
              enemynumber(N),
              (
				  (HP2 =< 0) -> ( NewN is N-1,retract(enemy(T,HP,W,X,Y)), retract(enemynumber(N)), asserta(enemynumber(NewN)), write('ENEMY DEAD'),nl )
								;
								(retract(enemy(T,HP,W,X,Y)), asserta(enemy(T,HP2,W,X,Y)), write('ENEMY STILL ALIVE'),nl )
			   ).

%MAIN GAME TEST : --------------------------------------------------------------------------------------------
	%RULE REPEATER :
		callmultiple(_,0).
		callmultiple(Command, N) :- call(Command), NewN is N-1, callmultiple(Command,NewN).

	%GAME LOOP :
		end:- write('Thank you for playing, lol. Actually, YOU should THANK me.').
		setup :- callmultiple(placeRandomEnemy,10), call(placeRandomPlayer), call(initMM(1,1)).
		game :-
			  write('>> '),
        %call(enemyRandomMove),
			  read(X),
			  (call(X) -> true ; game),
			  ((X = end) -> halt ; game).

initgame :- init_object, setup, game.

% TAKE & USE : --------------------------------------------------------------------------------------------

take(X) :- (object(_,X,X1,Y1)),
           player_bag(Size,Inv),
           length(Inv,C),
           C < Size,
           positionmatchI(X1,Y1),
           append(Inv,[X],New_inv),
           retract(player_bag(Size,Inv)),
           asserta(player_bag(Size,New_inv)),
           write('Take success'),nl,
           print_Inv(New_inv),!.

take(X) :- (object(_,X,_,_)),
           player_bag(Size,Inv),
           length(Inv,C),
           C < Size,
           format("~p not on the ground",[X]),nl,!.

take(_) :- player_bag(Size,Inv),
           length(Inv,C),
           C is Size,
           write('Bag Full'),nl,!.

print_Inv([]) :- write('Inv Empty'),nl,!.
print_Inv([X|[]]) :- format("~p",[X]),nl,!.
print_Inv([X|Y]) :- format("~p,",[X]),print_Inv(Y),!.

use(X) :- object(_,X,_,_),
          player_bag(Size,Inv),
          member(X,Inv),
          select(X,Inv,New_inv),
          retract(player_bag(Size,Inv)),
          asserta(player_bag(Size,New_inv)),
          format("~p used",[X]),nl,
          print_Inv(New_inv),!.

use(X) :- player_bag(_,Inv),
          format("~p is not in your bag",[X]),nl,
          print_Inv(Inv),!.

positionmatchI(X1,Y1) :- player_position(X,Y), (X == X1, Y == Y1) ,!.

%RULE CERITA : -------------------------------------
  enemy_north:- write('You see someone is moving. DAMN! He is in front of you. Attack him or you die?'), nl.
  enemy_south:- write('Do you hear that? Someone is following you. Oh no! He is behind you. Grab your weapon, now!'), nl.
  enemy_west:- write('Men always left because women always right. Is it true? Look at left and you see your enemy. Oops.'), nl.
  enemy_east:- write('Left. Right. Left. RIGHT! He is coming. Be a man, finish him!'), nl.
  your_weapon :- write('Your weapon is ...'), player_weapon(X), write(X), nl.
  look_weapon :- write('Hey. No weapon, huh? Look around. You see a weapon. Grab it.'), nl.
  look_armor :- write('Feeling insecure? Just grab that armor and you feel free.'), nl.
  look_medicine :- write('Give up? Never say that! Claim your victor. Take that medicine and kill all your enemy!'), nl.
  north_deadzone :- write('One step to the north and you will die.'), nl.
  south_deadzone :- write('One step to the south and you will die.'), nl.
  west_deadzone :- write('One step to the west and you will die.'), nl.
  east_deadzone :- write('One step to the east and you will die.'), nl.
  northeast_deadzone :- write('One step to the north or east and you will die.'), nl.
  northwest_deadzone :- write('One step to the north or west and you will die.'), nl.
  southeast_deadzone :- write('One step to the south or east and you will die.'), nl.
  southwest_deadzone :- write('One step to the south or west and you will die.'), nl.
  free_zone :- write('You see nothing. You're feeling lonely right now. Maybe you already become a victor?'), nl.
  empty_ammo :- write('Shame on you. Weapon without ammo, huh?'), nl.
  attack_enemy :- write('It's time. Grab your weapon. Shoot him! BOOM!'), nl.
  deadzone_expanded :- write('OH NO! Deadzone is expanding. Be careful.'), nl.
