:- dynamic(gamemap/1).
:- dynamic(deathclock/1). deathclock(0).
:- dynamic(threatlvl/1). threatlvl(0).
:- dynamic(player_position/2).  player_position(2,2).
:- dynamic(mapEff_row/1). mapEff_row(10).
:- dynamic(mapEff_col/1). mapEff_col(10).
:- dynamic(map_row/1). map_row(12).
:- dynamic(map_col/1). map_col(12).

:-dynamic(enemy/2).
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
                              ((at(M,Xup,Y,Val), Val == 'X')-> replace_row(MM2,1,'X',MM3); append([],MM2,MM3)),
                              ((at(M,X,Yright,Val), Val == 'X') -> makecolX(MM,3,MM2); append([],MM,MM2)),
                              ((at(M,Xdown,Y,Val), Val == 'X')-> replace_row(MM4,3,'X',MM5);append([],MM4,MM5)),
                              ((at(M,X,Yleft,Val), Val == 'X')->makecolX(MM3,1,MM4);append([],MM3,MM4)).
                              %DEBUGGER :printRows(MM5,1).

  %CHECK FOR ENEMY ! : ----------------------------------------------
      positionmatch :- player_position(X,Y), enemy(X2,Y2), (X == X2, Y == Y2) ,!.
      checkEnemy(MM,MResult):- positionmatch, placeRandomEnemyMM(MM,MResult).
      placeValidityMM(MM,X,Y,Z) :- at(MM,X,Y,Val), Val == Z.
      look :- player_position(X,Y), minimaps(MM), at(MM,X,Y,Minimap), changeMM(Minimap, 2,2,'P',Minimap2), ((checkEnemy(Minimap2, Result)-> printRows(Result,1)) ; printRows(Minimap2,1)).

  %CHANGE MINI MAP : ----------------------------------------------
  placeRandomEnemyMM(MM,Mresult) :-
                        randomize, %USE RANDOMIZE TO RESET SEED.
                        random(1, 4, X),
                        random(1, 4, Y),
                        ( (placeValidityMM(MM,X,Y,'-'))  ->
                          ( changeMM(MM,X,Y,'E',Mresult));
                          placeRandomEnemyMM(MM,Mresult)).

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

    placeRandomEnemy :- map_row(R), map_col(C), threatlvl(LVL),
                          Low is 1+LVL, High is R-LVL,
                          Low2 is 2+LVL, High2 is C-LVL,
                          repeat,
                          randomize, %USE RANDOMIZE TO RESET SEED.
                          random(Low, High, X),
                          random(Low2, High2, Y),
                          ( (moveValidity(X,Y,'-'))  ->
                            fail;
                          (enemynumber(N), Newnumber is N+1, retract(enemynumber(N)), asserta(enemynumber(Newnumber)), changemap(_,X,Y,'E',M2), asserta(enemy(X,Y)), retractall(gamemap(_)), asserta(gamemap(M2)))).


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

        ne(X,Y):-  NX is X-1,
              moveValidity(NX,Y,'X'),
              changemap(_,X,Y,'-',M2),
              changemap(M2,NX,Y,'E',_),
              retract(enemy(X,Y)),
              asserta(enemy(NX,Y)).

        ee(X,Y):- NY is Y+1,
              moveValidity(X,NY,'X'),
              changemap(_,X,Y,'-',M2),
              changemap(M2,X,NY,'E',_),
              retract(enemy(X,Y)),
              asserta(enemy(X,NY)).

        se(X,Y):-  NX is X+1,
              moveValidity(NX,Y,'X'),
              changemap(_,X,Y,'-',M2),
              changemap(M2,NX,Y,'E',_),
              retract(enemy(X,Y)),
              asserta(enemy(NX,Y)).

        we(X,Y):-  NY is Y-1,
              moveValidity(X,NY,'X'),
              changemap(_,X,Y,'-',M2),
              changemap(M2,X,NY,'E',_),
              retract(enemy(X,Y)),
              asserta(enemy(X,NY)).

        enemyMoveList([ne,ee,se,we]).

        enemyRandomMove:-
              repeat,
              forall(enemy(X,Y), (
					(randomize,
					random(0,3,Idx),
					enemyMoveList(L),
					nth0(Idx,L,Move),
					call(Move,X,Y) -> true ; fail)
					)
				).

         %DEBUGGING :
          printEnemy:-
              forall(enemy(X,Y), (write(X), write(','), write(Y), nl)).

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

		initgame :- setup, game.
