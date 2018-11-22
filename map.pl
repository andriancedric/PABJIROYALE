:- dynamic(gamemap/1).
:- dynamic(deathclock/1). deathclock(0).
:- dynamic(threatlvl/1). threatlvl(0).
:- dynamic(player_position/2).  player_position(5,5).
:- dynamic(map_width/1). map_width(10).
:- dynamic(map_height/1). map_height(10).

gamemap([['X','-','-','-','-','-','-','-','-','-','-','X'],
    ['X','-','-','-','-','-','-','-','-','-','-','X'],
    ['X','-','-','-','-','-','-','-','-','-','-','X'],
    ['X','-','-','-','-','-','-','-','-','-','-','X'],
    ['X','-','-','-','P','-','-','-','-','-','-','X'],
    ['X','-','-','-','-','-','-','-','-','-','-','X'],
    ['X','-','-','-','-','-','-','-','-','-','-','X'],
    ['X','-','-','-','-','-','-','-','-','-','-','X'],
    ['X','-','-','-','-','-','-','-','-','-','-','X'],
    ['X','-','-','-','-','-','-','-','-','-','-','X']]).

%Matrix Rules :
  
  %RIBBON OF CHAR 'X' (FOR TOP AND BOTTOM OF MAP) -----------------------
    xribbon(0) :- nl.
    xribbon(N) :-
      write('X'), NextN is N-1, xribbon(NextN).

  %BUILD MATRIX       -----------------------
    matrix(Matrix, I, J, Value) :-
        nth0(I, Matrix, Row),
        nth0(J, Row, Value).
 
  %PRINTING MAP GROUP -----------------------   	
        %HELPER RULES :
        printRows([], _).
        printRows([H|T], R) :- 
          printRow(H, R, 0), 
          Rpp is R + 1, 
          printRows(T, Rpp).

        printRow([], _, _) :- nl.
        printRow([H|T], R, C) :- 
          write(H),
          write(' '),
          Cpp is C + 1, 
          printRow(T, R, Cpp).

  map :- gamemap(M), xribbon(23), printRows(M, 0), xribbon(23). 

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

        % REPLACE A ROW :

          rowN([H|T],1,[X|T]) :- fill_row(H,'X',X),!, rowN(T,1,T).
          rowN([H|T],I,[H|T]) :-
              I1 is I-1,
              rowN(T,I1,T).
          rowN([],_,[]).

          fill_row([],_,[]).
          fill_row([_|T],A,[A|R]) :- fill_row(T,A,R).

          replace_row([H|T],N,A,[H|R]) :- N > 1, NextN is N-1, replace_row(T,NextN,A,R).
          %replace_row([],_,_,_).
          replace_row([H|T],N,A,[Q|R]) :-
                                        N == 1,
                                        NextN is N-1,
                                        fill_row(H,A,Q), 
                                        replace_row(T,NextN,_,R).
          replace_row(L, _, _, L).
          
        % REPLACE A COL : 

          subs([],_,_).
          subs([H|T], Index, [H2|R]) :-
                  replace_col(H, Index, 'X', H2),
                  subs(T, Index, R).

          %replace(List,Index,Value,NewList)

          replace_col([_|T], 0, X, [X|T]).
          replace_col([H|T], I, X, [H|R]):-
                  I > 0, 
                  NI is I-1,
                  replace_col(T, NI, X, R), !.
            replace_col(L, _, _, L).



  %GET MATRIX VALUE -----------------------

    at(Mat, Row, Col, Val) :- nth1(Row, Mat, ARow), nth1(Col, ARow, Val).

  %BOOM BOOM AREA (ADVANCES DEATHZONE BY 1 INCREMENT) -----------------------
    %youdie(threatlvl)

    youdie :- threatlvl(X), 
                NX is X+1, gamemap(M), 
                replace_row(M,NX,'X',M2), 
                subs(M2,NX,M3),
                replace_row(M3,11-NX,'X',M4), 
                subs(M4,11-NX,M5),  
                retractall(gamemap(M)), 
                asserta(gamemap(M5)),
                retract(threatlvl(X)),
                asserta(threatlvl(NX)).

  %RANDOM ENEMY PLACEMENT GROUP -----------------------

  %repeat.
  %repeat :- repeat.

  placeValidity(X,Y,Z) :- gamemap(M), at(M,X,Y,Val), Val == Z.

  placeRandomEnemy() :- map_width(W), map_height(H), threatlvl(LVL), 
                        Low is 2+LVL, High is W-LVL,
                        Low2 is 2+LVL, High2 is H-LVL,
                        repeat,
                        random_between(Low, High, X), 
                        random_between(Low2, High2, Y), 
                        ( (moveValidity(X,Y,'-'))  ->
                            fail;
                        changemap(_,X,Y,'E',M2), retractall(gamemap(_)), asserta(gamemap(M2)), !).

  %MOVEMENT GROUP -----------------------
    %MOVE UP :
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



