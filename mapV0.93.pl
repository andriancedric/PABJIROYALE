:- dynamic(gamemap/1).
:- dynamic(deathclock/1). deathclock(0).
:- dynamic(threatlvl/1). threatlvl(0).
:- dynamic(player_health/1). player_health(50).
:- dynamic(player_position/2).  player_position(2,2).
:- dynamic(player_weapon/1). player_weapon(ak47).
:- dynamic(player_ammo/1). player_ammo(5).
:- dynamic(player_armor/1). player_armor(10).
:- dynamic(player_inventory/1). player_inventory(0).
:- dynamic(player_bag/2).  player_bag(5,[]).
:- dynamic(mapEff_row/1). mapEff_row(10).
:- dynamic(mapEff_col/1). mapEff_col(10).
:- dynamic(map_row/1). map_row(12).
:- dynamic(map_col/1). map_col(12).
:- dynamic(turn/1). turn(1).
:- dynamic(object/4). 

:-dynamic(cur_minimap/1). cur_minimap([]).
:-dynamic(enemy/5). % GUIDE : enemy(Type, HP, Weapon, X, Y)
:-dynamic(enemy_number/1). enemy_number(0).

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
	
/*Object List*/	
objectTypeList(['weapon','armor','item']).
weaponNameList(['gun' , 'pan', 'ak47', 'watergun', 'yoyo']).
armorNameList(['vest' , 'kevlar']).
itemNameList(['medicine', 'tolakangin', 'panadol']).
ammoNameList(['magazine', 'bullet']).

/*Object*/
init_object :-
    	asserta(object(weapon,gun,3,3)),
		asserta(object(weapon,gun,3,4)),
    	asserta(object(weapon,pan,4,4)),
    	asserta(object(weapon,ak47,3,5)),
    	asserta(object(armor,vest,4,5)),
    	asserta(object(armor,kevlar,6,2)),
    	asserta(object(medicine,tolakangin,5,5)),
    	asserta(object(medicine,panadol,4,3)),
    	asserta(object(medicine,aspirin,5,6)),
		asserta(object(bag,mediumbag,6,6)),
		asserta(object(bag,largebag,6,7)).
	  
	  
% TAKE & USE & DROP : --------------------------------------------------------------------------------------------
		
		

				   
		take(X) :- object('ammo',X,X1,Y1), player_ammo(AM),
				   positionmatchI(X1,Y1),
				   (
						( X == 'bullet') ->
							(
								write('You picked up some unused bullet!'),nl,
								randomize,
								random(1,6,AMplus),
								NewAM is AM+AMplus
							)
						;
							(
								write('You picked up an untouched magazine! Lucky you.'),nl,
								randomize,
								random(10,20,AMplus),
								NewAM is AM+AMplus
							)
					),
				   retract(object('ammo',X,X1,Y1)),
				   asserta(object('ammo',X,0,0)),
				   retract(player_ammo(AM)),
				   asserta(player_ammo(NewAM)),
				   write('Take success'),nl,!.
				   
		take(X) :- object(W,X,X1,Y1),
				   player_bag(Size,Inv),
				   length(Inv,C),
				   C < Size,
				   positionmatchI(X1,Y1),
				   append(Inv,[X],New_inv),
				   retract(player_bag(Size,Inv)),
				   asserta(player_bag(Size,New_inv)),
				   write('Take success'),nl,
				   retract(object(W,X,X1,Y1)),
				   asserta(object(W,X,0,0)),
				   print_Inv(New_inv),!.

		
		take(_) :- player_bag(Size,Inv),
				   length(Inv,C),
				   C is Size,
				   write('Bag Full'),nl,!.
				   
		take(X) :- 
				   player_bag(Size,Inv),
				   length(Inv,C),
				   C < Size,
				   format("~p not on the ground",[X]),nl,!.
				   
	    
				   



		print_Inv([]) :- write('Inv Empty'),nl,!.
		print_Inv([X|[]]) :- format("~p",[X]),nl,!.
		print_Inv([X|Y]) :- format("~p,",[X]),print_Inv(Y),!.

		use(X) :- object(armor,X,_,_),
				  player_armor(Armor),
				  retract(player_armor(Armor)),
				  ( (X == 'vest') -> (Narmor is Armor+20) ; true ),
				  ( (X == 'kevlar') -> (Narmor is Armor+40) ; true ),
				  player_bag(Size,Inv),
				  member(X,Inv),
				  select(X,Inv,New_inv),
				  retract(player_bag(Size,Inv)),
				  asserta(player_bag(Size,New_inv)),
				  asserta(player_armor(Narmor)),
				  format("Armor : ~p",[Narmor]),nl,
				  format("~p used",[X]),nl,
				  print_Inv(New_inv),!.

		use(X) :- object(item,X,_,_),
				  player_health(Health),
				  retract(player_health(Health)),
				  ( (X == 'tolakangin') -> (Nhealth is Health+20) ; true ),
				  ( (X == 'panadol') -> (Nhealth is Health+40) ; true ),
				  ( (X == 'aspirin') -> (Nhealth is Health+60) ; true ),
				  player_bag(Size,Inv),
				  member(X,Inv),
				  select(X,Inv,New_inv),
				  retract(player_bag(Size,Inv)),
				  asserta(player_bag(Size,New_inv)),
				  ( (Nhealth > 100) -> (Nhealth2 is 100) ; Nhealth2 is Nhealth),
				  asserta(player_health(Nhealth2)),
				  format("Health : ~p",[Nhealth2]),nl,
				  format("~p used",[X]),nl,
				  print_Inv(New_inv),!.

		use(X) :- object(weapon,X,_,_),
				  player_weapon(Weapon),
				  player_bag(Size,Inv),
				  member(X,Inv),
				  select(X,Inv,New_inv),
				  ( (Weapon \== 'watergun') -> (append(New_inv,[Weapon],New_inv2)) ; append([],New_inv,New_inv2) ),
				  retract(player_bag(Size,Inv)),
				  asserta(player_bag(Size,New_inv2)),
				  retract(player_weapon(Weapon)),
				  asserta(player_weapon(X)),
				  format("Weapon : ~p",[X]),nl,
				  format("~p equipped",[X]),nl,
				  print_Inv(New_inv),!.

		use(X) :- object(bag,X,_,_),
				  player_bag(Size,Inv),
				  ( (X == 'mediumbag') -> (Nsize is Size + 5) ; true ),
				  ( (X == 'largebag') -> (Nsize is Size+ 10) ; true ),
				  member(X,Inv),
				  select(X,Inv,New_inv),
				  retract(player_bag(Size,Inv)),
				  asserta(player_bag(Nsize,New_inv)),
				  format("Bag Size : ~p",[Nsize]),nl,
				  format("~p equipped",[X]),nl,
				  print_Inv(New_inv),!.

		use(X) :- player_bag(_,Inv),
				  format("~p is not in your bag",[X]),nl,
				  print_Inv(Inv),!.

		positionmatchI(X1,Y1) :- player_position(X,Y), (X == X1, Y == Y1) ,!.

		drop(X) :- object(W,X,0,0),
				   player_bag(Size,Inv),
				   member(X,Inv),
				   select(X,Inv,New_inv),
				   player_position(X1,Y1),
				   retract(object(W,X,0,0)),
				   asserta(object(W,X,X1,Y1)),
				   retract(player_bag(Size,Inv)),
				   asserta(player_bag(Size,New_inv)),
				   format("~p dropped",[X]), nl,
				   print_Inv(New_inv),!.

		drop(X) :- player_bag(_,Inv),
				   format("~p is not in your bag",[X]),nl,
				   print_Inv(Inv),!.

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

%CHECK STATUS :
	status :- 
			write('%%%%%%%%%% STATUS WINDOW %%%%%%%%%'), nl,
			turn(T),				write('TURN               :          '), write(T),nl,
			player_health(HP), 		write('Health             :          '), write(HP),nl,
			player_armor(AR),  		write('Armor              :          '), ( (AR < 1) -> write(0) ;  write(AR)),nl,
			player_weapon(W), 		write('Weapon             :          '), write(W),nl,
			player_ammo(AM),		write('Ammo               :          '), write(AM),nl,
			player_bag(_,INV),	    write('Inventory          :          '), nl,
			print_Inv(INV),
			write('%%%%%%%%%% STATUS WINDOW %%%%%%%%%'),nl.

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
	  %DEPRECCATED (CHECK BASED ON SYMBOL AT MAIN MAP)
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
                            ((positionmatchES(X,Yright))-> (placeEnemyMM(MM,2,3,MM2)); append([],MM,MM2)),
                            ((positionmatchES(Xup,Y))-> (placeEnemyMM(MM2,1,2,MM3)); append([],MM2,MM3)),
                            ((positionmatchES(X,Yleft))->(placeEnemyMM(MM3,2,1,MM4));append([],MM3,MM4)),
                            ((positionmatchES(Xdown,Y))-> (placeEnemyMM(MM4,3,2,MM5));append([],MM4,MM5)),
                            (positionmatchE -> (placeEnemyMM(MM5,2,2,MM6));append([],MM5,MM6)),
                            ((positionmatchES(Xup,Yright)) -> (placeEnemyMM(MM6,1,3,MM7)); append([],MM6,MM7)),
                            ((positionmatchES(Xdown,Yright))-> (placeEnemyMM(MM7,3,3,MM8)); append([],MM7,MM8)),
                            ((positionmatchES(Xup,Yleft))->(placeEnemyMM(MM8,1,1,MM9));append([],MM8,MM9)),
                            ((positionmatchES(Xdown,Yleft))-> (placeEnemyMM(MM9,3,1,MM10)); append([],MM9,MM10)).

      placeValidityMM(MM,X,Y,Z) :- at(MM,X,Y,Val), Val == Z.
	  
	  
	  positionmatchO(Type) :- player_position(X,Y), object(Type,_,X2,Y2), (X == X2, Y == Y2) ,!.
      %SPECIFIC MATCH :
      positionmatchOS(X,Y,Type) :-  object(Type,_,X2,Y2), (X == X2, Y == Y2) ,!.
	  
	  checkObject(MM,MM10):-    player_position(X,Y), Yleft is (Y-1), Yright is (Y+1), Xdown is (X+1), Xup is (X-1),
                            ((positionmatchOS(X,Yright,Type))-> (placeObjectMM(Type, MM,2,3,MM2)); append([],MM,MM2)),
                            ((positionmatchOS(Xup,Y,Type2))-> (placeObjectMM(Type2, MM2,1,2,MM3)); append([],MM2,MM3)),
                            ((positionmatchOS(X,Yleft,Type3))->(placeObjectMM(Type3, MM3,2,1,MM4));append([],MM3,MM4)),
                            ((positionmatchOS(Xdown,Y,Type4))-> (placeObjectMM(Type4, MM4,3,2,MM5));append([],MM4,MM5)),
                            ((positionmatchOS(X,Y,Type5)) -> (placeObjectMM(Type5, MM5,2,2,MM6));append([],MM5,MM6)),
                            ((positionmatchOS(Xup,Yright,Type6)) -> (placeObjectMM(Type6, MM6,1,3,MM7)); append([],MM6,MM7)),
                            ((positionmatchOS(Xdown,Yright,Type7))-> (placeObjectMM(Type7, MM7,3,3,MM8)); append([],MM7,MM8)),
                            ((positionmatchOS(Xup,Yleft,Type8))->(placeObjectMM(Type8, MM8,1,1,MM9));append([],MM8,MM9)),
                            ((positionmatchOS(Xdown,Yleft,Type9))-> (placeObjectMM(Type9, MM9,3,1,MM10)); append([],MM9,MM10)).
	  
	   placeObjectMM(Type,MM,X,Y,Mresult) :-
                              moveValidityMM(MM,X,Y,'X'),(
							  ((Type == 'weapon') -> (changeMM(MM,X,Y,'W',Mresult)) ; true ),
							  ((Type == 'item') -> (changeMM(MM,X,Y,'I',Mresult)) ; true ),
							  ((Type == 'ammo') -> (changeMM(MM,X,Y,'A',Mresult)) ; true ),
							  ((Type == 'armor') -> (changeMM(MM,X,Y,'R',Mresult)) ; true ),
							  ((Type == 'bag') -> (changeMM(MM,X,Y,'B',Mresult)) ; true )
							  ).
							  
		
	  
		checkDZ(MM,MM10):-    player_position(X,Y), Yleft is (Y-1), Yright is (Y+1), Xdown is (X+1), Xup is (X-1),
							  ((at(M,X,Yright,Val), Val == 'X') -> changeMM(MM,2,3,'X',MM2); append([],MM,MM2)),
							  ((at(M,Xup,Y,Val), Val == 'X')-> changeMM(MM2,1,2,'X',MM3); append([],MM2,MM3)),
							  ((at(M,X,Yleft,Val), Val == 'X')->changeMM(MM3,2,1,'X',MM4);append([],MM3,MM4)),
							  ((at(M,Xdown,Y,Val), Val == 'X')-> changeMM(MM4,3,2,'X',MM5);append([],MM4,MM5)),
							  ((at(M,X,Y,Val), Val == 'X') -> changeMM(MM5,2,2,'X',MM6);append([],MM5,MM6)),
							  ((at(M,Xup,Yright,Val), Val == 'X') -> changeMM(MM6,1,3,'X',MM7); append([],MM6,MM7)),
							  ((at(M,Xdown,Yright,Val), Val == 'X')-> changeMM(MM7,3,3,'X',MM8); append([],MM7,MM8)),
							  ((at(M,Xup,Yleft,Val), Val == 'X')->changeMM(MM8,1,1,'X',MM9);append([],MM8,MM9)),
							  ((at(M,Xdown,Yleft,Val), Val == 'X')-> changeMM(MM9,3,1,'X',MM10);append([],MM9,MM10)).	
							
      look :- minimaps(MM), player_position(X,Y), at(MM,X,Y,MM2),
							(
								%PLACE DEADZONES (if exists) : 
								checkDZ(MM2,MM3),
								%PLACE PLAYER :
								changeMM(MM3,2,2,'P',MM4),
								%SHOW OBJECTS :
								(checkObject(MM4,MM5)),
								%SHOW ENEMY :
								(checkEnemyF(MM5, Result)),
								
								retractall(cur_minimap(_)),
								asserta(cur_minimap(Result)),
								narrate_look,
								printRows(Result,1) 
							).

  %CHANGE MINI MAP : ----------------------------------------------
      placeRandomEnemyMM(MM,Mresult) :-
                            randomize, %USE RANDOMIZE TO RESET SEED.
                            random(1, 4, X),
                            random(1, 4, Y),
                            ( (placeValidityMM(MM,X,Y,'-'))  ->
                              ( changeMM(MM,X,Y,'E',Mresult));
                              placeRandomEnemyMM(MM,Mresult)).

      placeEnemyMM(MM,X,Y,Mresult) :-
                              (moveValidityMM(MM,X,Y,'X') ->
                              changeMM(MM,X,Y,'E',Mresult) ; true).

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

    expandDZ:- threatlvl(X),
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
						%WEAPON RANDOMIZE :
						  random(0,5,Idx0),
						  weaponNameList(WL),
						  nth0(Idx0,WL,WeaponName),
                          randomize, %USE RANDOMIZE TO RESET SEED.
                          random(Low, High, X),
                          random(Low2, High2, Y),
                          ( (moveValidity(X,Y,'-'))  ->
                            fail;
						  (
							enemy_number(N), Newnumber is N+1, retract(enemy_number(N)), asserta(enemy_number(Newnumber)),
							  (
								randomize,
								random(0,3,Idx),
								enemyTypeList(L),
								nth0(Idx,L,EnemyType),(
									( (EnemyType == 'S') -> HP is 15 ;true ),
									( (EnemyType == 'M') -> HP is 35 ;true ),
									( (EnemyType == 'L') -> HP is 75 ;true )
									),
								asserta(enemy(EnemyType,HP,WeaponName,X,Y)),
								changemap(_,X,Y,'E',M2),  retractall(gamemap(_)), asserta(gamemap(M2))))
							).
	
	placeRandomObject :- map_row(R), map_col(C), threatlvl(LVL),
                          Low is 1+LVL, High is R-LVL,
                          Low2 is 2+LVL, High2 is C-LVL,
						%WEAPON RANDOMIZE :
						  random(0,5,Idx),
						  weaponNameList(WL),
						  nth0(Idx,WL,WeaponName),
						%ITEM RANDOMIZE :
						  random(0,2,Idx2),
						  itemNameList(IL),
						  nth0(Idx2,IL,ItemName),
						%ARMOR RANDOMIZE :
						  random(0,2,Idx3),
						  armorNameList(AL),
						  nth0(Idx3,AL,ArmorName),
						%AMMO RANDOMIZE :
						  random(0,2,Idx4),
						  ammoNameList(AmL),
						  nth0(Idx4,AmL,AmmoName),
						%PLACEMENT :
						 
						  placeA(Low,Low2,High,High2, ArmorName),
						  placeAm(Low,Low2,High,High2, AmmoName),
						  placeW(Low,Low2,High,High2, WeaponName),
						  placeI(Low,Low2,High,High2, ItemName).
						  
    placeRandomBag :- 
					map_row(R), map_col(C), threatlvl(LVL),
                    Low is 1+LVL, High is R-LVL,
                    Low2 is 2+LVL, High2 is C-LVL,
					placeB1(Low,Low2,High,High2, mediumbag),
					placeB2(Low,Low2,High,High2, largebag).

					
	placeB1(Low,Low2,High,High2,BagName):- 	repeat,
													randomize,
													random(Low, High, X),
													random(Low2, High2, Y),
													(placeValidity(X,Y,'-') ->
														asserta(object('bag',BagName,X,Y)) ; fail
													).
								
	placeB2(Low,Low2,High,High2,BagName):- 	repeat,
													randomize,
													random(Low, High, X),
													random(Low2, High2, Y),
													(placeValidity(X,Y,'-') ->
														asserta(object('bag',BagName,X,Y)) ; fail
													).
					

	placeW(Low,Low2,High,High2,WeaponName):- 	repeat,
													randomize,
													random(Low, High, X),
													random(Low2, High2, Y),
													(placeValidity(X,Y,'-') ->
														asserta(object('weapon',WeaponName,X,Y)) ; fail
													).
							
	placeA(Low,Low2,High,High2,ArmorName):- 	repeat,
												randomize,
												random(Low, High, X2),
												random(Low2, High2, Y2),
												(placeValidity(X2,Y2,'-') ->
													asserta(object('armor',ArmorName,X2,Y2)) ; fail
												).
	
	placeI(Low,Low2,High,High2,ItemName):- 	repeat,
												randomize,
												random(Low, High, X3),
												random(Low2, High2, Y3),
												(placeValidity(X3,Y3,'-') ->
													asserta(object('item',ItemName,X3,Y3)) ; fail
												).
							
	placeAm(Low,Low2,High,High2,AmmoName):- 	repeat,
												randomize,
												random(Low, High, X4),
												random(Low2, High2, Y4),
												(placeValidity(X4,Y4,'-') ->
													asserta(object('ammo',AmmoName,X4,Y4)) ; fail
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
		moveValidityMM(MM,X,Y,Z) :- at(MM,X,Y,Val), Val \= Z.
		
        n:- 
              player_position(X,Y), NX is X-1,
              (moveValidity(NX,Y,'X') -> true ; (write('YOU STUMBLED INTO THE DEADZONE! HOW STUPID OF YOU!'),halt)) ,
              changemap(_,X,Y,'-',M2),
              changemap(M2,NX,Y,'P',_),
              retractall(player_position(_,_)),
              asserta(player_position(NX,Y)),
			  narrate_map.

        e:- 
		      player_position(X,Y), NY is Y+1,
              (moveValidity(X,NY,'X')-> true ; (write('YOU STUMBLED INTO THE DEADZONE! HOW STUPID OF YOU!') ,halt)),
              changemap(_,X,Y,'-',M2),
              changemap(M2,X,NY,'P',_),
              retractall(player_position(_,_)),
              asserta(player_position(X,NY)),
			  narrate_map.

        s:- 
		      player_position(X,Y), NX is X+1,
              (moveValidity(NX,Y,'X')-> true ; (write('YOU STUMBLED INTO THE DEADZONE! HOW STUPID OF YOU!'),halt)),
              changemap(_,X,Y,'-',M2),
              changemap(M2,NX,Y,'P',_),
              retractall(player_position(_,_)),
              asserta(player_position(NX,Y)),
			  narrate_map.

        w:- 
		      player_position(X,Y), NY is Y-1,
              (moveValidity(X,NY,'X')-> true ; ( write('YOU STUMBLED INTO THE DEADZONE! HOW STUPID OF YOU!'), halt)),
              changemap(_,X,Y,'-',M2),
              changemap(M2,X,NY,'P',_),
              retractall(player_position(_,_)),
              asserta(player_position(X,NY)),
			  narrate_map.

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
					random(0,4,Idx),
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
                           ((W = 'yoyo') -> (randomize, random(1,21,DMG), HP2 is HP-DMG); true),
						   ((W = 'gun') -> (randomize, random(10,20,DMG), HP2 is HP-DMG); true),
						   ((W = 'pan') -> (randomize, random(1,30,DMG), HP2 is HP-DMG); true),
						   ((W = 'ak47') -> (randomize, random(20,25,DMG), HP2 is HP-DMG); true),
                           ((W = 'Da Piss Tall') -> (randomize, random(50,100,DMG), HP2 is HP-DMG) ; true).

    attack :- player_weapon(PW), player_position(X,Y), player_ammo(AM), ( 
				(positionmatchES(X,Y), AM > 0) -> 
				  (	  NewAM is AM-1, retract(player_ammo(AM)), asserta(player_ammo(NewAM)),
					  enemy(T,HP,EW,X,Y) , dealDMG(HP,PW,HP2),
					  DMG is abs(HP-HP2),
					  write('########## ENEMY IS DAMAGED by '), write(DMG), write(' using '), write(PW), write(' ##########'),nl,
					  write('Leftover enemy HP : '), write(HP2), nl,
					  enemy_number(N),
					  (
						  (HP2 < 1) -> ( NewN is N-1,retract(enemy(T,HP,W,X,Y)), retract(enemy_number(N)), asserta(enemy_number(NewN)), asserta(object('weapon',EW,X,Y)) , write('ENEMY DEAD'),nl ) 
										;
										(retract(enemy(T,HP,W,X,Y)), asserta(enemy(T,HP2,W,X,Y)), write('ENEMY STILL ALIVE'),nl )
					   )
				   )
				;
				   (
						(positionmatchES(X,Y)) ->
							(
							write('<<<<<<<<<<----------SYSTEM MESSAGE---------->>>>>>>>>>'),nl,
							write('                     No Ammo Left!                    '),nl,
							write('<<<<<<<<<<----------*****END.*****---------->>>>>>>>>>'),nl
							)
						;
							(
							write('<<<<<<<<<<----------SYSTEM MESSAGE---------->>>>>>>>>>'),nl,
							write('              No enemy in your position!        '),nl,
							write('<<<<<<<<<<----------*****END.*****---------->>>>>>>>>>'),nl
							)
							
					)
				).
				
%MAIN GAME TEST : --------------------------------------------------------------------------------------------
	%RULE REPEATER :
		callmultiple(_,0).
		callmultiple(Command, N) :- call(Command), NewN is N-1, callmultiple(Command,NewN).
		
	%Fact CHECKER :
		check(Fact) :-
			call(Fact), !,
			true;
			(warning,write('Input Not Valid, Please REINPUT!'), nl, warningE, fail.
			
	%GAME LOOP :
		end:- write('Thank you for playing, lol. Actually, YOU should THANK me.').
		setup :- callmultiple(placeRandomEnemy,1), callmultiple(placeRandomObject,10), callmultiple(placeRandomBag,5), call(placeRandomPlayer), call(initMM(1,1)).
		game :-
			  write('>> '),
			  read(Command),
			  turn(T),
			  /*Call Command :*/
				((Command) -> true ; game),	
				
			  /*Advance Turn Counter :*/
				( (Command = map ; Command= look ; Command= status) -> true ; 
					( NewT is T+1, retract(turn(T)), asserta(turn(NewT)),
						(
							player_position(X,Y), 
							/*Check Deadzone Expansion :*/
							( ( (NewT mod 10) =:= 0) -> (expandDZ, warning,deadzone_expanded,warningE) ; true),gamemap(Map),
							( ( at(Map,X,Y,CurPos), CurPos == 'X') -> (warning,write('The Deadzone has caught up to you. Nothing escapes the vile clutches of death...'),nl,warningE,halt) ; true),
							
							/*Check DMG :*/
							( (positionmatchES(X,Y), enemy(_,_,W,X,Y) ) -> 
								( 
									player_health(HP), player_armor(AR),
									( (AR < 1) -> 
											(
												dealDMG(HP,W,HP2), 
												DMG is HP-HP2,
												retract(player_health(HP)), 
												asserta(player_health(HP2))
											)
											;
											(
												dealDMG(AR,W,AR2), 
												DMG is AR-AR2,
												retract(player_armor(AR)), 
												asserta(player_armor(AR2)),
												((AR2 < 1) -> (warning, write('WARNING : ARMOR IS BROKEN'),nl, warningE) ; true)
											)
									), 
									write('########### YOU ARE DAMAGED by '), write(DMG), write(' using '), write(W), write(' ##########'),nl,
								/*CHECK GAMEOVER */
									( (player_health(EndHP), EndHP < 1 ) -> (nl,warning,write('YOU DIED! GAMEOVER'),nl, warningE ,halt) ; true)
								); true
							)
						)
					)
				),
			  
			  /*WIN CON. CHECK:*/
				(enemy_number(EN), EN == 0 -> (write('CONGRATULATIONS! Go eat your chicken dinner now, or whatever, I do not care'),halt) ; game),
				
			   /*ENEMY RANDOM MOVEMENT : */
				%( (Command = map ; Command= look ; Command= status) -> true ; call(enemyRandomMove)),
									
			   /*ENEMY REINFORCEMENT EVERY 13th TURN : */
				turn(NextT),
				( ( (NextT mod 13) =:= 0 )-> (call(placeRandomEnemy), warning, write('WARNING : A REINFORCEMENT HAS ARRIVED!'), nl, warningE) ; true),
			   
			  /*Check EXIT command : */
				((Command = end) -> halt ; game).

		initgame :- setup, game.
		start :- write('WELCOME TO MOBILE LEGENDS! Five Minutes till deadline tubesch!'), nl, initgame.


%RULE CERITA : -------------------------------------
  warning:- write('!!!!!!!!! WARNING !!!!!!!!!!'),nl.
  warningE:- write('!!!!!!!!!!!!!!!!!!!!!!!!!!!!'),nl.
  enemy_north:- write('You see someone is moving. DAMN! He is far away...at north. Attack him or you die?'), nl.
  enemy_northwest:- write('Tick tock tick tock. 11 on the clock and you see someone run unto you.'), nl.
  enemy_northeast:- write('Feeling lonely? Nope. Look at northeast and you are in trouble.'), nl.
  enemy_south:- write('Do you hear that? Someone is following you. Oh no! He is far behind you. Grab your weapon, now!'), nl.
  enemy_southwest:- write('Good at math? Okay. Rotate your head for 135 degrees counter-clockwise from north. TADA! ENEMY BOI!'), nl.
  enemy_southeast:- write('Phew, feeling tired, bro? Relax.....HAH, there is an enemy at southwest. RUNNN!'), nl.
  enemy_west:- write('Men always left because women always right. Is it true? Look at left and you see your enemy. Oops.'), nl.
  enemy_east:- write('Left. Right. Left. RIGHT! He is coming. Be a man, finish him!'), nl.
  enemy_center:- write('Face to face. Your enemy is in front of you. You need to take an action. No action? PAW. He attacked you.'), nl.
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
  free_zone :- write('You see nothing on your ').
  empty_ammo :- write('Shame on you. Weapon without ammo, huh?'), nl.
  attack_enemy :- write('It is time. Grab your weapon. Shoot him! BOOM!'), nl.
  deadzone_expanded :- write('OH NO! Deadzone is expanding. Be careful.'), nl.

  
	%USED WHEN MOVE :
	narrate_map :-  %Narrate AREA
							write('<<<<<<<<<<----------SYSTEM MESSAGE---------->>>>>>>>>>'),nl,
							gamemap(M),
							player_position(X,Y),
							Yleft is Y-1, Yright is Y+1, Xdown is X+1, Xup is X-1,
							  ((at(M,Xup,Y,Val), Val == 'X')->north_deadzone; (free_zone, write('north.'),nl)),
                              ((at(M,X,Yright,Val), Val == 'X') -> east_deadzone; (free_zone, write('east.'),nl)),
                              ((at(M,Xdown,Y,Val), Val == 'X')-> south_deadzone; (free_zone, write('south.'),nl)),
							  ((at(M,X,Yleft,Val), Val == 'X')->west_deadzone; (free_zone, write('west.'),nl)),
							write('<<<<<<<<<<----------*****END.*****---------->>>>>>>>>>'),nl.
							
    %USED WHEN LOOK 
	narrate_look  :- write('<<<<<<<<<<----------SYSTEM MESSAGE---------->>>>>>>>>>'),nl,	
							write('<ENEMY SCAN RESULT>'),nl,
							  X is 2, Y is 2, cur_minimap(M),
							  Yleft is Y-1, Yright is Y+1, Xdown is X+1, Xup is X-1,
							  ((at(M,X,Y,Val1), Val1 == 'E')->enemy_center; true),
							  ((at(M,Xup,Y,Val2), Val2 == 'E')->enemy_north; true),
							  ((at(M,Xup,Yright,Val3), Val3 == 'E') -> enemy_northeast; true),
                              ((at(M,X,Yright,Val4), Val4 == 'E') -> enemy_east; true),
							  ((at(M,Xdown,Yright,Val5), Val5 == 'E')-> enemy_southeast;true),
                              ((at(M,Xdown,Y,Val6), Val6 == 'E')-> enemy_south;true),
							  ((at(M,Xdown,Yleft,Val7), Val7 == 'E')->enemy_southwest;true),
							  ((at(M,X,Yleft,Val8), Val8 == 'E')->enemy_west;true),
							  ((at(M,Xup,Yleft,Val9), Val9 == 'E')->enemy_northwest; true),
							write('<OBJECT SCAN RESULT>'),nl, 
							player_position(X2,Y2), Yleft2 is (Y2-1), Yright2 is (Y2+1), Xdown2 is (X2+1), Xup2 is (X2-1),
						   ( object(_,Name0,X2,Y2)         ->   ( write('On your feet lies '), write(Name0),nl); true),
						   ( object(_,Name1,Xup2,Y2)        ->   ( write('On your north lies '), write(Name1),nl); true),
						   ( object(_,Name2,Xup2,Yright2)  ->   (  write('On your northeast lies '), write(Name2),nl) ; true),
						   ( object(_,Name3,X2,Yright2)    ->   (  write('On your east lies '), write(Name3),nl); true),
						   ( object(_,Name4,Xdown2,Yright2)->   (   write('On your southeast lies '), write(Name4),nl);true),
						   ( object(_,Name5,Xdown2,Y2)      ->   (  write('On your south lies '), write(Name5),nl);true),
						   ( object(_,Name6,Xdown2,Yleft2) ->	(  write('On your southwest lies '), write(Name6),nl);true),
						   ( object(_,Name7,X2,Yleft2)      ->   (  write('On your west lies '),      write(Name7),nl); true),
						   ( object(_,Name8,Xup2,Yleft2)   ->   (  write('On your northwest lies '), write(Name8),nl); true),  
					write('<<<<<<<<<<----------*****END.*****---------->>>>>>>>>>'),nl.						  
							  
    
							
	
			

  
 