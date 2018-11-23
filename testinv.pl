:- dynamic(bag/2).  bag(5,[]).

object(medicine).
object(armor).

take(X) :- object(X),
           bag(Size,Inv),
           length(Inv,C),
           C < Size,
           append(Inv,[X],New_inv),
           retract(bag(Size,Inv)),
           asserta(bag(Size,New_inv)),
           write('Take success'),nl,
           print_Inv(New_inv),!.

take(X) :- bag(Size,Inv),
           length(Inv,C),
           C is Size,
           write('Bag Full'),nl,!.

print_Inv([]) :- write('Inv Empty'),nl,!.
print_Inv([X|[]]) :- format("~p",[X]),nl,!.
print_Inv([X|Y]) :- format("~p,",[X]),print_Inv(Y),!.

use(X) :- object(X),
          bag(Size,Inv),
          member(X,Inv),
          select(X,Inv,New_inv),
          retract(bag(Size,Inv)),
          asserta(bag(Size,New_inv)),
          format("~p used",[X]),nl,
          print_Inv(New_inv),!.

use(X) :- object(X),
          bag(Size,Inv),
          format("~p is not in your bag",[X]),nl,
          print_Inv(Inv),!.
