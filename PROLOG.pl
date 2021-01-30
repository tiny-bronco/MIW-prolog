
% Rooms

:- dynamic(current_room/1).
:- dynamic(pos/1).
:- dynamic(strenght/1). 
:- dynamic(defense/1).

:- dynamic(enemy/4).
:- dynamic(objects/4).

:- dynamic(inventory/3).
:- dynamic(weapon/2).
:- dynamic(shield/2).


%23 pola
%DEF nie jest zaimplementowane

path([1,3], [2,3],n).
path([2,3], [3,3],n).
path([3,3], [4,3],n).
path([4,3], [5,3],n).
path([5,3], [6,3],n).
path([6,3], [7,3],n).

path([7,3], [7,4],e).
path([7,4], [7,5],e).
path([7,5], [7,6],e).
path([7,5], [6,5],s).
path([7,6], [7,7],e).

path([7,7], [6,7],s).
path([6,7], [5,7],s).
path([5,7], [4,7],s).


path([2,3], [2,2],w).
path([2,2], [2,1],w).
path([2,1], [3,1],n).
path([3,1], [4,1],n).
path([4,1], [4,2],e).
path([4,2], [4,3],e).

path([2,3], [2,4],e).
path([2,4], [2,5],e).
path([2,5], [1,5],s).



path1(X,Y,s) :- path(X,Y,s); path(Y,X, n).
path1(X,Y,n) :- path(X,Y,n); path(Y,X, s).
path1(X,Y,w) :- path(X,Y,w); path(Y,X, e).
path1(X,Y,e) :- path(X,Y,e); path(Y,X, w).


enemy([2,3],'rat',1,0).
enemy([3,1],'wolf',3,1).
enemy([5,3],'bandit',2,3).
enemy([4,7],'troll',5,5).


objects([1,5],'knife',3,0,'weapon').
objects([6,5],'firesword',6,3,'weapon').


checkenemy(X) :- enemy(X,NAME,STR,DEF), !, write('There is a '), write(NAME), nl, fight(STR,DEF,enemy(X,NAME,STR,DEF)).
checkenemy(X) :- checkobject(X).

fight(STR,DEF, E) :- 
strenght(X), X >= STR,!, write('You won!'), retract(E), nl .
fight(STR,DEF,E) :- write('You lost. Try again.'),nl, 
retractall(pos(_)),
start.

checkobject(X) :- objects(X,NAME,STR,DEF,_), !, write('There is a '), write(NAME), nl, addToInventory(NAME,STR,DEF).
checkobject(X) :- nextmove.
addToInventory(NAME,STR,DEF) :- asserta(inventory(NAME,STR,DEF)), write('You add item to inventory. Press "i" to see your inventory.'),nl.

openinventory :-
inventory(_,_,_),!,
forall((inventory(NAME,STR,DEF)), (write(NAME),write('-STR:'),write(STR),write(',DEF:'),write(DEF),nl)),nl,write('For equip item write equip(itemname).'),nl.

equip(NAME) :-
objects(_,NAME,STR,_,T),
T == 'weapon', 
retract(weapon(_,_)),!, asserta(weapon(NAME,STR)),updatestats(STR,defense(_)).
equip(NAME) :-
objects(_,NAME,STR,_,T),
T == 'weapon', asserta(weapon(NAME,STR)),updatestats(STR,defense(_)),write('Item equiped.'),nl,fail.


updatestats(STR,DEF) :-
retract(strenght(_)),
asserta(strenght(STR)),
retract(defense(_)),
asserta(defense(DEF)).


move(X) :- 
pos(Z), 
path1(Z,Y,X),
retractall(pos(Z)), 
asserta(pos(Y)),
checkenemy(Y),
checkwin(Y),
nextmove.

n :- move(n).
s :- move(s).
w :- move(w).
e :- move(e).
i :- openinventory.

checkwin(Y) :-
Y == [4,7],!, write('You win the game!'), false.
checkwin(Y) :-
true.


start :- write('You are in dungeon. You can move to:'),nl, asserta(pos([1,3])),asserta(strenght(2)),asserta(defense(0)), pos(Z), path1(Z,_,X), write(X),write(' '), fail.
nextmove :- write('You are in dungeon.You can move to: '),pos(Z), path1(Z,_,X),nl, write(X),write(' '),fail.
