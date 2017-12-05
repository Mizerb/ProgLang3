:- module(mazeSolver, []).
:- use_module(mazeInfo, [info/3, wall/2, button/3, num_buttons/1, start/2, goal/2]).


main :-
	num_buttons(N),
	myloop(N).


path(A,B,Path) :-
       travel(A,B,[A],Q), 
       reverse(Q,Path),
       write_list_to_file("path-solution.txt", Path).

travel(A,B,P,[B|P]) :- 
       adj_square(A,B).
travel(A,B,Visited,Path) :-
       adj_square(A,C),           
       C \== B,
       \+member(C,Visited),
       travel(C,B,[C|Visited],Path).


natural(1).
natural(N) :- natural(M), N is M+1.

myloop(N) :- N > 0, natural(I), sGOAL(I,X,Y ) , eGOAL(I,X1,Y1),
	path([X,Y],[X1,Y1], DaList),
	write_list_to_file("path-solution.txt", DaList)
	I = N, 
	!, fail.

/* Did we get these from stack overflow? yes, yes we did */
loop_through_list(File, List) :-
    member(Element, List),
    write(File, Element),
    write(File, '\n'),
    fail.


write_list_to_file(Filename,List) :-
    open(Filename, append, File),
    \+ loop_through_list(File, List),
    close(File).



sGOAL(I, X, Y) :- I =:= 1, start(X,Y).
sGOAL(I, X, Y) :- Z is I-1, button(X,Y, Z).

eGOAL(I, X, Y) :- goal(X, Y), num_buttons(I).
eGOAL(I, X, Y) :- button(X,Y, I).
 
	

/* Check if the two squares are adjacent */
adj_square([X0,Y0], [X0,Y1]) :- (Y1 is Y0-1), inside_board(X0,Y1), \+ wall(X0,Y1).
adj_square([X0,Y0], [X0,Y1]) :- (Y1 is Y0+1), inside_board(X0,Y1), \+ wall(X0,Y1).
adj_square([X0,Y0], [X1,Y0]) :- (X1 is X0-1), inside_board(X1,Y0), \+ wall(X1,Y0).
adj_square([X0,Y0], [X1,Y0]) :- (X1 is X0+1), inside_board(X1,Y0), \+ wall(X1,Y0).



/* Check if the square is within the board boundaries given info(width,height,c) */
inside_board(W,H) :- 
	info(Width,Height,_), 
	W >= 0,
	H >= 0, 
	W < Width,
	H < Height.
