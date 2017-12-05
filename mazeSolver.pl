:- module(mazeSolver, []).
:- use_module(mazeInfo, [info/3, wall/2, button/3, num_buttons/1, start/2, goal/2]).


main :- 
	num_buttons(N),
	myloop(N).

/* Find a Path from one square to another. */
findPath(Start, Dest, [Dest]) :-
	valid_move(Start, Dest).
	
findPath(Start, Dest, [Head|Tail]) :-
	vaild_move(Start, Head),
	findPath(Head, Dest, Tail).
	

edge(1,2).
edge(1,4).
edge(1,3).
edge(2,3).
edge(2,5).
edge(3,4).
edge(3,5).
edge(4,5).

connected(X,Y) :- edge(X,Y).
connected(X,Y) :- edge(Y,X).

path(A,B,Path) :-
       travel(A,B,[A],Q), 
       reverse(Q,Path).

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
	findPath([X,Y], [X1,Y1], [ [X1,Y1] | DaList]),
	I = N, 
	!, write_list_to_file("path-solution.txt", DaList).

/* Did we get these from stack overflow? yes, yes we did */
loop_through_list(File, List) :-
    member(Element, List),
    write(File, Element),
    write(File, '\n'),
    fail.


write_list_to_file(Filename,List) :-
    open(Filename, write, File),
    \+ loop_through_list(File, List),
    close(File).



sGOAL(I, X, Y) :- I =:= 1, start(X,Y).
sGOAL(I, X, Y) :- Z is I-1, button(X,Y, Z).

eGOAL(I, X, Y) :- goal(X, Y), num_buttons(I).
eGOAL(I, X, Y) :- button(X,Y, I).
 



%current([A,B]).  
%current([A1,B1]) :- current[A11,B11], [A,B] is [A11,B11].
 
%moved([A,B], seenLocations, num_buttons_pressed) :- 
	/* Move to clean adjcent square */
	/* Check for buttons? */
	/* update num buttons pressed */
	
	

%findPath :- moved(start(A,B), seenLocations, num_buttons_pressed ),
%	write([A,B]), nl,
%	done([A,B], num_buttons_pressed),
%	!. 
	
	

/* Check if the two squares are adjacent */
adj_square([X0,Y0], [X0,Y1]) :- (Y1 is Y0-1), inside_board(X0,Y1), \+ wall(X0,Y1).
adj_square([X0,Y0], [X0,Y1]) :- (Y1 is Y0+1), inside_board(X0,Y1), \+ wall(X0,Y1).
adj_square([X0,Y0], [X1,Y0]) :- (X1 is X0-1), inside_board(X1,Y0), \+ wall(X1,Y0).
adj_square([X0,Y0], [X1,Y0]) :- (X1 is X0+1), inside_board(X1,Y0), \+ wall(X1,Y0).


/* The square is traversable (not blocked by wall) */
traversable_square([X,Y]) :- traversable_square(X,Y).
traversable_square(X0,Y0) :- inside_board(X0,Y0), \+ wall(X0,Y0).
												 /* square is not a wall */

/* Check if the square is within the board boundaries given info(width,height,c) */
inside_board(W,H) :- 
	info(Width,Height,_), 
	W >= 0,
	H >= 0, 
	W < Width,
	H < Height.


%whatisLove(X,Y) :- button(X,Y).

/* Check to see if goal */
%done([X0,Y0], button_Pressed) :- goal(X0,Y0) , num_buttons(button_Pressed).

%button_Pressed(0).
%button_Pressed(N) :- button_Pressed(M), N is M+1.
/* Check to see if at correct button */
atButton(X,Y, ID) :- 
	button(X, Y, ID).

/* Move iff the square we want to move to is adjacent & not traversed & traversable(inside the board & not a wall) */
valid_move([X0,Y0], [X1,Y1]) :- 
	traversable_square(X1,Y1),
	adj_square([X0,Y0], [X1,Y1]).
	/* traversed([X1,Y1]), */
	/* traversed([X1,Y1]), */
	
	

	
/* So add things to a list of things when they have been hit, and remove them at goal */	
%traversed() :- 


/* check if the square is a member of the traversed list */
%member(Square,[Square|_]).
%member(Square,[_|T]) :- member(Square,T).