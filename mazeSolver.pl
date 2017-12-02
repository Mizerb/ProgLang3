:- module(mazeSolver, []).
:- use_module(mazeInfo, [info/3, wall/2, button/3, num_buttons/1, start/2, goal/2]).


main :- 


/* Check if the two squares are adjacent */
adj_square([X0,Y0], [X0,Y1]) :- (Y1 is Y0-1).
adj_square([X0,Y0], [X0,Y1]) :- (Y1 is Y0+1).
adj_square([X0,Y0], [X1,Y0]) :- (X1 is X0-1).
adj_square([X0,Y0], [X1,Y0]) :- (X1 is X0+1).


/* The square is traversable (not blocked by wall) */
traversable_square([X,Y]) :- traversable_square(X,Y).
traversable_square(X0,Y0) :- inside_board(X0,Y0), \+ wall(X0,Y0).
												 %% square is not a wall

/* Check if the square is within the board boundaries given info(width,height,c) */
inside_board(W,H) :- 
	info(Width,Height,_), 
	W >= 0,
	H >= 0, 
	W =< Width,
	H =< Height.


/* Move iff the square we want to move to is adjacent & traversable(inside the board & not a wall) */
valid_move([X0,Y0], [X1,Y1]) :- 
	adj_square([X0,Y0], [X1,Y1]),  
	traversable_square(X1,Y1).