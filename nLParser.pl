:- module(nLParser, []).
:- use_module(mazeInfo, [info/3, wall/2, button/3, num_buttons/1, start/2, goal/2]).


/* Sentence contains a subject and verb phrase */
sentence(Phrase1, Phrase2, Dir, Count) :-
    subjectPhrase(Phrase1, Body), verbPhrase(Body, Phrase2,  Dir, Count).

/* Subject Phrase is same for both structures */
subjectPhrase([Subject | Phrase2], Phrase2) :- subject(Subject).
subjectPhrase([Article, Subject | Phrase2], Phrase2) :- article(Article), subject(Subject).

directionPhrase([Number, Subject, Direction | Phrase2], Phrase2, Direction, Number) :- 
    num(Number), subject(Subject), direction(Direction).

/* Structure 1 */
verbPhrase([Verb | Body], Phrase2, Dir, Count) :-
    verb(Verb), directionPhrase(Body, Phrase2, Dir, Count).   
    
/* Structure 2 */
verbPhrase([Verb | Body], Phrase2, _, '-1') :-
    verb(Verb), subjectPhrase(Body, Phrase2).


%%%%%%%%%

article(the).
article(a).
subject(rat).
subject(rodent).
subject(einstein).
subject(it).
subject(he).
subject(button).
subject(square).
subject(squares).
subject(cell).
subject(cells).
verb(ran).
verb(moved).
verb(pushed).
verb(scurried).
direction(up).
direction(down).
direction(left).
direction(right).
num('1').
num('2').
num('3').
num('4').
num('5').
num('6').
num('7').
num('8').
num('9').



main :-
    open('NL-input.txt', read, Str),
    read_file(Str,Lines),
    %Convert the lines in file to an list of sentences that are lists of words
    lines_to_words(Lines, Words),
    close(Str),
    start(X,Y),
    open("NL-parse-solution", write, File),
    testSentences(Words, [X,Y], File),
    close(File).


sentToAtom([] , [] ) :- !.
sentToAtom([SS | N] , [AS | AN]):-
    atom_string(AS, SS),
    sentToAtom(N, AN).


testSentences([], _ , _) :- !.
testSentences([Sent | Next], Pos, File):-
    sentToAtom(Sent, Atomz),
    %isSentence(Atomz),
    sentence(Atomz, [], Dir, Count),
    atom_number(Count,X),
    validMotion(Pos, X, Dir, NewPos),
    %write("Valid Move"), nl,
    write(File, "Valid Move"),
    write(File, '\n'),
    testSentences( Next, NewPos, File ).
    
testSentences([Sent | Next], Pos, File):-
    sentToAtom(Sent, Atomz),
    \+sentence(Atomz, [], _ , _),
    %write("Not a valid sentence"), nl,
    write(File, "Not a valid sentence"),
    write(File, '\n'),
    testSentences(Next, Pos, File).
    
testSentences([Sent | Next], Pos, File):-
    sentToAtom(Sent, Atomz),
    sentence(Atomz, [], Dir, Count),
    atom_number(Count,X),
    \+validMotion(Pos, X, Dir, _),
    %write("Not a valid move"), nl,
    write(File, "Not a valid Move"),
    write(File, '\n'),
    testSentences(Next, Pos, File).




/*Then we can figure if his movments are even potentially valid */
validMotion([X,Y], _, pushbutton, [X,Y] ) :- button(X,Y, _).
validMotion(OutPos, 0, _, OutPos) :- !.
validMotion([X,Y], Count, left, OutPos ):-  
    X1 is X - 1,
    DownCount is Count - 1,
    adj_square([X,Y], [X1, Y]),
    validMotion([X1,Y], DownCount, left, OutPos).

validMotion([X,Y], Count, right, OutPos):- 
    X1 is X + 1,
    DownCount is Count - 1,
    adj_square([X,Y], [X1, Y]),
    validMotion([X1,Y], DownCount, right, OutPos).

validMotion([X,Y], Count, down, OutPos):-
    Y1 is Y + 1,
    DownCount is Count - 1,
    adj_square([X,Y], [X, Y1]),
    validMotion([X,Y1], DownCount, down, OutPos).

validMotion([X,Y], Count, up, OutPos):-
    Y1 is Y - 1,
    DownCount is Count - 1,
    adj_square([X,Y], [X, Y1]),
    validMotion([X,Y1], DownCount, up, OutPos).



% Credit to StackOverflow and author Ishq for file parser
% https://stackoverflow.com/a/4805931
% https://stackoverflow.com/users/577045/ishq
read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read_file(Stream,L).

%Converts sentence to a list of words
lines_to_words([], []).
lines_to_words([H|T], [H2|T2]) :-
	split_string(H, " ", "", H2),
	lines_to_words(T, T2).
	
	
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
