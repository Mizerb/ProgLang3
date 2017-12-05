:- module(nLParser, []).
:- use_module(mazeInfo, [info/3, wall/2, button/3, num_buttons/1, start/2, goal/2]).


/* Sentence contains a subject and verb phrase */
%sentence(Phrase1, Phrase2, Dir, Count) :-
%    subjectPhrase(Phrase1, Body), verbPhrase(Body, Phrase2, Dir, Count).

/* Subject Phrase is same for both structures */
%subjectPhrase([Subject | Phrase2], Phrase2) := subject(Subject).
%subjectPhrase([Article, Subject | Phrase2], Phrase2) :- article(Article), subject(Subject).

%directionPhrase([Number, Subject, Direction | Phrase2], Phrase2, NumberN, Direction) :- 
%    num(Number), subject(Subject), direction(Direction), atom_number(Number,NumberN).

/* Structure 1 */
%verbPhrase([Verb | Body], Phrase2, Dir, Count) :-
%    verb(Verb), directionPhrase(Body, Phrase2, Dir, Count).   
    
/* Structure 2 */
%verbPhrase([Verb | Body], Phrase2, buttonpush, -1) :-
%    verb(Verb), subjectPhrase(Body, Phrase2).


%%%%%%%%%

/* Sentence contains a subject and verb phrase */
sentence(Phrase1, Phrase2) :-
    subjectPhrase(Phrase1, Body), verbPhrase(Body, Phrase2).

/* Subject Phrase is same for both structures */
subjectPhrase([Subject | Phrase2], Phrase2) :- subject(Subject).
subjectPhrase([Article, Subject | Phrase2], Phrase2) :- article(Article), subject(Subject).

directionPhrase([Number, Subject, Direction | Phrase2], Phrase2) :- 
    num(Number), subject(Subject), direction(Direction).

/* Structure 1 */
verbPhrase([Verb | Body], Phrase2) :-
    verb(Verb), directionPhrase(Body, Phrase2).   
    
/* Structure 2 */
verbPhrase([Verb | Body], Phrase2) :-
    verb(Verb), subjectPhrase(Body, Phrase2).

%article("the").
%article("a").
%subject("rat").
%subject("rodent").
%subject("einstein").
%subject("it").
%subject("he").
%subject("button").
%subject("square").
%subject("squares").
%subject("cell").
%subject("cells").
%verb("ran").
%verb("moved").
%verb("pushed").
%verb("scurried").
%direction("up").
%direction("down").
%direction("left").
%direction("right").
%num("1").
%num("2").
%num("3").
%num("4").
%num("5").
%num("6").
%num("7").
%num("8").
%num("9").

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

%isSentence(X) :- sentence(X,_,_, [ ]).
isSentence(X) :- sentence(X, [ ]).


main :-
    open('NL-input.txt', read, Str),
    read_file(Str,Lines),
    %Convert the lines in file to an list of sentences that are lists of words
    lines_to_words(Lines, Words),
    close(Str),
    /*write(Words), nl,*/
    start(X,Y),
    %inSent(Words).
    testSentences(Words, [X,Y]).


inSent( [Wordz| _ ] ):-
    sentToAtom(Wordz, Att),
    isSentence(Att),
    write("HUZZA"),nl,!.
inSent( [Wordz| _ ] ):-
    sentToAtom(Wordz, Att),
    \+isSentence(Att),
    write("FUCK"),nl,!.

sentToAtom([] , [] ) :- !.
sentToAtom([SS | N] , [AS | AN]):-
    atom_string(AS, SS),
    sentToAtom(N, AN).


testSentences([], _) :- !.
testSentences([Sent | Next], Pos):-
    sentToAtom(Sent, Atomz),
    isSentence(Atomz),
    %validMotion(Pos, Count, Dir,  NewPos),
    write("Valid Move"), nl,
    testSentences( Next, Pos ).
    
testSentences([Sent | Next], Pos):-
    sentToAtom(Sent, Atomz),
    \+isSentence(Atomz),
    write("Not a valid sentence"), nl,
    testSentences(Next, Pos).
    
testSentences([Sent | Next], Pos):-
    sentToAtom(Sent, Atomz),
    isSentence(Atomz),
    %\+validMotion(Pos, Count, Dir,  _),
    write("Not a valid move"), nl,
    testSentences(Next, Pos).




/*Then we can figure if his movments are even potentially valid */
validMotion([X,Y], _, pushbutton, [X,Y] ) :- button(X,Y, _).
validMotion(OutPos, 0, _, OutPos) :- !.
validMotion([X,Y], Count, left, OutPos ):-  
    X1 is X - 1,
    DownCount is Count - 1,
    adj_square([X,Y], [X1, Y]),
    validMotion([X1,Y], DownCount, left, OutPos).

validMotion([X,Y], Count, "right", OutPos):- 
    X1 is X + 1,
    DownCount is Count - 1,
    adj_square([X,Y], [X1, Y]),
    validMotion([X1,Y], DownCount, "right", OutPos).

validMotion([X,Y], Count, "down", OutPos):-
    Y1 is Y + 1,
    DownCount is Count - 1,
    adj_square([X,Y], [X, Y1]),
    validMotion([X,Y1], DownCount, "down", OutPos).

validMotion([X,Y], Count, "up", OutPos):-
    Y1 is Y - 1,
    DownCount is Count - 1,
    adj_square([X,Y], [X, Y1]),
    validMotion([X,Y1], DownCount, "up", OutPos).



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