% Rooms - R1 R2 R3

% Keys - BK = Brass key, SK = Steel key

% Robot - should have inv = 2 spaces

% Goal - Take Package (P) to room R2

%%%%WALKING%%%%

%walk from R1 to R2
move(state(have,BK,P,INV,r1),"From R1 to R2",state(have,BK,P,INV,r2)).

%walk from R2 to R1
move(state(have,BK,P,INV,r2),"From R2 to R1",state(have,BK,P,INV,r1)).

%walk from R1 to R3
move(state(SK,have,P,INV,r1),"From R1 to R3",state(SK,have,P,INV,r3)).

%walk from R3 to R1
move(state(SK,have,P,INV,r3),"From R3 to R1",state(SK,have,P,INV,r1)).

%%%%PICKUP%%%%

%pickup BK
move(state(SK,ROOM,P,INV,ROOM),"Take BK", state(SK,have,P,UpdateINV,ROOM)) :- INV < 2 , UpdateINV is INV + 1.
%pickup SK
move(state(ROOM,BK,P,INV,ROOM),"Take SK", state(have,BK,P,UpdateINV,ROOM)) :- INV < 2 , UpdateINV is INV + 1.
%pickup P
move(state(SK,BK,ROOM,INV,ROOM),"Take PK", state(SK,BK,have,UpdateINV,ROOM)) :- INV < 2 , UpdateINV is INV + 1.

%%%%PICKUP%%%%

%drop BK
move(state(SK,have,P,INV,ROOM),"Drop Bk", state(SK,ROOM,P,UpdateINV,ROOM)) :- UpdateINV is INV - 1.

%drop SK
move(state(have,BK,P,INV,ROOM),"Drop Sk", state(ROOM,BK,P,UpdateINV,ROOM)) :- UpdateINV is INV - 1.

%drop P
move(state(SK,BK,have,INV,ROOM),"Drop P", state(SK,BK,ROOM,UpdateINV,ROOM)) :- UpdateINV is INV - 1.

%%%%SEARCH DFS limited%%%%
solveR(state(_, _, r2,_,_),_, ["The goal is done"|[]]).
solveR(State,N, [OutPut|Trace]) :-
    N > 0,
    move(State,OutPut,NewState),
    NewN is N -1,
    solveR(NewState,NewN,Trace).


start(Trace):-
    solveR(state(r1,r2,r3,0,r1),12,Trace),
    printL(Trace,1).
% Print list helper function
printL([], _).
printL([H|T], Index) :-
    format('~t~d~2|. ~w~n', [Index, H]),
    Next is Index + 1,
    printL(T, Next).

