% Prints a header and each row: Sum, I, J, Sublist
print_sub_lists([]).
print_sub_lists([(I, J, Sum, Sub)|Rest]) :-
    format('~w   ~w   ~w   ~w~n', [Sum, I, J, Sub]),
    print_sub_lists(Rest).

% Wrapper to print with a header
printSubLists(List) :-
    write('size  i   j   sublist'), nl,
    print_sub_lists(List).


% Sum of a list
my_sum([], 0).
my_sum([X|Xs], Sum) :-
    my_sum(Xs, RestSum),
    Sum is X + RestSum.

% Take K
takeK(0, _, []).
takeK(_, [], []).
takeK(K, [X|Xs], [X|Ys]) :-
    K > 0,
    K1 is K - 1,
    takeK(K1, Xs, Ys).

% Get sublist from index I to J (inclusive)
% Int -> Int -> [a] -> [I,J,[a]]
subList(I, J, List, SubList) :-
    findall(Value,
            (between(I, J, Index),
             getValue(Index, List, Value)),
            SubList).

% Get the element at index N
% nth0
% Int -> [a] -> Int
getValue(0, [X|_], X).
getValue(N, [_|Xs], Value) :-
    N > 0,
    N1 is N - 1,
    getValue(N1, Xs, Value).

% [a] -> [Int,Int,[a]] 
allSubLists(List, AllSubLists) :-
    length(List, N),
    MaxIndex is N - 1,
    findall((I, J, Sub),
            (between(0, MaxIndex, I),
             between(I, MaxIndex, J),
             subList(I, J, List, Sub)),
            AllSubLists).

% [Int,Int,[a]] -> [Int,Int,Int,[a]]
sumSubLists(ListIn, ListOut) :- 
    findall((I,J,Sum,Sub),
        (member((I, J, Sub), ListIn),
        my_sum(Sub, Sum)), 
        ListOut).







% [Int,Int,Int,[a]] -> (Sorted) [Int,Int,Int,[a]]
insert_sort(List, Sorted):-i_sort(List,[],Sorted).

i_sort([], Acc, Acc).
i_sort([X|Xs], Acc, Sorted) :- insert(X, Acc, NAcc), i_sort(Xs, NAcc, Sorted).

insert((I, J, Sum, Sub), [], [(I, J, Sum, Sub)]).
insert((I, J, Sum, Sub), [(I2, J2, Sum2, Sub2)|Xs], [(I, J, Sum, Sub), (I2, J2, Sum2, Sub2)|Xs]) :-
    Sum < Sum2.
insert((I, J, Sum, Sub), [(I2, J2, Sum2, Sub2)|Xs], [(I2, J2, Sum2, Sub2)|Xss]) :-  
    Sum >= Sum2,
    insert((I, J, Sum, Sub), Xs, Xss).

% [K,[a]] -> (K) [Int,Int,Int,[a]]
smallestKset(K, List, Result) :-
    allSubLists(List, AllSubLists),
    sumSubLists(AllSubLists, SumSubLists),
    insert_sort(SumSubLists, Sorted),
    takeK(K, Sorted, Result).


printList :-
    allSubLists([1,5,6,8,3], AllSubLists),
    sumSubLists(AllSubLists, SumSubLists),
    write(SumSubLists), nl.



% Main test driver
main :-
    % Test 1
    nl, write('Test 1'),nl, nl,
    K1 = 15,
    List1 = [-1,2,-3,4,-5,6,-7,8,-9,10,-11,12,-13,14,-15,16,-17,18,-19,20,-21,22,-23,24,-25,26,-27,28,-29,30,-31,32,-33,34,-35,36,-37,38,-39,40,-41,42,-43,44,-45,46,-47,48,-49,50,-51,52,-53,54,-55,56,-57,58,-59,60,-61,62,-63,64,-65,66,-67,68,-69,70,-71,72,-73,74,-75,76,-77,78,-79,80,-81,82,-83,84,-85,86,-87,88,-89,90,-91,92,-93,94,-95,96,-97,98,-99,100],
    smallestKset(K1,List1,Result1),
    printSubLists(Result1),

    % Test 2
    nl, write('Test 2'), nl, nl,
    K2 = 6,
    List2 = [24, -11, -34, 42, -24, 7, -19, 21],
    smallestKset(K2,List2,Result2),
    printSubLists(Result2),

    % Test 3
    nl, write('Test 3'), nl, nl,
    K3 = 8,
    List3 = [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3],
    smallestKset(K3,List3,Result3),
    printSubLists(Result3).

