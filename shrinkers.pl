% shrink integers
% try: changing the sign 
% try: 0
% try: bisection
% shrink(Type,Value,Shrank).

% get_length(N,NA): NA is the number of atoms composing N
% Does not consider the minus sign at the beginning of a term (number)
% Examples:
% get_length(1,NA), NA = 1.
% get_length(-1,NA), NA = 1.
% get_length(33,NA), NA = 2.
get_length(N,NA):-
    ( atom_chars(N,[-|A]) -> true ; atom_chars(N,A)), % do not consider the - sign
    length(A,NA).

% my_compare/3: compares the length of two lists.
% in the > case there is >= to keep duplicates.
% Leaves choice points open, due to the = case.
my_compare(<,N0,N1):-
	get_length(N0,NA0),
    get_length(N1,NA1),
    NA0 < NA1.
my_compare(>,N0,N1):-
	get_length(N0,NA0),
    get_length(N1,NA1),
    NA0 >= NA1. % to keep duplicates
my_compare(=,_N0,_N1).

% cartesian_product_2/3: given a list of two lists as first
% arguments, generates two lists, one for the first value
% and one for the second value. Used to generate alternatives
% while shrinking.
% Example:
% cartesian_product_2([[1, 2, 3], [a, b,c]], R0, R1).
% R0 = [1, 1, 1, 2, 2, 2, 3, 3, 3],
% R1 = [c, b, a, c, b, a, c, b, a]
cartesian_product_2([A,B],Alternatives0,Alternatives1):-
    cartesian_product_2(A,B,[],[],CPL0,CPL1),
    flatten(CPL0,LF0),
    predsort(my_compare,LF0,Alternatives0), !,
    flatten(CPL1,LF1),
    predsort(my_compare,LF1,Alternatives1), !.
cartesian_product_2([],_,L0,L1,L0,L1).
cartesian_product_2([A|TA],LB,LT0,LT1,FL0,FL1):-
    length(LB,N),
    length(LA,N),
    maplist(=(A),LA),
    cartesian_product_2(TA,LB,[LA|LT0],[LB|LT1],FL0,FL1).

% generate_shrinking_alternatives/3: generates possible shrinks
% for the term of type specified in the first argument starting
% from the value specified in the second argument (Value) and
% returns them in the list ShrankList.
generate_shrinking_alternatives(Type,Value,ShrankList):-
    findall(S,shrink(Type,Value,S),LS),
    sort(LS,ShrankList).

shrink(Type,_,0):-
    member(Type,[int,float,number]).
shrink(Type,Value,ChangedSign):-
    member(Type,[int,float,number]),
    ChangedSign is -Value.
shrink(Type,Value,Shrank):-
    member(Type,[int,float,number]),
    setting(depth,MaxAttempts),
    ( Value > 0 ->
        LB is -Value, UB = Value ;
        LB is Value, UB is -Value
    ),
    LStartingPoints = [left,right],
    member(StartingPoint,LStartingPoints),
    shrink_bisect(Type,MaxAttempts,StartingPoint,LB,UB,Shrank).
shrink_bisect(Type,MaxAttempts,_,LB,UB,Shrank):-
    member(Type,[int,float,number]),
    MaxAttempts > 0,
    LB < UB,
    ( LB < 0 ->
        Shrank is UB + LB ;
        Shrank is UB - LB
    ).
shrink_bisect(Type,MaxAttempts,left,LB,UB,Shrank):-
    member(Type,[int,float,number]),
    MaxAttempts > 0,
    LB < UB,
    ( Type = int -> 
        LB1 is floor(LB/2) ;
        LB1 is LB/2
    ),
    LB1 \= LB, % to avoid 1
    M1 is MaxAttempts - 1,
    shrink_bisect(Type,M1,right,LB1,UB,Shrank).
shrink_bisect(Type,MaxAttempts,right,LB,UB,Shrank):-
    LB < UB,
    ( Type = int -> 
        UB1 is floor(UB/2) ;
        UB1 is UB/2
    ),
    UB1 \= UB, % to avoid 1
    M1 is MaxAttempts - 1,
    shrink_bisect(Type,M1,left,LB,UB1,Shrank).

