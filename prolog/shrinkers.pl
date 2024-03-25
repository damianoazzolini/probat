% shrink integers
% try: changing the sign 
% try: 0
% try: bisection
% shrink(Type,Value,Shrank).

% to keep together shrinks for the same type
:- discontiguous shrink/3.

% get_length(N,NA): NA is the number of atoms composing N
% or the length of a list.
% Does not consider the minus sign at the beginning of a number.
% Examples:
% get_length(1,NA), NA = 1.
% get_length(-1,NA), NA = 1.
% get_length(33,NA), NA = 2.
% get_length([33],NA), NA = 1.
% get_length([33,1],NA), NA = 2.
% get_length([[33,1]],NA), NA = 2.
get_length(N,NA):-
    atomic(N), 
    N \= [], !, % the empty list is atomic
    ( atom_chars(N,[-|A]) -> true ; atom_chars(N,A)), % do not consider the - sign
    length(A,NA).
get_length(L,S):-
    is_list(L),
    ( L = [LIn], is_list(LIn) ->  
    	get_length(LIn,S) ;
    	length(L,S)
    ).


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

% % cartesian_product_2/3: given a list of two lists as first
% % arguments, generates two lists, one for the first value
% % and one for the second value. Used to generate alternatives
% % while shrinking.
% % Example:
% % cartesian_product_2([[1, 2, 3], [a, b,c]], R0, R1).
% % R0 = [1, 1, 1, 2, 2, 2, 3, 3, 3],
% % R1 = [c, b, a, c, b, a, c, b, a]
% % TODO: fix this, flatten is not ok for lists
% cartesian_product_2([A,B],Alternatives0,Alternatives1):-
%     cartesian_product_2(A,B,[],[],CPL0,CPL1),
%     flatten(CPL0,LF0),
%     predsort(my_compare,LF0,Alternatives0), !,
%     flatten(CPL1,LF1),
%     predsort(my_compare,LF1,Alternatives1), !.
% cartesian_product_2([],_,L0,L1,L0,L1).
% cartesian_product_2([A|TA],LB,LT0,LT1,FL0,FL1):-
%     length(LB,N),
%     length(LA,N),
%     maplist(=(A),LA),
%     cartesian_product_2(TA,LB,[LA|LT0],[LB|LT1],FL0,FL1).

% generate_shrinking_alternatives/3: generates possible shrinks
% for the term of type specified in the first argument starting
% from the value specified in the second argument (Value) and
% returns them in the list ShrankList.
generate_shrinking_alternatives(Type,Value,ShrankList):-
    findall(S,shrink(Type,Value,S),LS),
    predsort(my_compare,LS,ShrankList), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% shrink(Type,Value,Shrank): Shrank is an attempt to shrink the value Value of type Type
% shrink for numbers (int, float, or numbers)
% number: try 0
shrink(Type,_,0):-
    member(Type,[int,float,number]).
% number: try changing sign
shrink(Type,Value,ChangedSign):-
    member(Type,[int,float,number]),
    ChangedSign is -Value.
% number: bisect
shrink(Type,Value,Shrank):-
    member(Type,[int,float,number]),
    setting(depth,MaxAttempts),
    ( Value > 0 ->
        LB is -Value, UB = Value ;
        LB is Value, UB is -Value
    ),
    LStartingPoints = [left,right],
    member(StartingPoint,LStartingPoints),
    shrink_bisect_number(Type,MaxAttempts,StartingPoint,LB,UB,Shrank).
shrink_bisect_number(MaxAttempts,_,LB,UB,Shrank):-
    MaxAttempts > 0,
    LB < UB,
    ( LB < 0 ->
        Shrank is UB + LB ;
        Shrank is UB - LB
    ).
shrink_bisect_number(MaxAttempts,left,LB,UB,Shrank):-
    member(Type,[int,float,number]),
    MaxAttempts > 0,
    LB < UB,
    ( Type = int -> 
        LB1 is floor(LB/2) ;
        LB1 is LB/2
    ),
    LB1 \= LB, % to avoid 1
    M1 is MaxAttempts - 1,
    shrink_bisect_number(M1,right,LB1,UB,Shrank).
shrink_bisect_number(Type,MaxAttempts,right,LB,UB,Shrank):-
    MaxAttempts > 0,
    LB < UB,
    ( Type = int -> 
        UB1 is floor(UB/2) ;
        UB1 is UB/2
    ),
    UB1 \= UB, % to avoid 1
    M1 is MaxAttempts - 1,
    shrink_bisect_number(M1,left,LB,UB1,Shrank).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sublist(List,Start,End,Sublist) :-
    findall(El,(between(Start,End,Idx),nth1(Idx,List,El)),Sublist).
% lists: try empty
shrink(Type,_,[]):-
    ( Type = list ; Type = list(*,_) ).
% lists of any length: bisect it
shrink(Type,List,Shrank):-
    % list of arbitrary length: bisect
    ( Type = list ; Type = list(*,_) ),
    setting(depth,MaxAttempts),
    LStartingPoints = [left,right],
    member(StartingPoint,LStartingPoints),
    length(List,LenList),
    shrink_bisect_list(MaxAttempts,List,StartingPoint,1,LenList,Shrank).
shrink_bisect_list(MaxAttempts,List,_,Start,End,Shrank):-
    MaxAttempts > 0,
    Start < End,
    sublist(List,Start,End,Shrank).
% increase the position of the first element
shrink_bisect_list(MaxAttempts,List,left,Start,End,Shrank):-
    MaxAttempts > 0,
    Start < End,
    S1 is floor((End + Start)/2),
    M1 is MaxAttempts - 1,
    shrink_bisect_list(M1,List,right,S1,End,Shrank).
% increase the position of the last element
shrink_bisect_list(MaxAttempts,List,right,Start,End,Shrank):-
    MaxAttempts > 0,
    Start < End,
    E1 is ceil((End + Start)/2),
    M1 is MaxAttempts - 1,
    shrink_bisect_list(M1,List,left,Start,E1,Shrank).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO
get_type(A,int):- integer(A).
get_type(A,float):- float(A).
get_type(A,float):- float(A).
shrink(list(N,Types),List,Shrank):-
    % list of length N of types only Types
    integer(N),
    maplist(get_type,List,TypeIndex), % TODO: these must be in types
    maplist(shrink,TypeIndex,List,Shrank). % TODO: finish this: the values must be aggregated
shrink(list(Types),List,Shrank):-
    % list of length len(Types) where each element is of type Types_i in Types
    maplist(shrink,Types,List,Shrank). % TODO: finish this: the values must be aggregated

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%