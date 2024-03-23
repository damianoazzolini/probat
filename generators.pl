type(int).
type(float).
generator(int).
generator(pos_int).
generator(neg_int).
generator(float).
generator(pos_float).
generator(neg_float).
generator(number).
generator(list).

% valid_generator(Generator): check whether Generator is a valid generator
valid_generator(Generator):-
    generator(Generator), !.
valid_generator(int(L,U)):-
    integer(L),
    integer(U),
    U > L.
valid_generator(float(L,U)):-
    float(L),
    float(U),
    U > L.
valid_generator(number(L,U)):-
    number(L),
    number(U),
    U > L.
valid_generator(list(A)):-
    integer(A).
valid_generator(list(L)):- 
    is_list(L),
    maplist(valid_generator,L).
valid_generator(list(N,L)):-
    (integer(N) ; N = *),
    maplist(valid_generator,L).


% for integers
% random_element(int,V): V is a random integer between -2**31 and 2**31 
% random_element(pos_int,V): V is a random integer between 0 and 2**31 
% random_element(neg_int,V): V is a random integer between -2**31 and 0 
% random_element(int,L,U,V): V is a random integer between L and U 
random_element(int,V):-
    setting(minVal,MinV),
    setting(maxVal,MaxV),
    random_element(int,MinV,MaxV,V).
random_element(pos_int,V):-
    setting(maxVal,MaxV),
    random_element(int,0,MaxV,V).
random_element(neg_int,V):-
    setting(minVal,MinV),
    random_element(int,MinV,0,V).
random_element(int,Lower,Upper,V):-
    random_between(Lower, Upper, V).

% for floats
% random_element(float,V): V is a random float between -2**31 and 2**31 
% random_element(pos_float,V): V is a random float between 0 and 2**31 
% random_element(neg_float,V): V is a random float between -2**31 and 0 
% random_element(float,L,U,V): V is a random float between L and U 
random_element(float,V):-
    setting(minVal,MinV),
    setting(maxVal,MaxV),
    random_element(float,MinV,MaxV,V).
random_element(pos_float,V):-
    setting(maxVal,MaxV),
    random_element(float,0,MaxV,V).
random_element(neg_float,V):-
    setting(minVal,MinV),
    random_element(float,MinV,0,V).
random_element(float,Lower,Upper,V):-
    random(R),
    V is R*(Upper - Lower) + Lower.

% for number (int or float)
random_element(number,V):-
    random_member(T,[int,float]),
    random_element(T,V).
random_element(number,L,U,V):-
    random_member(T,[int,float]),
    random_element(T,L,U,V).

% pick_random_type(V): V is a random type among all possible types
% pick_random_type(Allowed,V): V is a random type among the allowed
pick_random_type(V):-
    findall(T,type(T),LT),
    random_member(Type,LT),
    random_element(Type,V).
pick_random_type(Allowed,V):-
    random_member(Type,Allowed),
    random_element(Type,V).

% for lists
% random_element(list,L): L is a list of random length of any type
% random_element(list(N), L): L is a list of fixed length N of random types
% random_element(list(N,LT),L): L is a list of length N (possibly random, *) of types LT
% random_element(list(LT),L): L is a list of the same length of LT where each element of LT
%   denotes a type in that specific position
random_element(list,L):-
    setting(maxLenList, MaxLen),
    random_between(0, MaxLen, Len), % ok empty list
    length(L, Len),
    maplist(pick_random_type,L).
% list of fixed length of random types
random_element(list(N), L):-
    integer(N), !,
    length(L, N),
    maplist(pick_random_type, L).
% list of fixed types LT of length N
random_element(list(N,LT),L):-
    ( N = * -> % any length
        setting(maxLenList, MaxLen),
        random_between(0, MaxLen, Len) ;
        Len = N
    ),
    length(L, Len),
    maplist(pick_random_type(LT), L).
% list of the same length of LT where each element of LT
% denotes a type
random_element(list(LT),L):-
    length(LT,N), !,
    length(L,N),
    maplist(random_element, LT, L).