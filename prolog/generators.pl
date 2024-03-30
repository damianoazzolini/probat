:- discontiguous random_element/2.

type(int).
type(float).
generator(int).
generator(pos_int).
generator(pos_int_no_zero).
generator(neg_int).
generator(neg_int_no_zero).
generator(float).
generator(pos_float).
generator(neg_float).
generator(number).
generator(list).
generator(atom).
generator(string).
% generator(nonempty_list). % TODO?
generator(any).
generator(var). % variable

provided_composite_generators([int(_,_), float(_,_), number(_,_), list(_), list(_,_), atom(_,_), string(_,_)]).

% valid_generator(Generator): check whether Generator is a valid generator
valid_generator(Generator):-
    generator(Generator), !.
valid_generator(int(L,U)):-
    maplist(integer,[L,U]),
    U > L, !.
valid_generator(float(L,U)):-
    maplist(float,[L,U]),
    U > L, !.
valid_generator(number(L,U)):-
    maplist(number,[L,U]),
    U > L, !.
valid_generator(list(A)):-
    integer(A), !.
valid_generator(list(L)):- 
    is_list(L),
    maplist(valid_generator,L), !.
valid_generator(list(N,L)):-
    ((integer(N), N > 0) ; N = any),
    maplist(valid_generator,L), !.
valid_generator(atom(L,U)):-
    integer(L), L >= 1,
    integer(U), U >= L, !.
valid_generator(string(L,U)):-
    integer(L), L >= 1,
    integer(U), U >= L, !.
% user defined generator
valid_generator(G):-
    ground(G),
    G =.. [Name|Args],
    append([_],Args,ArgsTrue),
    G1 =.. [Name|ArgsTrue],
    clause(G1,_), !.

% variable
random_element(var,_).

% any element: number or list
random_element(any,V):-
    random_member(T,[number,list,atom,string]),
    random_element(T,V).

% for integers
% random_element(int,V): V is a random integer between -2**31 and 2**31 
% random_element(pos_int,V): V is a random integer between 0 and 2**31 
% random_element(neg_int,V): V is a random integer between -2**31 and 0 
% random_element(int(L,U),V): V is a random integer between L and U 
random_element(int,V):-
    setting(min_val,MinV),
    setting(max_val,MaxV),
    random_element(int(MinV,MaxV),V).
random_element(pos_int,V):-
    setting(max_val,MaxV),
    random_element(int(0,MaxV),V).
random_element(pos_int_no_zero,V):-
    setting(max_val,MaxV),
    random_element(int(1,MaxV),V).
random_element(neg_int,V):-
    setting(min_val,MinV),
    random_element(int(MinV,0),V).
random_element(neg_int_no_zero,V):-
    setting(min_val,MinV),
    random_element(int(MinV,-1),V).
random_element(int(Lower,Upper),V):-
    random_between(Lower, Upper, V).

% for floats
% random_element(float,V): V is a random float between -2**31 and 2**31 
% random_element(pos_float,V): V is a random float between 0 and 2**31 
% random_element(neg_float,V): V is a random float between -2**31 and 0 
% random_element(float,L,U,V): V is a random float between L and U 
random_element(float,V):-
    setting(min_val,MinV),
    setting(max_val,MaxV),
    random_element(float(MinV,MaxV),V).
random_element(pos_float,V):-
    setting(max_val,MaxV),
    random_element(float(0,MaxV),V).
random_element(neg_float,V):-
    setting(min_val,MinV),
    random_element(float(MinV,0),V).
random_element(float(Lower,Upper),V):-
    random(R),
    V is R*(Upper - Lower) + Lower.

% for number (int or float)
% Check: when float, I allow integers as arguments, explicitly
% not allowed by the initial float check. That is,
% number(2,3) may call float(2,3) while a direct call to
% float(2,3) raises an error in the generator checking phase.
random_element(number,V):-
    random_member(T,[int,float]),
    random_element(T,V).
random_element(number(L,U),V):-
    random_member(T,[int,float]),
    RE =.. [T,L,U],
    random_element(RE,V).

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
    setting(max_len_list, MaxLen),
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
    ( N = any -> % any length
        setting(max_len_list, MaxLen),
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

% atoms
random_element(atom,AT):-
    setting(max_len_list, MaxLen),
    random_element(atom(1,MaxLen),AT).
random_element(atom(L,U),AT):-
    random_between(L,U,Len),
    length(LAtom,Len),
    maplist(random_between(0x20, 0x7e),LAtom),
    atom_codes(AT,LAtom).
random_element(string,String):-
    random_element(atom,Atom),
    atom_string(Atom,String).
random_element(string(L,U),String):-
    random_element(atom(L,U),Atom),
    atom_string(Atom,String).
    

% user defined random element
random_element(G,El):-
    \+ generator(G),
    provided_composite_generators(CG),
    \+ member(G,CG), !,
    G =.. [Name|Args],
    append([El],Args,ArgsTrue),
    G1 =.. [Name|ArgsTrue],
    catch(G1, _Error, (
        format("Error in generating custom values from ~w~n",[G1]),
        false
    )), !.
