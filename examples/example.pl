:- use_module(library(probat)).

always_hold(A,B):-
    A > 0,
    B > 0.
always_hold_3([A,B,C]):-
    A > B, B > C.

append_ok(A,B):-
    append(A,B,C),
    length(A,NA),
    length(B,NB),
    length(C,NC),
    NC is NA + NB.

good_property(A,B):-
    C is A + B,
    C >= A,
    C >= B.

append_not_ok(A,B):-
    append(A,B,C),
    length(A,NA),
    length(B,NB),
    length(C,NC),
    NC is NA + NB - 1.
append_not_ok_0(A):-
    append(A,[a],C),
    length(A,NA),
    length(C,NC),
    NC is NA + 8.

prova(A):-
    A > 1.

prop_1(A):-
    A > -568.

my_generator(A):-
    random_between(1,3,A).

ok_reverse(List):-
    reverse(List,L1),
    reverse(L1,List).

prop_atoms(AT):-
    atom_codes(AT,C),
    length(C,N),
    N > 5.
prop_string(AT):-
    string_codes(AT,C),
    length(C,N),
    N > 5.
proper_atoms(AT):-
    atom(AT).
proper_string(S):-
    string(S).

property(proper_string(string)).
property(proper_string(string(4,5))).
property(prop_string(string(1,2))).
property(prop_atoms(atom)).
property(prop_atoms(atom(3,3))).
property(proper_atoms(atom)).
property(prop_atoms(atom(5,6))).
property(prop_atoms(atom(6,6))).

property(prop_1(number)).
property(prova(my_generator)).
property(prova(int)).
property(prova(int(2,3))).
property(prova(number)).
property(prova(number(2,3))).
property(prova(float)).
property(prova(float(2.0,3.0))).
property(prova(var)).
property(prova(list)).
property(good_property(pos_int,pos_int)).
property(append_not_ok_0(list)).
property(append_not_ok(list,list)).
property(append_not_ok(list([list([list])]),list)).
property(always_hold(int,int)).
property(always_hold_3(list(3,[int,float]))).
property(always_hold_3(list([int,int,int]))).
property(append_ok(list,list)).