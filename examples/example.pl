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

property(good_property(pos_int,pos_int)).
property(append_not_ok_0(list)).
property(append_not_ok(list,list)).
property(always_hold(int,int)).
property(always_hold_3(list(3,[int,float]))).
property(always_hold_3(list([int,int,int]))).
property(append_ok(list,list)).