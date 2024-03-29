:- module(test_sanitizers, [test_sanitizers/0]).
:- use_module(library(plunit)).
% :- use_module(library(prolog_coverage)).

:- ['../prolog/generators'].

test_sanitizers:-
    % coverage(run_tests).
    run_tests.

:- begin_tests(valid_generator).
test(basic_0):- valid_generator(int).
test(basic_1):- valid_generator(int).
test(basic_2):- valid_generator(pos_int).
test(basic_3):- valid_generator(pos_int_no_zero).
test(basic_4):- valid_generator(neg_int).
test(basic_5):- valid_generator(neg_int_no_zero).
test(basic_6):- valid_generator(float).
test(basic_7):- valid_generator(pos_float).
test(basic_8):- valid_generator(neg_float).
test(basic_9):- valid_generator(number).
test(basic_10):- valid_generator(list).
test(basic_11):- valid_generator(atom).
:- end_tests(valid_generator).

:- begin_tests(valid_composite_generators).
test(composite_0):- valid_generator(int(1,2)).
test(composite_1):- valid_generator(float(1.0,2.0)).
test(composite_2):- valid_generator(number(1,2)).
test(composite_3):- valid_generator(list(1)).
test(composite_4):- valid_generator(list(any,[int])).
test(composite_5):- valid_generator(list(3,[int,float])).
test(composite_6):- valid_generator(list([int,float])).
test(composite_7):- valid_generator(atom(1,2)).
:- end_tests(valid_composite_generators).

:- begin_tests(valid_composite_generators_fail).
test(composite_0):- \+ valid_generator(int(0,0)).
test(composite_1):- \+ valid_generator(int(1,-1)).
test(composite_2):- \+ valid_generator(float(-1,0)).
test(composite_3):- \+ valid_generator(float(0,0)).
test(composite_4):- \+ valid_generator(list(any)).
test(composite_5):- \+ valid_generator(list(int,int)).
test(composite_6):- \+ valid_generator(list(3,int)).
test(composite_7):- \+ valid_generator(float(1,2)). % since 1 and 2 are not a float
test(composite_8):- \+ valid_generator(atom(2,1)).
test(composite_9):- \+ valid_generator(atom(1,-2)).
:- end_tests(valid_composite_generators_fail).

