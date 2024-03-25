:- module(test_shrinker, [test_shrinker/0]).
:- use_module(library(plunit)).
% :- use_module(library(prolog_coverage)).

:- ['../prolog/shrinkers'].

test_shrinker:-
    run_tests.

:- begin_tests(get_length).
test(basic_0):- get_length(1,NA), NA = 1.
test(basic_1):- get_length(-1,NA), NA = 1.
test(basic_2):- get_length(33,NA), NA = 2.
test(basic_3):- get_length([33],NA), NA = 1.
test(basic_4):- get_length([33,1],NA), NA = 2.
test(basic_5):- get_length([[33,1]],NA), NA = 2.
test(basic_6):- get_length([],NA), NA = 0.
:- end_tests(get_length).