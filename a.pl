:- module(probat, [property_test/0, property_test/1]).

:- multifile valid_generator/1.
:- multifile random_element/2.
:- multifile random_element/4.
:- multifile generate_shrinking_alternatives/3.
:- multifile cartesian_product_2/3.

:- setting(trials, integer, 3, "Number of test").
:- setting(depth, integer, 8, "Max shrink depth.").
:- setting(maxLenList, integer, 8, "Max list length.").
:- setting(minVal, integer, -2147483648, "Min val to generate."). % -2**31
:- setting(maxVal, integer, 2147483648, "Max val to generate."). % 2**31


always_hold(A,B):-
    A > 0,
    B > 0.

append_ok(A,B):-
    append(A,B,C),
    length(A,NA),
    length(B,NB),
    length(C,NC),
    NC is NA + NB.

append_non_ok(A,B):-
    append(A,B,C),
    length(A,NA),
    length(B,NB),
    length(C,NC),
    NC is NA + NB - 1.

% somma(A,B,C):-
%     C is A + B.
% always_gt(Predicate,Arguments).
% property(always_gt(0),somma(int,int,*)). 
% per definire alcune proprietà: nel caso sopra, always_gt è
% predefinita. In questo caso, genera due int per somma, il terzo
% è l'argomento che deve essere testato dalla proprietà always gt.
% Si traduce in qualcosa del tipo:
% somma_gt(A,B,C):-
%     C is A + B,
%     C > 0.

% property(append_non_ok(list,list)).
property(always_hold(int,int)).
property(always_hold(float,int)).
property(append_ok(list,list)).

test_shrank(Predicate,[A0|TA0],[A1|TA1],F0,F1):-
    ToCall =.. [Predicate,A0,A1],
    ( \+ToCall -> 
        F0 = A0, F1 = A1 ;
        test_shrank(Predicate,TA0, TA1, F0, F1)
    ).

test_loop(I,_,_,L,L):- I =< 0, !.
test_loop(I,Predicate,Arguments,LF,FailuresList):-
    I > 0,
    I1 is I - 1,
    % generate random inputs
    maplist(random_element,Arguments,LRandomElements),
    ToCall =.. [Predicate|LRandomElements],
    ( ToCall -> 
        test_loop(I1,Predicate,Arguments,LF,FailuresList) ;
        maplist(generate_shrinking_alternatives,Arguments,LRandomElements,PossibleShrinks),
        cartesian_product_2(PossibleShrinks,L0,L1),
        ( test_shrank(Predicate,L0,L1,F0,F1) ->
            FailingInstance = [F0,F1] ;
            FailingInstance = LRandomElements
        ),
        test_loop(I1,Predicate,Arguments,[FailingInstance|LF],FailuresList)
    ).

run_test(Test,Result):-
    setting(trials,Trials),
    Test =.. [Predicate|Arguments], %
    ( maplist(valid_generator,Arguments) -> 
        true ;
        format("Some generators for ~w are not valid~n",[Test]),
        false
    ),
    format("Executing test: ~w~n", Test),
    test_loop(Trials,Predicate,Arguments,[],FailuresList),
    length(FailuresList,NFailures),
    % NSuccesses is Trials - NFailures,
    FailureRatio is NFailures / Trials,
    sort(FailuresList,FS),
    format("Run ~w tests, ~w failures (~w %)~n",[Trials,NFailures,FailureRatio]),
    ( NFailures > 0 ->
        format("Failures list ~w~n--- FAILED ---~n",[FS]),
        Result = -1 ;
        writeln("Passed"),
        Result = 1
    ).

pretty_print_arguments([Arg,Def]):-
    format("- ~w (default: ~w)~n",[Arg,Def]).
set_argument(seed(N)):-
    set_random(seed(N)).
set_argument(Arg):-
    Arg =.. [Argument,Value],
    member(Argument,[trial,depth,maxLenList]), 
    ( integer(Value), Value > 0 ->
        set_setting(Argument,Value) ;
        format("~w must be an integer > 0, found ~w~n",[Argument,Value]),
        fail
    ).
set_argument(Arg):-
    Arg =.. [Argument,Value],
    member(Argument,[minVal,maxVal]), 
    ( integer(Value), Value > 0 ->
        set_setting(Argument,Value) ;
        format("~w must be an integer > 0, found ~w~n",[Argument,Value]),
        fail
    ).
set_argument(A):-
    format("Argument ~w not found~n",[A]),
    findall([Arg,Default],setting(Arg,Default),LAS),
    writeln("Available arguments:"),
    maplist(pretty_print_arguments,LAS),
    fail.


property_test:-
    property_test_.
property_test(Arguments):-
    ( is_list(Arguments) -> 
        maplist(set_argument,Arguments), !,
        setting(minVal,MinV),
        setting(maxVal,MaxV),
        ( MinV >= MaxV -> 
            writeln("minVal must be less than maxVal"),
            fail ;
            true
        ),
        property_test_ ;
        writeln("Arguments must be passed inside a list"),
        false
    ).

property_test_:-
    consult("generators.pl"),
    consult("shrinkers.pl"),
    findall(Test,property(Test),LTests),
    length(LTests,NTests),
    format("Found ~w tests~n",[NTests]),
    % writeln(LTests),
    call_time(maplist(run_test,LTests,Results), Runtime),
    findall(-1,member(-1,Results),LFailed),
    length(LFailed,NFailed),
    length(Results,NTested),
    Ratio is NFailed / NTested,
    Runtime = time{cpu:_CT, inferences:_I, wall:W},
    writeln("--- Summary ---"),
    format("Executed ~w test in ~w seconds~n",[NTested,W]),
    format("Failed ~w over ~w (~w %)~n.",[NFailed,NTested,Ratio]).

