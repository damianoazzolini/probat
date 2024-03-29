:- module(probat, [property_test/0, property_test/1]).

:- ["generators.pl"].
:- ["shrinkers.pl"].

:- multifile valid_generator/1.
:- multifile random_element/2.
:- multifile random_element/4.
:- multifile generate_shrinking_alternatives/3.
:- dynamic property/1.

:- meta_predicate property(?).

:- setting(trials, integer, 100, "Number of test").
:- setting(depth, integer, 8, "Max shrink depth.").
:- setting(maxLenList, integer, 32, "Max list length.").
:- setting(verbosity, integer, 1, "Verbosity.").
:- setting(minVal, integer, -2147483648, "Min val to generate."). % -2**31
:- setting(maxVal, integer, 2147483648, "Max val to generate."). % 2**31

% test_shrank(Predicate,LToTest,FailingInstance)
% LToTest is a list of N lists each one with M elements, where N is
% the arity of the predicate Predicate. FailingInstance is an instance
% failing. TODO: is it possible to improve this and avoid replicating
% the signature?
setup_arguments(Predicate,[[A0|TA0]],ToCall,Remaining):-
    ToCall =.. [Predicate,A0],
    Remaining = [TA0].
setup_arguments(Predicate,[[A0|TA0],[A1|TA1]],ToCall,Remaining):-
    ToCall =.. [Predicate,A0,A1],
    Remaining = [TA0,TA1].
setup_arguments(Predicate,[[A0|TA0],[A1|TA1],[A2|TA2]],ToCall,Remaining):-
    ToCall =.. [Predicate,A0,A1,A2],
    Remaining = [TA0,TA1,TA2].
setup_arguments(Predicate,[[A0|TA0],[A1|TA1],[A2|TA2],[A3|TA3]],ToCall,Remaining):-
    ToCall =.. [Predicate,A0,A1,A2,A3],
    Remaining = [TA0,TA1,TA2,TA3].
setup_arguments(Predicate,[[A0|TA0],[A1|TA1],[A2|TA2],[A3|TA3],[A4|TA4]],ToCall,Remaining):-
    ToCall =.. [Predicate,A0,A1,A2,A3,A4],
    Remaining = [TA0,TA1,TA2,TA3,TA4].

test_shrank(Predicate,LArgs,F0):-
    setup_arguments(Predicate,LArgs,ToCall,Remaining),
    setting(verbosity,V),
    ( V > 1 -> 
        format("Testing ~w~n",[ToCall]) ;
        true
    ),
    ( \+ catch(ToCall, _Error, false) -> 
        F0 = ToCall ;
        test_shrank(Predicate,Remaining,F0)
    ).

test_loop(I,_,_,L,L):- I =< 0, !.
test_loop(I,Predicate,Arguments,LF,FailuresList):-
    I > 0,
    I1 is I - 1,
    % generate random inputs
    maplist(random_element,Arguments,LRandomElements), !,
    ToCall =.. [Predicate|LRandomElements],
    ( catch(ToCall, _Error, false) ->
        test_loop(I1,Predicate,Arguments,LF,FailuresList) ;
        maplist(generate_shrinking_alternatives,Arguments,LRandomElements,PossibleShrinks),
        setting(verbosity,V),
        ( V > 1 ->
            format("Calling with ~w~n",[PossibleShrinks]) ; 
            true
        ),
        ( test_shrank(Predicate,PossibleShrinks,F) ->
            FailingInstance = F ;
            FailingInstance = LRandomElements % the shrink operation is not successful
        ),
        test_loop(I1,Predicate,Arguments,[FailingInstance|LF],FailuresList)
    ).

run_test(Test,Result):-
    setting(trials,Trials),
    Test =.. [Predicate|Arguments],
    ( maplist(valid_generator,Arguments) -> 
        true ;
        format("Some generators for ~w are not valid~n",[Test]),
        false
    ),
    format("Executing test: ~w ..... ", Test),
    test_loop(Trials,Predicate,Arguments,[],FailuresList),
    length(FailuresList,NFailures),
    FailureRatio is roundtoward((NFailures / Trials)*100, to_nearest),
    sort(FailuresList,FS),
    % return the smallest one
    format("Run ~w attempts, ~w failures (~w %) ..... ",[Trials,NFailures,FailureRatio]),
    ( NFailures > 0 ->
        FS = [Smallest|_],
        reverse(FS,FSRev),
        FSRev = [Greatest|_],
        ansi_format([fg(red)],"FAILED~nSmallest failing ~w~nGreatest failing ~w~n",[Smallest,Greatest]),
        Result = -1 ;
        ansi_format([fg(green)],"Passed~n",[]),
        Result = 1
    ).

pretty_print_arguments([Arg,Def]):-
    format("- ~w (default: ~w)~n",[Arg,Def]).
set_argument(seed(N)):-
    set_random(seed(N)).
set_argument(Arg):-
    Arg =.. [Argument,Value],
    member(Argument,[trial,depth,maxLenList,verbosity]), 
    ( integer(Value), Value > 0 ->
        set_setting(Argument,Value) ;
        format("~w must be an integer > 0, found ~w~n",[Argument,Value]),
        fail
    ).
set_argument(Arg):-
    Arg =.. [Argument,Value],
    member(Argument,[minVal,maxVal]), 
    ( number(Value) ->
        set_setting(Argument,Value) ;
        format("~w must be a number, found ~w~n",[Argument,Value]),
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

% check_existence(Predicate,DoesExist): check whether the current predicate
% exist. DoesExist = 1 if the predicate exists, 0 otherwise.
check_existence(Predicate,DoesExist):-
    functor(Predicate, Name, Arity),
    ( current_predicate(user:Name/Arity) ->
        DoesExist = 1 ;
        DoesExist = 0
    ),
    ( Arity > 5 -> 
        format("Currently predicates with at most 5 arguments are supported, found ~w in ~w~n",[Arity,Predicate]),
        false ;
        true
    ).

write_predicates_not_found(Predicate,0):-
    format("Predicate ~w not defined~n", [Predicate]).
write_predicates_not_found(_,1).

property_test_:-
    catch(
        user:property(_),
        error(existence_error(_,_),_),
        (writeln("Please specify at least one property to test"), false)
    ), !,
    findall(Test,user:property(Test),LTests),
    % checks that the predicate exist
    maplist(check_existence,LTests,ExistOrNot),
    ( member(0,ExistOrNot) ->
        maplist(write_predicates_not_found,LTests,ExistOrNot),
        false ;
        true
    ),
    length(LTests,NTests),
    ( ( NTests = 0; NTests = 1 ) ->
        format("Found ~w test.~n",[NTests]);
        format("Found ~w tests.~n",[NTests])
    ),
    % writeln(LTests),
    ( NTests > 0 ->
        call_time(maplist(run_test,LTests,Results), Runtime),
        findall(-1,member(-1,Results),LFailed),
        length(LFailed,NFailed),
        length(Results,NTested),
        Ratio is roundtoward((NFailed / NTested)*100,to_nearest),
        Runtime = time{cpu:_CT, inferences:_I, wall:W},
        writeln("--- Summary ---"),
        format("Executed ~w test in ~w seconds~n",[NTested,W]),
        format("Failed ~w over ~w (~w %)~n.",[NFailed,NTested,Ratio]) ;
        true
    ).

