
:- use_module(library(plunit)).
:- use_module(test_util).
:- use_module('../src/automaton.pl').
:- use_module('../src/state.pl').

:- begin_tests(automaton_basic).

test(construction) :-
    new_automaton(Automaton) , 
    get_initial(Automaton, Initial) , 
    get_deterministic(Automaton, Deterministic) , 
    get_transitions(Initial, InitialTransitions) , 
    InitialTransitions == transitions{} , 
    get_transitions(Initial, a, TransitionsA) , 
    TransitionsA == [] , 
    get_transitions(Initial, j, TransitionsJ) , 
    TransitionsJ == [] , 
    Deterministic == true , 
    get_singleton(Automaton, Singleton) , 
    Singleton == null.

test(construction_and_extension) :-
    new_automaton(Automaton) , 
    get_initial(Automaton, Initial) , 
    new_state(S1) , 
    new_state(S2) , 
    add_transition(Initial, [a,d]-S1) , 
    add_transition(S1, [e,f]-S2) , 
    get_transitions(Initial, InitialTransitions) , 
    InitialTransitions \== transitions{} , 
    get_transitions(Initial,a,TransitionsA) , 
    TransitionsA == [S1] , 
    get_transitions(Initial,c,TransitionsC) , 
    TransitionsC == [S1] , 
    get_transitions(S1,e,TransitionsE) , 
    TransitionsE == [S2] , 
    get_transitions(S1,f,TransitionsF) , 
    TransitionsF == [S2].

test(construction_and_extension_nondet) :-
    new_automaton(Automaton) , 
    get_initial(Automaton, Initial) , 
    set_deterministic(Automaton, false) , 
    get_deterministic(Automaton, Deterministic) , 
    Deterministic == false , 
    new_state(S1) , 
    new_state(S2) , 
    add_transition(Initial, [a,d]-S1) , 
    add_transition(S1, [e,f]-S2) , 
    add_transition(Initial, [a,d]-S2) , 
    get_transitions(Initial, InitialTransitions) , 
    InitialTransitions \== transitions{} , 
    get_transitions(Initial,a,TransitionsA) , 
    equal_lists_as_set(TransitionsA,[S1,S2]) , 
    get_transitions(Initial,b,TransitionsB) , 
    equal_lists_as_set(TransitionsB,[S1,S2]) , 
    get_transitions(Initial,c,TransitionsC) , 
    equal_lists_as_set(TransitionsC,[S1,S2]) , 
    get_transitions(Initial,d,TransitionsD) , 
    equal_lists_as_set(TransitionsD,[S1,S2]) , 
    get_transitions(S1,e,TransitionsE) , 
    TransitionsE == [S2] , 
    get_transitions(S1,f,TransitionsF) , 
    TransitionsF == [S2].

:- end_tests(automaton_basic).

:- begin_tests(automaton_states).

test(get_states1) :- 
    new_automaton(A) , 
    get_initial(A, Initial) , 
    new_state(S1) , new_state(S2) , new_state(S3) , 
    add_transition(Initial, [a,d]-S1) , 
    add_transition(Initial, [e,f]-S2) , 
    add_transition(S1, [g,h]-S3) , 
    get_states(A, ReachableStates) , 
    equal_lists_as_set(ReachableStates, [Initial, S1, S2, S3]) , 
    get_number_of_states(A, AmountOfStates) , 
    AmountOfStates == 4.

test(get_states2) :- 
    new_automaton(A) , 
    set_deterministic(A, false) , 
    get_initial(A, Initial) , 
    new_state(S1) , new_state(S2) , new_state(S3) , new_state(S4) , 
    add_transition(Initial, [a,d]-S1) , 
    add_transition(Initial, [e,f]-S2) , 
    add_transition(S1, [g,h]-S3) , 
    add_transition(S3, [a,a]-S1) , 
    add_transition(S3, [a,a]-S2) , 
    add_transition(S3, [a,a]-S3) , 
    add_transition(S3, [a,a]-S4) , 
    get_states(A, ReachableStates) , 
    equal_lists_as_set(ReachableStates, [Initial, S1, S2, S3, S4]) , 
    get_number_of_states(A, AmountOfStates) , 
    AmountOfStates == 5.

test(get_accept_states1) :- 
    new_automaton(A) , 
    get_initial(A, Initial) , 
    new_state(S1) , new_state(S2) , new_state(S3) , 
    set_accept(S3, true) , 
    add_transition(Initial, [a,d]-S1) , 
    add_transition(Initial, [e,f]-S2) , 
    add_transition(S1, [g,h]-S3) , 
    get_accept_states(A, ReachableAcceptStates) , 
    ReachableAcceptStates == [S3] , 
    get_number_of_states(A, AmountOfStates) , 
    AmountOfStates == 4.

test(get_accept_states2) :- 
    new_automaton(A) , 
    set_deterministic(A, false) , 
    get_initial(A, Initial) , 
    new_state(S1) , new_state(S2) , new_state(S3) , new_state(S4) , 
    set_accept(Initial, true) , 
    set_accept(S2, true) , 
    set_accept(S3, true) , 
    add_transition(Initial, [a,d]-S1) , 
    add_transition(Initial, [e,f]-S2) , 
    add_transition(S1, [g,h]-S3) , 
    add_transition(S3, [a,a]-S1) , 
    add_transition(S3, [a,a]-S2) , 
    add_transition(S3, [a,a]-S3) , 
    add_transition(S3, [a,a]-S4) , 
    get_accept_states(A, ReachableAcceptStates) , 
    equal_lists_as_set(ReachableAcceptStates, [Initial, S2, S3]) , 
    get_number_of_states(A, AmountOfStates) , 
    AmountOfStates == 5.

test(get_accept_states3) :- 
    new_automaton(A) , 
    set_deterministic(A, false) , 
    get_initial(A, Initial) , 
    new_state(S1) , new_state(S2) , new_state(S3) , new_state(S4) , 
    add_transition(Initial, [a,d]-S1) , 
    add_transition(Initial, [e,f]-S2) , 
    add_transition(S1, [g,h]-S3) , 
    add_transition(S3, [a,a]-S1) , 
    add_transition(S3, [a,a]-S2) , 
    add_transition(S3, [a,a]-S3) , 
    add_transition(S3, [a,a]-S4) , 
    get_accept_states(A, ReachableAcceptStates) , 
    ReachableAcceptStates == [] , 
    get_number_of_states(A, AmountOfStates) , 
    AmountOfStates == 5 , 
    set_accept(S4, true) , 
    get_accept_states(A, ReachableAcceptStates2) , 
    ReachableAcceptStates2 == [S4] , 
    get_number_of_states(A, AmountOfStates2) , 
    AmountOfStates2 == 5.

test(get_accept_states4) :- 
    new_automaton(A) , 
    get_initial(A, Initial) , 
    add_transition(Initial, [a,z]-Initial) , 
    get_accept_states(A, ReachableAcceptStates) , 
    ReachableAcceptStates == [] , 
    get_number_of_states(A, AmountOfStates) , 
    AmountOfStates == 1 , 
    set_accept(Initial, true) , 
    get_accept_states(A, ReachableAcceptStates2) , 
    ReachableAcceptStates2 == [Initial] , 
    get_number_of_states(A, AmountOfStates2) , 
    AmountOfStates2 == 1.

test(number_of_states) :- 
    new_automaton(A) , 
    set_deterministic(A, false) , 
    get_number_of_states(A, AmountOfStates) , 
    AmountOfStates == 1 , 
    new_state(S1) , new_state(S2) , 
    get_initial(A, Initial) , 
    add_transition(Initial, [a,b]-Initial) , 
    get_number_of_states(A, AmountOfStates2) , 
    AmountOfStates2 == 1 , 
    add_transition(Initial, [a,b]-S1) , 
    get_number_of_states(A, AmountOfStates3) , 
    AmountOfStates3 == 2 , 
    add_transition(S1, [a,b]-S2) , 
    get_number_of_states(A, AmountOfStates4) , 
    AmountOfStates4 == 3 , 
    add_transition(S2, [a,b]-Initial) , 
    get_number_of_states(A, AmountOfStates5) , 
    AmountOfStates5 == 3 , 
    reset_transitions(Initial) , 
    get_number_of_states(A, AmountOfStates6) , 
    AmountOfStates6 == 1.

test(live_states1) :- 
    new_automaton(A) , 
    new_state(S1) , 
    get_initial(A, Initial) , 
    add_transition(Initial, [a,d]-S1) , 
    new_state(S2) , 
    add_transition(S1, [a,a]-S2) , 
    set_accept(S1, true) , 
    get_live_states(A, LiveStates) , 
    equal_lists_as_set(LiveStates, [Initial, S1]).

test(live_states2) :- 
    new_automaton(A) , 
    new_state(S1) , 
    get_initial(A, Initial) , 
    add_transition(Initial, [a,d]-S1) , 
    new_state(S2) , 
    add_transition(S1, [a,a]-S2) , 
    add_transition(S2, [b,b]-S1) , 
    set_accept(S1, true) , 
    get_live_states(A, LiveStates) , 
    equal_lists_as_set(LiveStates, [Initial, S1, S2]).

test(live_states3) :- 
    new_automaton(A) , 
    new_state(S1) , 
    new_state(S2) , 
    new_state(S3) , 
    new_state(S4) , 
    get_initial(A, Initial) , 
    add_transition(Initial, [a,d]-S1) , 
    add_transition(S1, [a,a]-S2) , 
    add_transition(S2, [b,b]-S3) , 
    add_transition(S3, [a,z]-S4) , 
    set_accept(S4, true) , 
    get_live_states(A, LiveStates) , 
    equal_lists_as_set(LiveStates, [Initial, S1, S2, S3, S4]).

test(live_states4) :- 
    new_automaton(A) , 
    new_state(S1) , 
    new_state(S2) , 
    new_state(S3) , 
    new_state(S4) , 
    get_initial(A, Initial) , 
    add_transition(Initial, [a,d]-S1) , 
    add_transition(S1, [a,a]-S2) , 
    add_transition(S2, [k,m]-S3) , 
    add_transition(S3, [a,z]-S4) , 
    set_accept(Initial, true) , 
    get_live_states(A, LiveStates) , 
    LiveStates == [Initial].

test(live_states5) :- 
    new_automaton(A) , 
    new_state(S1) , 
    new_state(S2) , 
    get_initial(A, Initial) , 
    add_transition(Initial, [a,d]-S1) , 
    add_transition(S1, [c,d]-S2) , 
    get_live_states(A, LiveStates) , 
    LiveStates == [].

:- end_tests(automaton_states).

:- begin_tests(automaton_clone).

test(clone1) :- 
    new_automaton(A) , 
    clone(A, B) , 
    clone(A, C) , 
    get_initial(A, InitialA) , 
    get_initial(B, InitialB) , 
    get_initial(C, InitialC) , 
    \+ equals(InitialA, InitialB) , 
    \+ equals(InitialA, InitialC) , 
    \+ equals(InitialB, InitialC) , 
    get_deterministic(A, DetA) , 
    get_deterministic(B, DetB) , 
    get_deterministic(C, DetC) , 
    DetA == DetB , DetA == DetC , DetB == DetC , 
    get_minimise_always(A, MinA) , 
    get_minimise_always(B, MinB) , 
    get_minimise_always(C, MinC) , 
    MinA == MinB , MinA == MinC , MinB == MinC , 
    get_singleton(A, SinA) , 
    get_singleton(B, SinB) , 
    get_singleton(C, SinC) , 
    SinA == SinB , SinA == SinC , SinB == SinC , 
    set_deterministic(A, false) , 
    get_deterministic(A, DetA2) , 
    get_deterministic(B, DetB2) , 
    get_deterministic(C, DetC2) , 
    DetA2 == false , DetB2 == true, DetC2 == true , 
    set_minimise_always(B, false) , 
    set_minimise_always(C, false) , 
    get_minimise_always(A, MinA2) , 
    get_minimise_always(B, MinB2) , 
    get_minimise_always(C, MinC2) , 
    MinA2 == true , MinB2 == false, MinC2 == false.

test(clone2) :- 
    new_automaton(A) , 
    set_deterministic(A, false) , 
    get_initial(A, Initial) , 
    new_state(S1) , new_state(S2) , new_state(S3) , new_state(S4) , 
    add_transition(Initial, [a,f]-S1) , 
    add_transition(Initial, [g,j]-S2) , 
    add_transition(Initial, [a,b]-S3) , 
    add_transition(S1, [a,d]-S2) , 
    add_transition(S1, [e,f]-S3) , 
    add_transition(S2, [a,z]-S3) , 
    add_transition(S2, [a,h]-S4) , 
    add_transition(S2, [a,h]-S4) , 
    clone(A, C) , 
    % different attributed state variables 
    get_initial(A, InitialA) , 
    get_initial(C, InitialC) , 
    \+ equals(InitialA, InitialC) , 
    get_states(A, StatesA) , 
    get_states(C, StatesC) , 
    \+ equal_lists_as_set(StatesA, StatesC) , 
    get_deterministic(C, CDet) , CDet == false , 
    get_minimise_always(C, CMin) , CMin == true , 
    get_singleton(C, CSin) , CSin == null , 
    % adding transitions to cloned automata should not change original
    new_state(C1) , new_state(C2) , 
    add_transition(InitialC, [a,z]-C1) , 
    add_transition(InitialC, [f,p]-C2) , 
    add_transition(C1, [b,k]-C2) , 
    get_states(A, StatesA2) , 
    equal_lists_as_set(StatesA, StatesA2) , 
    get_states(C, StatesC2) , 
    \+ equal_lists_as_set(StatesC, StatesC2).

% TODO: more tests

:- end_tests(automaton_clone).
