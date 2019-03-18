
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
    equal_lists_as_set(ReachableStates, [Initial, S1, S2, S3]).

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
    equal_lists_as_set(ReachableStates, [Initial, S1, S2, S3, S4]).

test(get_accept_states1) :- 
    new_automaton(A) , 
    get_initial(A, Initial) , 
    new_state(S1) , new_state(S2) , new_state(S3) , 
    set_accept(S3, true) , 
    add_transition(Initial, [a,d]-S1) , 
    add_transition(Initial, [e,f]-S2) , 
    add_transition(S1, [g,h]-S3) , 
    get_accept_states(A, ReachableStates) , 
    ReachableStates == [S3].

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
    get_accept_states(A, ReachableStates) , 
    equal_lists_as_set(ReachableStates, [Initial, S2, S3]).

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
    get_accept_states(A, ReachableStates) , 
    ReachableStates == [] , 
    set_accept(S4, true) , 
    get_accept_states(A, ReachableStates2) , 
    ReachableStates2 == [S4].

:- end_tests(automaton_states).

% TODO: extend tests
