
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

% TODO: extend tests
