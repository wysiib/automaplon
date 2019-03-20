
:- use_module(library(plunit)).
:- use_module(test_util).
:- use_module('../src/automaton.pl').
:- use_module('../src/state.pl').
:- use_module('../src/basic_operations.pl').

:- begin_tests(basic_operations_run).

test(run_for_singleton_automaton1) :-
    new_automaton(A),
    set_singleton(A, 'test'),
    run(A, 'test'),
    \+ run(A, 'reject-me'),
    \+ run(A, '').

test(run_for_singleton_automaton2) :-
    new_automaton(A),
    set_singleton(A, 'test with larger input than before and whitespaces :)'),
    run(A, 'test with larger input than before and whitespaces :)').

test(run_for_deterministic_automaton) :-
    new_automaton(A),
    set_deterministic(A, true),
    set_singleton(A, 'feh"1qädsf?nrouabe'),
    expand_singleton(A),
    get_number_of_states(A,N),
    N == 19,
    run(A, 'feh"1qädsf?nrouabe'),
    \+ run(A, ''),
    \+ run(A, ' '),
    \+ run(A, 'anything else'),
    get_initial(A, Initial),
    new_state(S1), new_state(S2),
    add_transition(Initial, [a,d]-S1),
    add_transition(S1, [a,z]-S2),
    run(A, 'feh"1qädsf?nrouabe').

test(run_for_non_deterministic_automaton) :-
    new_automaton(A),
    set_singleton(A, 'fe'),
    expand_singleton(A),
    get_number_of_states(A,N),
    N == 3,
    get_initial(A, Initial),
    set_deterministic(A, false),
    new_state(S1), new_state(S2),
    add_transition(Initial, [a,z]-S1),
    add_transition(S1, [a,z]-S2),
    run(A, 'fe'),
    \+ run(A, ''),
    \+ run(A, ' '),
    \+ run(A, 'anything else').

:- end_tests(basic_operations_run).
