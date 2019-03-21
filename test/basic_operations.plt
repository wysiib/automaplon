
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

test(run_for_non_deterministic_automaton1) :-
    new_automaton(A),
    set_singleton(A, 'fe'),
    expand_singleton(A),
    \+ is_singleton(A),
    get_number_of_states(A,N),
    N == 3,
    get_initial(A, Initial),
    set_deterministic(A, false),
    \+ is_deterministic(A),
    new_state(S1), new_state(S2),
    add_transition(Initial, [a,z]-S1),
    add_transition(S1, [a, z]-S2),
    run(A, 'fe'),
    \+ run(A, ''),
    \+ run(A, ' '),
    \+ run(A, 'anything else').

test(run_for_non_deterministic_automaton2) :-
    new_automaton(A),
    set_singleton(A, 'test'),
    expand_singleton(A),
    \+ is_singleton(A),
    set_deterministic(A,false),
    \+ is_deterministic(A),
    get_initial(A,I),
    new_state(S),
    new_state(S2),
    new_state(S3),
    new_state(S4),
    new_state(S5),
    new_state(S6),
    new_state(S7),
    add_transition(I, [' ',t]-S),
    add_transition(S, [e, t]-S2),
    add_transition(I, [c, l]-S2),
    add_transition(I, [d, p]-S5),
    add_transition(S5, [' ',z]-I),
    add_transition(S2, [s, y]-S3),
    add_transition(S3, [' ', z]-S4),
    add_transition(S4, [2, 6]-S5),
    add_transition(S5, [a, z]-S6),
    add_transition(S6, [a, z]-S7),
    set_accept(S2, true),
    set_accept(S4, true),
    set_accept(S6, true),
    \+ is_deterministic(A),
    run(A,'test').

test(run_for_non_deterministic_automaton3) :-
    new_automaton(A),
    set_singleton(A, '5098743763595415826950987437635954158269509874376359541582695098743763595415826950987437635954158269509874376359541582695098743763595415826950987437635954158269509874376359541582695098743763595415826950987437635954158269509874376359541582695098743763595415826950987437635954158269'),
    expand_singleton(A),
    \+ is_singleton(A),
    set_deterministic(A,false),
    \+ is_deterministic(A),
    get_initial(A,I),
    new_state(S),
    new_state(S2),
    new_state(S3),
    new_state(S4),
    new_state(S5),
    new_state(S6),
    new_state(S7),
    new_state(S8),
    new_state(S9),
    new_state(S10),
    new_state(S11),
    add_transition(I, [' ', z]-S),
    add_transition(S, [' ', z]-S2),
    add_transition(I, [' ', z]-S2),
    add_transition(I, [' ', z]-S5),
    add_transition(S5, [' ', z]-I),
    add_transition(S2, [' ', z]-S3),
    add_transition(S3, [' ', z]-S4),
    add_transition(S4, [' ', z]-S5),
    add_transition(S5, [' ', z]-S6),
    add_transition(S6, [' ', z]-S7),
    add_transition(S9, [' ', z]-S8),
    add_transition(S9, [' ', z]-S8),
    add_transition(S10, [' ', z]-S11),
    add_transition(S9, [' ', z]-S11),
    add_transition(S8, [' ', z]-S10),
    add_transition(S9, [' ', z]-S6),
    add_transition(S8, [' ', z]-S4),
    add_transition(S5, [' ', z]-S2),
    add_transition(S4, [' ', z]-S3),
    set_accept(S3, true),
    set_accept(S6, true),
    set_accept(S11, true),
    \+ is_deterministic(A),
    run(A,'5098743763595415826950987437635954158269509874376359541582695098743763595415826950987437635954158269509874376359541582695098743763595415826950987437635954158269509874376359541582695098743763595415826950987437635954158269509874376359541582695098743763595415826950987437635954158269').

test(accept_multiple_words) :-
    new_automaton(A),
    get_initial(A, S0),
    new_state(S1),
    new_state(S2),
    new_state(S3),
    add_transition(S0, a, S1),
    add_transition(S0, a, S2),
    add_transition(S0, b, S2),
    add_transition(S1, a, S3),
    add_transition(S2, b, S3),
    set_accept(S2, true),
    set_accept(S3, true),
    assertion(run(A, a)),
    assertion(run(A, b)),
    assertion(run(A, aa)),
    assertion(run(A, ab)),
    assertion(run(A, bb)).

test(enumerate_words, [fixme("Should enumeration be possible?")]) :-
    new_automaton(A),
    get_initial(A, S0),
    new_state(S1),
    new_state(S2),
    new_state(S3),
    add_transition(S0, a, S1),
    add_transition(S0, a, S2),
    add_transition(S0, b, S2),
    add_transition(S1, a, S3),
    add_transition(S2, b, S3),
    set_accept(S2, true),
    set_accept(S3, true),
    findall(X, run(A, X), Words),
    assertion(length(Words, 5)).



:- end_tests(basic_operations_run).
