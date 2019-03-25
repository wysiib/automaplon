:- module(basic_operations, [concatenate/3,
                             run/2]).

:- use_module(automaton).
:- use_module(state).

%% concatenate(+Automaton1, +Automaton2, -Concat).
%
% Returns an automaton that accepts the concatenation of the languages of
% the given automata.
concatenate(Automaton1, Automaton2, Concat) :-
    get_singleton(Automaton1, Singleton1),
    get_singleton(Automaton2, Singleton2),
    !,
    new_automaton(Concat),
    atom_concat(Singleton1, Singleton2, SingletonC),
    set_singleton(Concat, SingletonC).
concatenate(Automaton1, Automaton2, Concat) :-
    is_empty_automaton(Automaton1),
    is_empty_automaton(Automaton2),
    !,
    new_automaton(Concat).
concatenate(Automaton1, Automaton2, CAutomaton1) :-
    (is_singleton(Automaton1),
     is_deterministic(Automaton2)
    ->  Deterministic = true
    ;   Deterministic = false),
    clone_expanded(Automaton1, CAutomaton1),
    clone_expanded(Automaton2, CAutomaton2),
    set_deterministic(CAutomaton1, Deterministic),
    get_accept_states(CAutomaton1, Accept1),
    get_initial(CAutomaton2, Initial2),
    concatenate_add_epsilons(Accept1, CAutomaton1, Initial2).
    %minimize(CAutomaton1).

concatenate_add_epsilons([], _, _).
concatenate_add_epsilons([AcceptState|T], CAutomaton1, Initial2) :-
    set_accept(AcceptState, false),
    add_epsilon(AcceptState, Initial2),
    concatenate_add_epsilons(T, CAutomaton1, Initial2).

is_empty_automaton(Automaton) :-
    get_singleton(Automaton, Singleton),
    Singleton == '',
    !.
is_empty_automaton(Automaton) :-
    get_initial(Automaton, Initial),
    is_accept(Initial),
    get_transitions(Initial, Transitions),
    Transitions == transitions{}.

%% run(+Automaton, +Atom).
%
% True if the given atom (or SWI-Prolog String) is accepted by the automaton.
run(Automaton, Atom) :-
    is_singleton(Automaton),
    !,
    get_singleton(Automaton, Singleton),
    Singleton == Atom.
run(Automaton, Atom) :-
    is_deterministic(Automaton),
    !,
    atom(Atom),
    atom_chars(Atom, ListOfAtoms),
    get_initial(Automaton, Initial),
    run_atom_codes(Initial, ListOfAtoms),!.
run(Automaton, Atom) :-
    atom(Atom),
    get_initial(Automaton, Initial),
    States = [Initial],
    get_accept(Initial, InitialAccept),
    atom_chars(Atom, ListOfAtoms),
    retractall(seen(_)),
    run_non_det(ListOfAtoms, States, InitialAccept),!.

:- dynamic seen/1.
:- volatile seen/1.

run_non_det([], _, Accept) :-
    Accept == true,
    !.
run_non_det([CharAtom|T], States, _) :-
    run_non_det_for_char(CharAtom, States, D-D, NewStates-D2, false, Accept),
    D2 = [],
    run_non_det(T, NewStates, Accept).

run_non_det_for_char(_, [], TempStates-D, TempStates-D, Accept, Accept).
run_non_det_for_char(CharAtom, [States|T], TempStates-D, NewTempStates, CurrentAccept, Accept) :-
    step(States, CharAtom, Destinations),
    run_non_det_for_char_destinations(Destinations, TempStates-D, TempTempStates, CurrentAccept, NewCurrentAccept),
    run_non_det_for_char(CharAtom, T, TempTempStates, NewTempStates, NewCurrentAccept, Accept).

run_non_det_for_char_destinations([], TempStates-D, TempStates-D, Accept, Accept).
run_non_det_for_char_destinations([Destination|T], TempStates-D, NewTempStates, CurrentAccept, Accept) :-
    (is_accept(Destination)
    -> NCurrentAccept = true
    ;  NCurrentAccept = CurrentAccept),
    get_id(Destination, DestinationId),
    (seen(DestinationId)
    -> D = ND
    ;  assert(seen(DestinationId)),
       D = [Destination|ND]),
    run_non_det_for_char_destinations(T, TempStates-ND, NewTempStates, NCurrentAccept, Accept).

run_atom_codes(State, []) :-
    is_accept(State).
run_atom_codes(State, [CharAtom|T]) :-
    step(State, CharAtom, NextStates),
    member(NextState, NextStates),
    run_atom_codes(NextState, T).
