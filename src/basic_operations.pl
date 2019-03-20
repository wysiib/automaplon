:- module(basic_operations, [run/2]).

:- use_module(automaton).
:- use_module(state).
:- use_module(util/bitset).

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
    atom_chars(Atom, ListOfAtoms),
    get_initial(Automaton, Initial),
    run_atom_codes(Initial, ListOfAtoms),!.
run(Automaton, Atom) :-
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
