:- module(basic_operations, [run/2]).

:- use_module(util/bitset).

%% run(+Automaton, +Atom).
%
% True if the given atom (or SWI-Prolog String) is accepted by the automaton.
% Complexity: linear in the length of the atom.
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
    run_atom_codes(Initial, ListOfAtoms).
run(Automaton, Atom) :-
    get_states(Automaton, States),
    enumerate_states_coherently(1, States) ,
    length(States, NrOfStates) ,
    get_initial(Automaton, Initial),
    PP = [Initial],
    new_bitset(NrOfStates, BB),
    new_bitset(NrOfStates, BB_other),
    get_accept(Initial, InitialAccept),
    atom_chars(Atom, ListOfAtoms),
    run_non_det(ListOfAtoms, PP, BB, BB_other, InitialAccept).

enumerate_states_coherently(_, []).
enumerate_states_coherently(C, [State|T]) :-
    set_number(State, C) ,
    C1 is C + 1 ,
    enumerate_states_coherently(C1, T).

run_non_det([], _, _, _, _, Accept) :-
    Accept == true,
    !.
run_non_det([CharAtom|T], PP, BB, BB_other, _) :-
    copy_term(BB, BBOld) ,
    run_non_det_for_char(CharAtom, PP, BB, D-D, BB_other, NewPP, false, Accept),
    run_non_det(T, NewPP, PP, BB_other, BBOld, Accept).

run_non_det_for_char(_, [], _, PP_other-D, _, PP_other-D, Accept, Accept).
run_non_det_for_char(CharAtom, [PP|T], BB, PP_other, BB_other, NewPP_other, CurrentAccept, Accept) :-
    step(PP, CharAtom, Destinations),
    run_non_det_for_char_and_destinations(Destinations, BB, PP_other, BB_other, TempNewPP_other, CurrentAccept, NewCurrentAccept),
    run_non_det_for_char(CharAtom, T, BB, TempNewPP_other, BB_other, NewPP_other, NewCurrentAccept, Accept).

run_non_det_for_char_and_destinations([], _, PP_other-D, _, PP_other-D, Accept, Accept).
run_non_det_for_char_and_destinations([Destination|T], BB, PP_other-D, BB_other, NewPP_other, CurrentAccept, Accept) :-
    (is_accept(Destination)
    -> NCurrentAccept = true
    ;  NCurrentAccept = CurrentAccept),
    get_number(Destination, DestinationNumber),
    (\+ bit_is_set(BB_other, DestinationNumber)
    -> set_bit(BB_other, DestinationNumber),
       D = [Destination|ND]
    ;  D = ND),
    run_non_det_for_char_and_destinations(T, BB, PP_other-ND, BB_other, NewPP_other, NCurrentAccept, Accept).

run_atom_codes(State, []) :-
    is_accept(State).
run_atom_codes(State, [CharAtom|T]) :-
    step(State, CharAtom, NextStates),
    member(NextState, NextStates),
    run_atom_codes(NextState, T).
