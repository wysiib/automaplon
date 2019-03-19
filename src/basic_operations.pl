:- module(basic_operations, [run/2]).

%% run(+Automaton, +Atom).
%
% True if the given atom is accepted by the automaton.
% Complexity: linear in the length of the atom.
run(Automaton, Atom) :- 
    is_singleton(Automaton) , 
    ! , 
    get_singleton(Automaton, Singleton) , 
    Singleton == Atom.
run(Automaton, Atom) :- 
    is_deterministic(Automaton) , 
    ! , 
    atom_codes(Atom, Codes) , 
    get_initial(Automaton, Initial) , 
    run_atom_codes(Initial, Codes).
run(Automaton, Atom) :- 
    % TODO: case for non-deterministic 
    true.

run_atom_codes(State, []) :- 
    is_accept(State).
run_atom_codes(State, [Code|T]) :- 
    atom_codes(Char, [Code]) , 
    step(State, Char, NextStates) , 
    member(NextState, NextStates) , 
    run_atom_codes(NextState, T).
