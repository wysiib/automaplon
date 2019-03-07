:- module(automaton, [new_automaton/1]).

:- use_module(state).

% class and instance variables
% getters and setters map to variable annotations
get_initial(Automaton, Initial) :-
    get_attr(Automaton, Initial).
set_initial(Automaton, Initial) :-
    set_attr(Automaton, Initial).

get_deterministic(Automaton, Deterministic) :-
    get_attr(Automaton, Deterministic).
set_deterministic(Automaton, Deterministic) :-
    set_attr(Automaton, Deterministic).

get_singleton(Automaton, Singleton) :-
    get_attr(Automaton, Singleton).
set_singleton(Automaton, Singleton) :-
    set_attr(Automaton, Singleton).

new_automaton(Automaton) :-
    new_state(State),
    set_initial(Automaton, State),
    set_deterministic(Automaton,true),
    set_singleton(Automaton,null).
