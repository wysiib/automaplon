:- module(automaton, [new_automaton/1,
                      get_initial/2, 
                      set_initial/2,
                      get_singleton/2,
                      set_singleton/2,
                      get_deterministic/2,
                      set_deterministic/2,
                      get_info/2,
                      set_info/2,
                      get_minimise_always/2,
                      set_minimise_always/2, 
                      get_states/2, 
                      get_accept_states/2, 
                      get_number_of_states/2]).

:- use_module(state).
:- use_module(util/maps).

% Variable Attributes:
%   initial - an initial State
%   deterministic - is true if the automaton is deterministic
%   singleton - a singleton string or null
%   minimise_always - always minimise automaton if true
%   info - associates extra information with this automaton as a list

%% new_automaton(-Automaton).
%
% Instantiates a fresh deterministic automaton with an initial state.
new_automaton(Automaton) :-
    new_state(State),
    set_initial(Automaton, State),
    set_minimise_always(Automaton, true),
    set_deterministic(Automaton, true),
    set_singleton(Automaton, null).

%% get_initial(+Automaton, -Initial).
%
% Returns the initial state of the automaton.
get_initial(Automaton, Initial) :-
    get_attr(Automaton, initial, Initial).

%% set_initial(+Automaton, +Initial).
%
% Set the initial state of the automaton.
set_initial(Automaton, Initial) :-
    put_attr(Automaton, initial, Initial).

%% get_deterministic(+Automaton, -Deterministic).
%
% Returns true if automaton is deterministic.
get_deterministic(Automaton, Deterministic) :-
    get_attr(Automaton, deterministic, Deterministic).

%% set_deterministic(+Automaton, +Deterministic).
%
% Set the automaton's deterministic attribute.
set_deterministic(Automaton, Deterministic) :-
    put_attr(Automaton, deterministic, Deterministic).

%% get_singleton(+Automaton, -Singleton).
%
% Returns an atom (singleton string) if the automaton is singleton, otherwise null.
get_singleton(Automaton, Singleton) :-
    get_attr(Automaton, singleton, Singleton).

%% set_singleton(+Automaton, +Singleton).
%
% Set the automaton's singleton attribute.
set_singleton(Automaton, Singleton) :-
    put_attr(Automaton, singleton, Singleton).

%% set_minimise_always(+Automaton, +MinimiseAlways).
%
% Set the automaton's minimise_always attribute.
set_minimise_always(Automaton, MinimiseAlways) :-
    put_attr(Automaton, minimise_always, MinimiseAlways).

%% get_minimise_always(+Automaton, -MinimiseAlways).
%
% Return boolean minimise_always attribute.
get_minimise_always(Automaton, MinimiseAlways) :-
    get_attr(Automaton, minimise_always, MinimiseAlways).

%% set_info(+Automaton, +Info).
%
% Set the automaton's info attribute.
set_info(Automaton, Info) :-
    put_attr(Automaton, info, Info).

%% get_info(+Automaton, -Info).
%
% Return info list attribute.
get_info(Automaton, Info) :-
    get_attr(Automaton, info, Info).

:- dynamic seen/1.
:- volatile seen/1.

%% get_states(+Automaton, -States).
%
% Get all states by exploring the initial state exhaustively.
get_states(Automaton, ReachableStates) :- 
  get_reachable_and_accept_states(Automaton, ReachableStates, _).

%% get_accept_states(+Automaton, -States).
%
% Get all accepting states by exploring the initial state exhaustively.
get_accept_states(Automaton, ReachableAcceptStates) :- 
  get_reachable_and_accept_states(Automaton, _, ReachableAcceptStates).

get_reachable_and_accept_states(Automaton, [Initial|ReachableStates], AcceptStates) :- 
  get_initial(Automaton, Initial) , 
  retractall(seen(_)) , 
  get_attr(Initial, id, StateId) , 
  get_reachable_and_accept_states(Initial, StateId, [], ReachableStates, [], TempAcceptStates) , 
  retractall(seen(_)) , 
  add_state_to_list_if_accepting(Initial, TempAcceptStates, AcceptStates).

get_reachable_and_accept_states(_, StateId, ReachableAcc, ReachableAcc, AcceptAcc, AcceptAcc) :- 
  seen(StateId) , 
  !.
get_reachable_and_accept_states(State, StateId, Acc, ReachableStates, AcceptAcc, AcceptStates) :- 
  assert(seen(StateId)) , 
  get_next_states(State, NextStates) , 
  map_get_reachable_and_accept_states(NextStates, Acc, ReachableStates, AcceptAcc, AcceptStates).

map_get_reachable_and_accept_states([], ReachableAcc, ReachableAcc, AcceptAcc, AcceptAcc).
map_get_reachable_and_accept_states([NextState|T], ReachableAcc, ReachableStates, AcceptAcc, AcceptStates) :- 
  get_attr(NextState, id, StateId) , 
  \+ seen(StateId) , 
  ! , 
  get_reachable_and_accept_states(NextState, StateId, ReachableAcc, TempReachableAcc, AcceptAcc, TempAcceptAcc) , 
  add_state_to_list_if_accepting(NextState, TempAcceptAcc, NewAcceptAcc) , 
  map_get_reachable_and_accept_states(T, [NextState|TempReachableAcc], ReachableStates, NewAcceptAcc, AcceptStates).
map_get_reachable_and_accept_states([_|T], ReachableAcc, ReachableStates, AcceptAcc, AcceptStates) :- 
  map_get_reachable_and_accept_states(T, ReachableAcc, ReachableStates, AcceptAcc, AcceptStates).

add_state_to_list_if_accepting(State, List, [State|List]) :- 
  is_accept(State) , 
  !.
add_state_to_list_if_accepting(_, List, List).

%% get_number_of_states(+Automaton, -States).
%
% Get the amount of states in the automaton.
get_number_of_states(Automaton, AmountOfStates) :- 
  get_states(Automaton, ReachableStates) , 
  length(ReachableStates, AmountOfStates).
