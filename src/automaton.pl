:- module(automaton, [new_automaton/1,
                      remove_dead_transitions/1,
                      clone/2,
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
                      get_number_of_states/2,
                      get_number_of_transitions/2,
                      get_live_states/2]).

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

%% is_singleton(+Automaton).
%
% True if the automaton is singleton, i.e., the attribute singleton is not null.
is_singleton(Automaton) :-
    get_attr(Automaton, singleton, Singleton) , 
    Singleton \== null.

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
  get_id(Initial, StateId) , 
  get_reachable_and_accept_states(Initial, StateId, [], ReachableStates, [], TempAcceptStates) , 
  retractall(seen(_)) , 
  add_state_to_list_if_accepting(Initial, TempAcceptStates, AcceptStates).

get_reachable_and_accept_states(_, StateId, ReachableAcc, ReachableAcc, AcceptAcc, NewAcc) :- 
  seen(StateId) , 
  ! , 
  NewAcc = AcceptAcc.
get_reachable_and_accept_states(State, StateId, Acc, ReachableStates, AcceptAcc, AcceptStates) :- 
  assert(seen(StateId)) , 
  get_next_states(State, NextStates) , 
  map_get_reachable_and_accept_states(NextStates, Acc, ReachableStates, AcceptAcc, AcceptStates).

map_get_reachable_and_accept_states([], ReachableAcc, ReachableAcc, AcceptAcc, AcceptAcc).
map_get_reachable_and_accept_states([NextState|T], ReachableAcc, ReachableStates, AcceptAcc, AcceptStates) :- 
  get_id(NextState, StateId) , 
  \+ seen(StateId) , 
  ! , 
  get_reachable_and_accept_states(NextState, StateId, ReachableAcc, TempReachableAcc, AcceptAcc, TempAcceptAcc) , 
  add_state_to_list_if_accepting(NextState, TempAcceptAcc, NewAcceptAcc) , 
  map_get_reachable_and_accept_states(T, [NextState|TempReachableAcc], ReachableStates, NewAcceptAcc, AcceptStates).
map_get_reachable_and_accept_states([_|T], ReachableAcc, ReachableStates, AcceptAcc, AcceptStates) :- 
  map_get_reachable_and_accept_states(T, ReachableAcc, ReachableStates, AcceptAcc, AcceptStates).

add_state_to_list_if_accepting(State, List, NewList) :- 
  is_accept(State) , 
  ! , 
  NewList = [State|List].
add_state_to_list_if_accepting(_, List, List).

%% get_number_of_states(+Automaton, -States).
%
% Get the amount of states in the automaton.
get_number_of_states(Automaton, AmountOfStates) :- 
  get_states(Automaton, ReachableStates) , 
  length(ReachableStates, AmountOfStates).

%% get_live_states(+Automaton, -LiveStates).
%
% Return a set of live states. A state is live if it can reach an accepting state.
get_live_states(Automaton, LiveStates) :- 
  expand_singleton(Automaton) , 
  get_reachable_and_accept_states(Automaton, ReachableStates, AcceptStates) , 
  get_live_states(ReachableStates, AcceptStates, LiveStates).

%% get_live_states(+ReachableStates, +AcceptStates, -LiveStates).
%
% Return a set of live states for given reachable and accept states.
get_live_states(_, [], LiveStates) :- 
  ! , % no live states if no accepting states
  LiveStates = [].
get_live_states(ReachableStates, AcceptStates, LiveStates) :- 
  empty_map(BackwardPathMap) , 
  init_backward_path_map_for_states(ReachableStates, BackwardPathMap, BackwardPathMap2) , 
  % all accepting states are live and each state reaching a live state is live as well
  get_live_states_from_accepting_states(AcceptStates, BackwardPathMap2, AcceptStates, LiveStates).

%% get_live_states_from_accepting_states(+AcceptStates, +BackwardPathMap, +Acc, -LiveStates).
%
% Return a set of live states for given accept states and an initialised backward path map.
% Each state we find when exploring the backward path starting at accepting states is live as well.
get_live_states_from_accepting_states([], _, Acc, Acc).
get_live_states_from_accepting_states([State|WorkListRest], BackwardPathMap, Acc, LiveStates) :- 
  get_id(State, StateId) , 
  map_get(BackwardPathMap, StateId, PreStates) , 
  ! , 
  ord_subtract(PreStates, Acc, NotYetLiveStates) , 
  ord_union(Acc, NotYetLiveStates, NewAcc) , 
  ord_union(WorkListRest, NotYetLiveStates, NewWorkListRest) , 
  get_live_states_from_accepting_states(NewWorkListRest, BackwardPathMap, NewAcc, LiveStates).
get_live_states_from_accepting_states([_|WorkListRest], BackwardPathMap, Acc, LiveStates) :- 
  get_live_states_from_accepting_states(WorkListRest, BackwardPathMap, Acc, LiveStates).

% Create a map of state ids mapping to a set of states that have a direct transition to their key state (backward path).
init_backward_path_map_for_states([], BackwardPathMap, BackwardPathMap).
init_backward_path_map_for_states([ReachableState|T], BackwardPathMap, NewBackwardPathMap) :- 
  init_backward_path_map_for_state(ReachableState, BackwardPathMap, TempBackwardPathMap) , 
  init_backward_path_map_for_states(T, TempBackwardPathMap, NewBackwardPathMap).

init_backward_path_map_for_state(ReachableState, BackwardPathMap, NewBackwardPathMap) :-
  get_next_states(ReachableState, NextStates) , 
  init_backward_path_map_for_state_and_next_states(ReachableState, NextStates, BackwardPathMap, NewBackwardPathMap).

init_backward_path_map_for_state_and_next_states(_, [], BackwardPathMap, BackwardPathMap).
init_backward_path_map_for_state_and_next_states(ReachableState, [NextState|T], BackwardPathMap, NewBackwardPathMap) :-
  get_id(NextState, NextStateId) , 
  ( map_get(BackwardPathMap, NextStateId, SetOfStates) , 
    ! , 
    map_assoc(BackwardPathMap, NextStateId, [ReachableState|SetOfStates], TempBackwardPathMap)
  ; map_assoc(BackwardPathMap, NextStateId, [ReachableState], TempBackwardPathMap)) , 
  init_backward_path_map_for_state_and_next_states(ReachableState, T, TempBackwardPathMap, NewBackwardPathMap).

%% clone(+Automaton, -ClonedAutomaton).
%
% Returns a cloned automaton (deep copy).
clone(Automaton, ClonedAutomaton) :- 
  get_initial(Automaton, Initial) , 
  get_minimise_always(Automaton, MinimiseAlways),
  get_deterministic(Automaton, Deterministic),
  get_singleton(Automaton, Singleton) , 
  get_states(Automaton, States) , 
  new_automaton(ClonedAutomaton) , 
  set_minimise_always(ClonedAutomaton, MinimiseAlways) , 
  set_deterministic(ClonedAutomaton, Deterministic) , 
  set_singleton(ClonedAutomaton, Singleton) , 
  map_states_to_fresh_states(States, StatesMap) , 
  clone_transitions_to_fresh_states(States, StatesMap) , 
  get_id(Initial, InitialId) , 
  map_get(StatesMap, InitialId, ClonedInitial) , 
  set_initial(ClonedAutomaton, ClonedInitial).

%% clone_transitions_to_fresh_states(+States, +StatesMap).
%
% Clone the transitions of each state in the first argument's list to the 
% cloned states in the second argument's map.
clone_transitions_to_fresh_states([], _).
clone_transitions_to_fresh_states([State|T], StatesMap) :- 
  get_id(State, StateId) , 
  map_get(StatesMap, StateId, ClonedState) , 
  get_transitions(State, Transitions) , % Note: bottleneck for performance since we expanded the char ranges in states.pl
  dict_pairs(Transitions, transitions, KeyValueList) , 
  clone_transitions_to_fresh_state(ClonedState, KeyValueList, StatesMap) , 
  clone_transitions_to_fresh_states(T, StatesMap).

%% clone_transitions_to_fresh_state(+ClonedState, +Transitions, +StatesMap).
%
% Clone the transitions given in the second argument to the cloned state given in the first argument. 
% The map of cloned states is used to point cloned transitions to cloned states instead of the original ones.
clone_transitions_to_fresh_state(_, [], _).
clone_transitions_to_fresh_state(ClonedState, [Lit-Destinations|T], StatesMap) :- 
  add_transitions_to_fresh_state(Destinations, ClonedState, Lit, StatesMap) , 
  clone_transitions_to_fresh_state(ClonedState, T, StatesMap).

add_transitions_to_fresh_state([], _, _, _).
add_transitions_to_fresh_state([Destination|T], ClonedState, Lit, StatesMap) :- 
  get_id(Destination, StateId) , 
  map_get(StatesMap, StateId, FreshDestination) , 
  ! , 
  add_transition(ClonedState, [Lit,Lit]-FreshDestination) , 
  add_transitions_to_fresh_state(T, ClonedState, Lit, StatesMap).
add_transitions_to_fresh_state([_|T], ClonedState, Lit, StatesMap) :- 
  % dead states are removed when cloning
  add_transitions_to_fresh_state(T, ClonedState, Lit, StatesMap).

%% map_states_to_fresh_states(+States, -StatesMap).
%
% Create a map pointing ids of the original states to cloned (empty) states. 
% We need new State variables before cloning the transitions 
% since they point to attributed state variables.
map_states_to_fresh_states(States, StatesMap) :- 
  empty_map(EmptyMap) , 
  map_states_to_fresh_states(States, EmptyMap, StatesMap).

map_states_to_fresh_states([], Acc, Acc).
map_states_to_fresh_states([State|T], Acc, StatesMap) :- 
  get_id(State, StateId) , 
  new_state(ClonedState) , 
  map_assoc(Acc, StateId, ClonedState, NewAcc) , 
  map_states_to_fresh_states(T, NewAcc, StatesMap).

%% remove_dead_transitions(+Automaton).
%
% Removes all transitions to dead states. A state is dead if no accept state 
% is reachable from it.
remove_dead_transitions(Automaton) :- 
  % a singleton automaton is not expanded yet and, thus, does not have dead states
  is_singleton(Automaton) , 
  !.
remove_dead_transitions(Automaton) :- 
  get_reachable_and_accept_states(Automaton, ReachableStates, AcceptStates) , 
  get_live_states(ReachableStates, AcceptStates, LiveStates) , 
  remove_dead_transitions_from_states(ReachableStates, LiveStates) , 
  reduce(Automaton).

remove_dead_transitions_from_states([], _).
remove_dead_transitions_from_states([State|T], LiveStates) :- 
  get_transitions(State, Transitions) , 
  dict_pairs(Transitions, transitions, KeyValueList) , 
  reset_transitions(State) , 
  add_live_transitions_to_state(State, KeyValueList, LiveStates) , 
  remove_dead_transitions_from_states(T, LiveStates).

add_live_transitions_to_state(_, [], _).
add_live_transitions_to_state(State, [Lit-Destinations|T], LiveStates) :- 
  add_live_transitions_to_state(State, Lit, Destinations, LiveStates) , ! , 
  add_live_transitions_to_state(State, T, LiveStates).

add_live_transitions_to_state(_, _, [], _).
add_live_transitions_to_state(State, Lit, [Destination|T], LiveStates) :- 
  member(Destination, LiveStates) , % TODO: improve performance
  ! , 
  add_transition(State, [Lit,Lit]-Destination) , 
  add_live_transitions_to_state(State, Lit, T, LiveStates).
add_live_transitions_to_state(State, Lit, [_|T], LiveStates) :- 
  % transition leads to a dead state
  add_live_transitions_to_state(State, Lit, T, LiveStates).

%% get_number_of_transitions(+Automaton, -NrOfTransitions).
%
% Return the amount of transitions in the automaton.
% Note: Since we unfolded char ranges for transitions in state.pl, 
% get_number_of_transitions/2 returns a different value than the 
% original Java library.
get_number_of_transitions(Automaton, NrOfTransitions) :- 
  get_singleton(Automaton, Singleton) , 
  Singleton \== null , 
  ! , 
  atom_length(Singleton, NrOfTransitions).
get_number_of_transitions(Automaton, NrOfTransitions) :- 
  get_states(Automaton, States) , 
  get_number_of_transitions(States, 0, NrOfTransitions).

get_number_of_transitions([], Acc, Acc).
get_number_of_transitions([State|T], C, NrOfTransitions) :- 
  get_transitions(State, Transitions) , 
  dict_pairs(Transitions, transitions, KeyValueList) , 
  map_get_number_of_transitions(KeyValueList, 0, AccNr) , 
  C1 is C + AccNr , 
  get_number_of_transitions(T, C1, NrOfTransitions).

map_get_number_of_transitions([], AccNr, AccNr).
map_get_number_of_transitions([_-Destinations|T], AccNr, NrOfTransitions) :- 
  length(Destinations, Len) , 
  NewAccNr is AccNr + Len , 
  map_get_number_of_transitions(T, NewAccNr, NrOfTransitions).

% TODO:

expand_singleton(Automaton) :- Automaton = Automaton.

shuffle(Automaton) :- Automaton = Automaton.

% do we need this if we expand ranges? is it a good idea to expand ranges (see state.pl)?
reduce(Automaton) :- Automaton = Automaton.

% Skipped public Methods: toDot(), toString(), load(), store()
