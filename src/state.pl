:- module(state, [new_state/1,
                  is_accept/1,
                  reset_transitions/1,
                  get_id/2,
                  get_next_states/2,
                  get_transitions/2,
                  get_transitions/3,
                  add_transition/2,
                  set_accept/2,
                  equals/2,
                  step/3]).

:- use_module(util/maps).
:- use_module(library(ordsets)).

% Note: - skipped attribute 'int number'
%       - skipped methods 'compareTo', 'toString' and all methods that are not public

% Class Transition: implemented as a Prolog term like [Min,Max]-Destination

% Variable Attributes:
%   id - unique state id
%   accept - is true if state is accepting
%   transitions - a map associating a literal to a list of states, the list might contain duplicates
%   next_states - an ordered set of states reachable in one step

:- dynamic next_id/1.
:- volatile next_id/1.

next_id(0).

get_next_id(N) :-
    retract(next_id(N)),
    NN is N + 1,
    assert(next_id(NN)).

%% get_id(+State, -Id).
%
% Return a state's id.
get_id(State, Id) :- 
    get_attr(State, id, Id).

%% new_state(-State).
%
% Instantiates a fresh automaton state.
new_state(State) :-
    get_next_id(NextId),
    put_attr(State, id, NextId) , 
    put_attr(State, next_states, []).

%% get_next_states(+State, -NextStates).
% 
% Return the set of states reachable in one step.
get_next_states(State, NextStates) :- 
    get_attr(State, next_states, NextStates).

%% get_transitions(+State, -Transitions).
%
% Access the data structure in which the all transitions are stored
% which lead away from State.
% The output variable Transitions might be unified with an empty data structure
% If no transitions are attached to the State.
get_transitions(State, Transitions) :-
    get_attr(State, transitions, Transitions) ,
    !.
get_transitions(_, transitions{}).

%% get_transitions(+State, +Literal, -DestinationStates).
%
% Return a list of destination states reachable from State by transitioning
% with Literal.
get_transitions(State, Lit, Destinations) :-
    step(State, Lit, Destinations).

%% step(+State, +Literal, -DestinationStates) is det.
%
% Return a list of destination states reachable from State by transitioning
% with Literal.
% If no state is reachable by the specified literal the resulting list will
% be empty.
step(State, Lit, Destinations) :-
    get_transitions(State, Transitions) ,
    map_get(Transitions, Lit, Destinations) ,
    !.
step(_, _, []).

%% add_transition(+State, +Transition).
%
% Attaches the Transition to the State.
% The transition has the form =|[Min,Max]-Dest|=,
% with Min being the lower bound literal and Max being the upper bound literal
% of a range of literals that transition into the destination state Dest.
%
% For instance, =|add_transition(S1, [a,d]-S2)|= allows to transition from
% state S1 to S2 with either literal from a to d.
add_transition(State, [Min,Max]-Destination) :-
    add_transition_range(State, Min, Max, Destination) , 
    get_attr(State, next_states, OldNextStates) , 
    ord_add_element(OldNextStates, Destination, NewNextStates) , 
    put_attr(State, next_states, NewNextStates).

%% add_transition(+State, +Literal, +Destination).
%
% Add a single transition to the Destination state from State by using
% the Literal.
add_transition(State, Lit, Destination) :-
    get_transitions(State, Transitions) ,
    (map_get(Transitions, Lit, Destinations) ; Destinations = []) , % TODO: use a set or difference list if we need to append States more often?
    ! , 
    map_assoc(Transitions, Lit, [Destination|Destinations], NewTransitions) , 
    put_attr(State, transitions, NewTransitions).

%% add_transition_range(+State, +Min, +Max, +Dest).
%
% Add transitions
add_transition_range(State, Min, Max, Dest) :-
    literals_list(Min, Max, Lits),
    add_transition_range(Lits, State, Dest).

%% add_transition_range(+Literals, +State, +Destination).
%
% Adds a transition from State to Destination for each literal in
% the list of Literals.
add_transition_range([], _, _).
add_transition_range([L|Ls], State, Dest) :-
    % NOTE: we add each literal to the map, the original library uses range abstractions but has to use Automaton#reduce()
    % TODO: evaluate pros and cons
    add_transition(State, L, Dest),
    add_transition_range(Ls, State, Dest).

%% literals_list(+Minimum, +Maximum, -Literals).
%
% Generates a list of literals from Minimum to Maximum.
% Minimum and Maximum have to be single character atoms.
%
% Example:
% ===
% ?- literals_list(a, d, Lits).
% L = [a, b, c, d].
% ===
literals_list(Min, Max, Literals) :-
    atom_codes(Min, [AsciiMin]),
    atom_codes(Max, [AsciiMax]),
    numlist(AsciiMin, AsciiMax, Asciis),
    findall(L, (member(A, Asciis), atom_codes(L, [A])), Literals).

%% reset_transitions(+State).
%
% Remove all transitions attached to State.
reset_transitions(State) :-
    put_attr(State, transitions, transitions{}) , 
    put_attr(State, next_states, []).

%% set_accept(+State, +Accept).
%
% Sets whether State shall be accepting or not.
% State will be accepting iff Accept == true.
set_accept(State, Accept) :-
    put_attr(State, accept, Accept).

%% is_accept(+State).
%
% Succeeds if State is accepting.
is_accept(State) :-
    get_attr(State, accept, Accept) ,
    Accept == true.

%% is_accept(+State1, +State2).
%
% True if two states are exactly the same.
equals(State1, State2) :-
    State1 == State2.

%% Hooks for unification of a State variable's attributes.
% We do not allow unification of two States but check for equality only, i.e., 
% both states have the same attribute id (both States are exactly the same).
id:attr_unify_hook(IdX, Y) :- 
    get_id(Y, IdY) , 
    IdX == IdY.
