:- module(state, [new_state/1,
                  is_state/1,
                  is_accept/1,
                  reset_transitions/1,
                  add_epsilon/2,
                  add_transition/2,
                  add_transition/3,
                  add_transition/4,
                  set_accept/2,
                  equals/2,
                  set_number/2,
                  get_number/2,
                  get_id/2,
                  get_accept/2,
                  get_next_states/2,
                  get_transitions/2,
                  get_transitions/3,
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
%   number - a number which is not necessarily unique among states (we are then able to enumerate a set of states and, for instance, use a bitset)

:- dynamic next_id/1.
:- volatile next_id/1.

next_id(0).

get_next_id(N) :-
    retract(next_id(N)),
    NN is N + 1,
    assert(next_id(NN)).

%% new_state(-State).
%
% Instantiates a fresh automaton state.
new_state(State) :-
    get_next_id(NextId),
    put_attr(State, id, NextId),
    put_attr(State, accept, false),
    empty_map(Transitions),
    put_attr(State, transitions, Transitions),
    put_attr(State, next_states, []).

%% is_state(+State).
%
% Succeeds if properly defined state.
is_state(State) :-
    attvar(State),
    get_attr(State, id, _),
    get_attr(State, accept, _),
    get_attr(State, transitions, _),
    get_attr(State, next_states, _).

%% get_id(+State, -Id).
%
% True if Id is the state's id.
get_id(State, Id) :-
    get_attr(State, id, Id).

%% get_number(+State, -Number).
%
% True if Number is the State's ground number, otherwise false.
get_number(State, Number) :-
    get_attr(State, number, Number).

%% set_number(+State, -Id).
%
% Set a state's number.
set_number(State, Number) :-
    put_attr(State, number, Number).

%% add_epsilon(State, Destination).
%
% Add an epsilon transition from State to Destination.
% Note: The original Java library does not introduce real epsilon transitions.
% Consequently, the transitions of an automaton that has introduced an "epsilon"
% transition depend on the time when a transition was added. For instance,
% add an epsilon transition from S1 to S2 and add a transition from S2 to S3
% afterwards. Then, one cannot transition from S1 to S3 in the original Java implementation.
% When introducing a real epsilon transition this is possible.
add_epsilon(State, Destination) :-
  add_transition(State, '', Destination).

%% get_next_states(+State, -NextStates).
%
% True if NextStates is the set of states reachable in one step.
get_next_states(State, NextStates) :-
    get_attr(State, next_states, NextStates).

%% get_transitions(+State, -Transitions).
%
% True if Transitions is the data structure of transitions from State.
get_transitions(State, Transitions) :-
    get_attr(State, transitions, Transitions).

%% get_transitions(+State, +Lit, -DestinationStates).
%
% True if Destinations is a list of destination states reachable from State by transitioning
% with Lit.
get_transitions(State, Lit, Destinations) :-
    step(State, Lit, Destinations).

%% step(+State, +Literal, -DestinationStates) is det.
%
% True if Destinations is a list of destination states reachable from State by transitioning
% with Lit. If no state is reachable by the specified literal Destinations will be the empty list.
step(State, Lit, Destinations) :-
    get_transitions(State, Transitions),
    map_get(Transitions, Lit, Destinations),
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
add_transition(State, [Min, Max]-Destination) :-
    add_transition_range(State, Min, Max, Destination),
    get_attr(State, next_states, OldNextStates),
    ord_add_element(OldNextStates, Destination, NewNextStates),
    put_attr(State, next_states, NewNextStates).

%% add_transition(+State, +Lit, +Destination).
%
% Attaches the a transitions from the State to Destination using the literal Lit.
add_transition(State, Lit, Destination) :-
    add_transition(State, [Lit, Lit]-Destination).

%% add_transition(+State, +Min, +Max, +Destination).
%
% Attaches the a transitions from the State to Destination using the literals
% between Min and Max.
add_transition(State, Min, Max, Destination) :-
    add_transition(State, [Min, Max]-Destination).

%% add_transition_internal(+State, +Literal, +Destination).
%
% Add a single transition to the Destination state from State by using the Literal.
% Note: only to be used in add_transition/2, attribute next_states is not updated here!
add_transition_internal(State, Lit, Destination) :-
    get_transitions(State, Transitions),
    (map_get(Transitions, Lit, Destinations) ; Destinations = []), % TODO: use a set?
    !,
    map_assoc(Transitions, Lit, [Destination|Destinations], NewTransitions),
    put_attr(State, transitions, NewTransitions).

%% add_transition_range(+State, +Min, +Max, +Dest).
%
% Add transitions from +State to +Dest for each char between +Min and +Max.
add_transition_range(State, Min, Max, Dest) :-
    literals_list(Min, Max, Lits),
    !,
    add_transition_range(Lits, State, Dest).
% Special case for the empty word.
add_transition_range(State, '', '', Dest) :-
    add_transition_range([''], State, Dest).

%% add_transition_range(+Literals, +State, +Destination).
%
% Adds a transition from State to Destination for each literal in
% the list of Literals.
add_transition_range([], _, _).
add_transition_range([L|Ls], State, Dest) :-
    % NOTE: we add each literal to the map, the original library uses range abstractions but has to use Automaton#reduce()
    % TODO: evaluate pros and cons
    add_transition_internal(State, L, Dest),
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
    asciis_to_atom_codes(Asciis,D-D,Literals).

asciis_to_atom_codes([], Acc-D, Acc) :- D = [].
asciis_to_atom_codes([Ascii|T], Acc-D, Literals) :-
    atom_codes(Codes,[Ascii]),
    D = [Codes|ND],
    asciis_to_atom_codes(T, Acc-ND, Literals).

%% reset_transitions(+State).
%
% Remove all transitions attached to State.
reset_transitions(State) :-
    empty_map(EmptyMap),
    put_attr(State, transitions, EmptyMap),
    put_attr(State, next_states, []).

%% set_accept(+State, +Accept).
%
% Sets whether State shall be accepting or not.
% State will be accepting iff Accept == true.
set_accept(State, Accept) :-
    put_attr(State, accept, Accept).

%% get_accept(+State, -Accept).
%
% True if Accept is the acceptance attribute of State, i.e., either the atom true or false.
get_accept(State, Accept) :-
    get_attr(State, accept, Accept).

%% is_accept(+State).
%
% True if State is accepting.
is_accept(State) :-
    get_attr(State, accept, Accept),
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
    get_id(Y, IdY),
    IdX == IdY.
