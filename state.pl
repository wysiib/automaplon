:- module(state, [new_state/1]).

:- use_module(library(plunit)).

% Note: - skipped attribute 'int number'
%       - skipped methods 'compareTo', 'toString' and all methods that are not public

% Class Transition: implemented as a Prolog term like [Min,Max]-Destination

% Variable Attributes:
%   id - unique state id
%   accept - is true if state is accepting
%   transitions - a dictionary mapping a literal to a list of states, the list might contain duplicates

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
    put_attr(State,id,NextId).

%% get_transitions(+State, -Transitions).
%
% Access the data structure in which the all transitions are stored
% which lead away from State.
% The output variable Transitions might be unified with an empty data structure
% If no transitions are attached to the State.
get_transitions(State,TransitionsDict) :-
    get_attr(State,transitions,TransitionsDict) , !.
get_transitions(_,transitions{}).

%% get_transitions(+State, +Literal, -DestinationStates).
%
% Return a list of destination states reachable from State by transitioning
% with Literal.
get_transitions(State,Lit,Destinations) :-
    step(State, Lit, Destinations).

%% step(+State, +Literal, -DestinationStates) is det.
%
% Return a list of destination states reachable from State by transitioning
% with Literal.
% If no state is reachable by the specified literal the resulting list will
% be empty.
step(State,Lit,Destinations) :-
    get_transitions(State,TransitionsDict) ,
    get_dict(Lit,TransitionsDict,Destinations) ,
    !. % red cut.
step(_,_,[]).

%% add_transition(+State, +Transition).
%
% Attaches the Transition to the State.
% The transition has the form =|[Min,Max]-Dest|=,
% with Min being the lower bound literal and Max being the upper bound literal
% of a range of literals that transition into the destination state Dest.
%
% For instance, =|add_transition(S1, [a,d]-S2)|= allows to transition from
% state S1 to S2 with either literal from a to d.
add_transition(State,[Min,Max]-Destination) :-
    add_transition_range(State,Min,Max,Destination).

%% add_transition(+State, +Literal, +Destination).
%
% Add a single transition to the Destination state from State by using
% the Literal.
add_transition(State,Lit,Destination) :-
    get_transitions(State,TransitionsDict) ,
    (get_dict(Lit,TransitionsDict,Destinations) ; Destinations = []) ,
    put_dict(Lit,TransitionsDict,[Destination|Destinations],NewTransitionsDict) ,
    put_attr(State,transitions,NewTransitionsDict).


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
    % NOTE: we add each literal to the dictionary, the original library uses range abstractions but has to use Automaton#reduce()
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
    put_attr(State,transitions,transitions{}).

%% set_accept(+State, +Accept).
%
% Sets whether State shall be accepting or not.
% State will be accepting if and only iff Accept == true.
set_accept(State,Accept) :-
    put_attr(State,accept,Accept).

%% is_accept(+State).
%
% Succeeds if State is accepting.
is_accept(State) :-
    get_attr(State,accept,Accept) ,
    Accept == true.

equals(State1,State2) :-
    State1 == State2.

cleanup_state :-
    retract(next_id(_)) ,
    assert(next_id(0)).

:- begin_tests(state_basic).

test(construction,[setup(cleanup_state)]) :-
    new_state(S1) ,
    new_state(S2) ,
    get_attr(S1,id,Id1) ,
    get_attr(S2,id,Id2) ,
    Id1 == 0 ,
    Id2 == 1 ,
    \+ equals(S1,S2) ,
    \+ equals(S2,S1) ,
    equals(S1,S1) ,
    equals(S2,S2).

test(equality,[setup(cleanup_state)]) :-
    new_state(S1) ,
    equals(S1, S1).

test(inequality,[setup(cleanup_state)]) :-
    new_state(S1) ,
    new_state(S2) ,
    \+ equals(S1, S2).

test(non_accepting_after_instantiation) :-
    new_state(S1),
    \+ is_accept(S1).

test(accepting_state) :-
    new_state(S1),
    set_accept(S1, true),
    is_accept(S1).

test(non_accepting_state) :-
    new_state(S1),
    set_accept(S1, false),
    \+ is_accept(S1).

test(toggle_accepting_state) :-
    new_state(S1),
    set_accept(S1, true),
    set_accept(S1, false),
    \+ is_accept(S1).

test(backtrack_accepting_state) :-
    new_state(S1),
    set_accept(S1, true),
    (set_accept(S1, false), fail ; true),
    is_accept(S1).

:- end_tests(state_basic).

:- begin_tests(helper_predicates).

test(generate_literals) :-
    literals_list(a, d, [a, b, c, d]).

:- end_tests(helper_predicates).

:- begin_tests(transitions).

test(contains_transitions) :-
    new_state(S1),
    new_state(S2),
    add_transition(S1,a,S2),
    get_transitions(S1, a, Ts),
    Ts == [S2].

test(add_singlelabel_transition) :-
    new_state(S1),
    new_state(S2),
    add_transition(S1,[a,a]-S2),
    get_transitions(S1, a, Ts),
    Ts == [S2].

test(add_multilabel_transition) :-
    new_state(S1),
    new_state(S2),
    add_transition(S1,[a,f]-S2),
    step(S1, a, Ts),
    step(S1, b, Ts),
    step(S1, c, Ts),
    step(S1, d, Ts),
    step(S1, e, Ts),
    step(S1, a, Ts),
    Ts == [S2].

:- end_tests(transitions).
