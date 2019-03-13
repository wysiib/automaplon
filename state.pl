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

new_state(State) :-
    get_next_id(NextId),
    put_attr(State,id,NextId).

get_transitions(State,TransitionsDict) :-
    get_attr(State,transitions,TransitionsDict) , !.
get_transitions(_,transitions{}).

get_transitions(State,Lit,Destinations) :-
    get_transitions(State,Transitions),
    Destinations = Transitions.get(Lit).

add_transition(State,[Min,Max]-Destination) :-
    add_transitions_in_range(State,Min,Max,Destination).

% Note: we add each literal to the dictionary, the original library uses range abstractions but has to use Automaton#reduce()
% TODO: evaluate pros and cons
add_transitions_in_range(_,Lit,Max,_) :-
    Lit > Max , !.
add_transitions_in_range(State,Lit,Max,Destination) :-
    add_transition(State,Lit,Destination) ,
    Lit1 is Lit + 1 ,
    add_transitions_in_range(State,Lit1,Max,Destination).

add_transition(State,Lit,Destination) :-
    get_transitions(State,TransitionsDict) ,
    (get_dict(Lit,TransitionsDict,Destinations) ; Destinations = []) ,
    put_dict(Lit,TransitionsDict,[Destination|Destinations],NewTransitionsDict) ,
    put_attr(State,transitions,NewTransitionsDict).

reset_transitions(State) :-
    put_attr(State,transitions,transitions{}).

set_accept(State,Accept) :-
    put_attr(State,accept,Accept).

is_accept(State) :-
    get_attr(State,accept,Accept) ,
    Accept == true.

equals(State1,State2) :- 
    State1 == State2.

step(State,Lit,Destinations) :-
    get_transitions(State,TransitionsDict) ,
    get_dict(Lit,TransitionsDict,Destinations) , !.
step(_,_,[]).

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
    print(foo),
    \+ equals(S1,S2) ,
    print(bar),
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


test(contains_transitions,[setup(cleanup_state)]) :-
    new_state(S1),
    new_state(S2),
    add_transition(S1,a,S2),
    get_transitions(S1, a, Ts),
    Ts == [S2].

:- end_tests(state_basic).
