:- module(state, [new_state/1]).

:- dynamic next_id/1.
next_id(0).
get_next_id(N) :-
    retract(next_id(N)),
    NN is N + 1,
    assert(next_id(NN)).
    
new_state(State) :-
    get_next_id(NextId),
    set_id(State,NextId).