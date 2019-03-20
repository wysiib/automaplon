:- module(test_util,[cleanup_state/0,
                     equal_lists_as_set/2]).

equal_lists_as_set(A,B) :-
    list_to_ord_set(A,SA),
    list_to_ord_set(B,SB),
    SA == SB.

cleanup_state :-
    retract(state:next_id(_)),
    assert(state:next_id(0)).
