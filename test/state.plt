
:- use_module(library(plunit)).
:- use_module(test_util).
:- use_module('../src/state.pl').

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

test(non_accepting_after_instantiation,[setup(cleanup_state)]) :-
    new_state(S1),
    \+ is_accept(S1).

test(accepting_state,[setup(cleanup_state)]) :-
    new_state(S1),
    set_accept(S1, true),
    is_accept(S1).

test(non_accepting_state,[setup(cleanup_state)]) :-
    new_state(S1),
    set_accept(S1, false),
    \+ is_accept(S1).

test(toggle_accepting_state,[setup(cleanup_state)]) :-
    new_state(S1) ,
    set_accept(S1, true),
    is_accept(S1) , 
    set_accept(S1, false),
    \+ is_accept(S1) ,
    set_accept(S1, true),
    is_accept(S1) , 
    set_accept(S1, false),
    \+ is_accept(S1).

test(backtrack_accepting_state,[setup(cleanup_state)]) :-
    new_state(S1),
    set_accept(S1, true),
    (set_accept(S1, false), fail ; true),
    is_accept(S1).

test(add_get_transition1,[setup(cleanup_state)]) :- 
    new_state(S1) , 
    new_state(S2) , 
    add_transition(S1,[a,d]-S2) , 
    get_transitions(S1,a,TransitionsA) , 
    equal_lists_as_set(TransitionsA,[S2]) , 
    get_transitions(S1,b,TransitionsB) , 
    equal_lists_as_set(TransitionsB,[S2]) , 
    get_transitions(S1,c,TransitionsC) , 
    equal_lists_as_set(TransitionsC,[S2]) , 
    get_transitions(S1,d,TransitionsD) , 
    equal_lists_as_set(TransitionsD,[S2]).

test(add_get_transition2,[setup(cleanup_state)]) :- 
    new_state(S1) , 
    new_state(S2) , 
    new_state(S3) , 
    add_transition(S1,[a,d]-S2) , 
    add_transition(S1,[a,d]-S3) , 
    get_transitions(S1,a,TransitionsA) , 
    equal_lists_as_set(TransitionsA,[S2,S3]) , 
    get_transitions(S1,b,TransitionsB) , 
    equal_lists_as_set(TransitionsB,[S2,S3]) , 
    get_transitions(S1,c,TransitionsC) , 
    equal_lists_as_set(TransitionsC,[S2,S3]) , 
    get_transitions(S1,d,TransitionsD) , 
    equal_lists_as_set(TransitionsD,[S2,S3]) , 
    get_transitions(S2,d,TransitionsD2) , 
    TransitionsD2 == [] , 
    get_transitions(S3,k,TransitionsK3) , 
    TransitionsK3 == [].

test(add_get_transition3,[setup(cleanup_state)]) :- 
    new_state(S1) , 
    new_state(S2) , 
    new_state(S3) , 
    new_state(S4) , 
    add_transition(S1,[m,q]-S2) , 
    add_transition(S1,[m,x]-S3) , 
    add_transition(S1,[l,z]-S4) , 
    get_transitions(S1,l,TransitionsN) , 
    equal_lists_as_set(TransitionsN,[S4]) , 
    get_transitions(S1,m,TransitionsM) , 
    equal_lists_as_set(TransitionsM,[S2,S3,S4]) , 
    get_transitions(S1,p,TransitionsP) , 
    equal_lists_as_set(TransitionsP,[S2,S3,S4]) , 
    get_transitions(S1,r,TransitionsR) , 
    equal_lists_as_set(TransitionsR,[S3,S4]) , 
    get_transitions(S1,x,TransitionsX) , 
    equal_lists_as_set(TransitionsX,[S3,S4]) ,
    get_transitions(S1,y,TransitionsY) , 
    equal_lists_as_set(TransitionsY,[S4]) , 
    get_transitions(S1,z,TransitionsZ) , 
    equal_lists_as_set(TransitionsZ,[S4]) , 
    get_transitions(S4,d,TransitionsD4) , 
    TransitionsD4 == [] , 
    get_transitions(S3,k,TransitionsK3) , 
    TransitionsK3 == [].

test(add_get_transition4,[setup(cleanup_state)]) :- 
    new_state(S1) , 
    new_state(S2) , 
    new_state(S3) , 
    new_state(S4) , 
    add_transition(S1,[m,q]-S2) , 
    add_transition(S1,[a,z]-S3) , 
    get_transitions(S1,b,TransitionsB) , 
    equal_lists_as_set(TransitionsB,[S3]) , 
    get_transitions(S1,m,TransitionsM) , 
    equal_lists_as_set(TransitionsM,[S2,S3]) , 
    get_transitions(S1,p,TransitionsP) , 
    equal_lists_as_set(TransitionsP,[S2,S3]) ,
    add_transition(S2,[a,d]-S3) , 
    add_transition(S2,[a,z]-S4) ,  
    get_transitions(S2,r,TransitionsR) , 
    equal_lists_as_set(TransitionsR,[S4]) , 
    get_transitions(S2,c,TransitionsC) , 
    equal_lists_as_set(TransitionsC,[S3,S4]) ,
    get_transitions(S2,z,TransitionsZ) , 
    equal_lists_as_set(TransitionsZ,[S4]) , 
    get_transitions(S3,a,TransitionsA3) , 
    TransitionsA3 == [] , 
    get_transitions(S4,k,TransitionsK4) , 
    TransitionsK4 == [].

test(add_get_transition_backtrackable,[setup(cleanup_state)]) :- 
    new_state(S1) , 
    new_state(S2) , 
    ( add_transition(S1,[a,d]-S2) , 
      get_transitions(S1,a,TransitionsA) , 
      TransitionsA == [S2] , 
      get_transitions(S1,b,TransitionsB) , 
      TransitionsB == [S2] , 
      get_transitions(S1,c,TransitionsC) , 
      TransitionsC == [S2] ,
      get_transitions(S1,d,TransitionsD) , 
      TransitionsD == [S2] , 
      fail
    ; get_transitions(S1,a,TransitionsA) , 
      TransitionsA == [] , 
      get_transitions(S1,b,TransitionsB) , 
      TransitionsB == [] , 
      get_transitions(S1,c,TransitionsC) , 
      TransitionsC == [] , 
      get_transitions(S1,d,TransitionsD) , 
      TransitionsD == []).

test(add_get_transition_backtrackable2,[setup(cleanup_state)]) :- 
    new_state(S1) , 
    new_state(S2) , 
    new_state(S3) , 
    new_state(S4) , 
    ( add_transition(S1,[a,b]-S2) , 
      add_transition(S1,[a,b]-S3) , 
      add_transition(S1,[a]-S4) , 
      get_transitions(S1,a,TransitionsA) , 
      TransitionsA == [S2,S3,S4] , 
      get_transitions(S1,b,TransitionsB) , 
      TransitionsB == [S2,S3] , 
      fail
    ; get_transitions(S1,a,TransitionsA) , 
      TransitionsA == [] , 
      get_transitions(S1,b,TransitionsB) , 
      TransitionsB == []).

test(next_states1,[setup(cleanup_state)]) :- 
    new_state(S1) , 
    new_state(S2) , 
    new_state(S3) , 
    new_state(S4) , 
    add_transition(S1, [a,b]-S2) , 
    add_transition(S1, [a,b]-S3) , 
    add_transition(S1, [a,a]-S4) , 
    get_next_states(S1, NextStates) , 
    equal_lists_as_set(NextStates, [S2, S3, S4]).

test(next_states2,[setup(cleanup_state)]) :- 
    new_state(S1) , 
    new_state(S2) , 
    new_state(S3) , 
    new_state(S4) , 
    add_transition(S1, [a,b]-S2) , 
    add_transition(S1, [a,a]-S4) , 
    add_transition(S2, [a,b]-S3) , 
    get_next_states(S1, NextStatesS1) , 
    equal_lists_as_set(NextStatesS1, [S2, S4]) , 
    get_next_states(S2, NextStatesS2) , 
    equal_lists_as_set(NextStatesS2, [S3]).

:- end_tests(state_basic).

:- begin_tests(helper_predicates).

test(generate_literals) :-
    state:literals_list(a, d, Lits1) , 
    Lits1 == [a, b, c, d] , 
    state:literals_list(a, e, Lits2) , 
    Lits2 == [a, b, c, d, e] , 
    state:literals_list(e, g, Lits3) , 
    Lits3 == [e, f, g] , 
    state:literals_list(a, a, Lits4) , 
    Lits4 == [a] , 
    state:literals_list(j, j, Lits5) , 
    Lits5 == [j] , 
    state:literals_list(z, z, Lits6) , 
    Lits6 == [z].

test(generate_literals_wrong_input1,[fail]) :-
    state:literals_list(d, a, _).

test(generate_literals_wrong_input2,[fail]) :-
    state:literals_list(z, t, _).

test(generate_literals_fail_unification1,[fail]) :-
    state:literals_list(e, t, [a, b, c]).

test(generate_literals_fail_unification2,[fail]) :-
    state:literals_list(a, z, [a, b, c, d, e, f, g, h]).

test(generate_literals_fail_unification_after_solution1,[fail]) :- 
    state:literals_list(a, d, Lits) , 
    Lits = [a, b].

test(generate_literals_fail_unification_after_solution2,[fail]) :- 
    state:literals_list(k, z, Lits) , 
    Lits = [k, l, m, n, O, O].

:- end_tests(helper_predicates).

:- begin_tests(transitions).

test(contains_transitions,[setup(cleanup_state)]) :-
    new_state(S1),
    new_state(S2),
    state:add_transition(S1,a,S2),
    get_transitions(S1, a, Ts),
    Ts == [S2].

test(add_singlelabel_transition,[setup(cleanup_state)]) :-
    new_state(S1),
    new_state(S2),
    state:add_transition(S1,[a,a]-S2),
    get_transitions(S1, a, Ts),
    Ts == [S2].

test(add_multilabel_transition,[setup(cleanup_state)]) :-
    new_state(S1),
    new_state(S2),
    state:add_transition(S1,[a,f]-S2),
    step(S1, a, Ts),
    step(S1, b, Ts),
    step(S1, c, Ts),
    step(S1, d, Ts),
    step(S1, e, Ts),
    step(S1, a, Ts),
    Ts == [S2].

:- end_tests(transitions).
