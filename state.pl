:- module(state, [new_state/1,
                  is_accept/1,
                  get_transitions/2,
                  get_transitions/3 ,
                  add_transition/2,
                  set_accept/2,
                  step/3]).

:- use_module(util/maps).

% Note: - skipped attribute 'int number'
%       - skipped methods 'compareTo', 'toString' and all methods that are not public

% Class Transition: implemented as a Prolog term like [Min,Max]-Destination

% Variable Attributes:
%   id - unique state id
%   accept - is true if state is accepting
%   transitions - a map associating a literal to a list of states, the list might contain duplicates

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
get_transitions(State,Transitions) :-
    get_attr(State,transitions,Transitions) ,
    !.
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
    get_transitions(State,Transitions) ,
    map_get(Transitions, Lit, Destinations) ,
    !.
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
    get_transitions(State,Transitions) ,
    (map_get(Transitions,Lit,Destinations) ; Destinations = []) , 
    ! , 
    map_assoc(Transitions,Lit,[Destination|Destinations],NewTransitions) , % TODO: use a set for Destinations?
    put_attr(State,transitions,NewTransitions).

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


:- use_module(library(plunit)).

equal_lists_as_set(A,B) :- 
    list_to_ord_set(A,SA) , 
    list_to_ord_set(B,SB) , 
    SA == SB.

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
    new_state(S1) ,
    set_accept(S1, true),
    is_accept(S1) , 
    set_accept(S1, false),
    \+ is_accept(S1) ,
    set_accept(S1, true),
    is_accept(S1) , 
    set_accept(S1, false),
    \+ is_accept(S1).

test(backtrack_accepting_state) :-
    new_state(S1),
    set_accept(S1, true),
    (set_accept(S1, false), fail ; true),
    is_accept(S1).

test(add_get_transition1) :- 
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

test(add_get_transition2) :- 
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

test(add_get_transition3) :- 
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

test(add_get_transition4) :- 
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

test(add_get_transition_backtrackable) :- 
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

test(add_get_transition_backtrackable2) :- 
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

:- end_tests(state_basic).

:- begin_tests(helper_predicates).

test(generate_literals) :-
    literals_list(a, d, Lits1) , 
    Lits1 == [a, b, c, d] , 
    literals_list(a, e, Lits2) , 
    Lits2 == [a, b, c, d, e] , 
    literals_list(e, g, Lits3) , 
    Lits3 == [e, f, g] , 
    literals_list(a, a, Lits4) , 
    Lits4 == [a] , 
    literals_list(j, j, Lits5) , 
    Lits5 == [j] , 
    literals_list(z, z, Lits6) , 
    Lits6 == [z].

test(generate_literals_wrong_input1,[fail]) :-
    literals_list(d, a, _).

test(generate_literals_wrong_input2,[fail]) :-
    literals_list(z, t, _).

test(generate_literals_fail_unification1,[fail]) :-
    literals_list(e, t, [a, b, c]).

test(generate_literals_fail_unification2,[fail]) :-
    literals_list(a, z, [a, b, c, d, e, f, g, h]).

test(generate_literals_fail_unification_after_solution1,[fail]) :- 
    literals_list(a, d, Lits) , 
    Lits = [a, b].

test(generate_literals_fail_unification_after_solution1,[fail]) :- 
    literals_list(k, z, Lits) , 
    Lits = [k, l, m, n, O, O].

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
