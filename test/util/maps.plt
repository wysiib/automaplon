:- use_module(library(plunit)).
:- use_module(src/util/maps).

:- begin_tests(map_manipulations).

test(no_value_associated) :-
  empty_map(M),
  \+ map_get(M, key, _).

test(access_default_value) :-
  empty_map(M),
  map_get(M, key, default, Value),
  Value == default.

test(assoc) :-
  empty_map(Empty),
  map_assoc(Empty, key, value, M),
  map_get(M, key, Value),
  Value == value.

test(assoc_variable) :-
  empty_map(Empty),
  map_assoc(Empty, key, V, M),
  map_get(M, key, Value),
  Value == V.

test(override_key) :-
  empty_map(Empty),
  map_assoc(Empty, key, value, M),
  map_assoc(M, key, new, M2),
  map_get(M2, key, Value),
  Value == new.

test(override_variable) :-
  empty_map(Empty),
  map_assoc(Empty, key, V, M),
  map_assoc(M, key, new, M2),
  map_get(M2, key, Value),
  \+ V == new,
  Value == new.

test(backtrack_assoc) :-
  % Test can only fail if implementation is not pure.
  empty_map(Empty),
  map_assoc(Empty, key, old, M),
  (map_assoc(M, key, new, M2), fail ; M2 = M),
  map_get(M2, key, Value),
  Value == old.

test(backtrack_variable_assoc) :-
  empty_map(Empty),
  map_assoc(Empty, key, V, M),
  (map_assoc(M, key, new, M2), fail ; M2 = M),
  map_get(M2, key, Value),
  Value == V,
  \+ V == new.

test(access_does_not_unify) :-
  empty_map(Empty),
  map_assoc(Empty, key, V, M),
  \+ map_get(M, key, unified),
  \+ V == unified.

test(dissoc) :-
  empty_map(Empty),
  map_assoc(Empty, key, value, M),
  map_dissoc(M, key, Dissoced),
  Empty == Dissoced.

test(dissoc_non_existent_fails) :-
  empty_map(Empty),
  \+ map_dissoc(Empty, key, _).


:- end_tests(map_manipulations).
