:- use_module(library(plunit)).
:- use_module(src/util/maps).

:- begin_tests(empty_maps).

test(new_maps_are_empty) :-
  empty_map(M),
  assertion(map_is_empty(M)).

test(variables_are_no_empty_maps) :-
  assertion(\+ map_is_empty(_)).

test(maps_with_content_are_not_empty) :-
  empty_map(Empty),
  map_assoc(Empty, key, value, M),
  assertion(\+ empty_map(M)),
  assertion(\+ map_is_empty(M)).

:- end_tests(empty_maps).

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

:- begin_tests(listing_map_entries).

test(list_pairs) :-
  empty_map(M0),
  map_assoc(M0, a, 1, M1),
  map_assoc(M1, b, 2, M2),
  map_assoc(M2, c, 3, M3),
  map_pairs(M3, Pairs),
  assertion(length(Pairs, 3)),
  assertion(member(a-1, Pairs)),
  assertion(member(b-2, Pairs)),
  assertion(member(c-3, Pairs)).

test(list_keys) :-
  empty_map(M0),
  map_assoc(M0, a, 1, M1),
  map_assoc(M1, b, 2, M2),
  map_assoc(M2, c, 3, M3),
  map_keys(M3, Keys),
  assertion(length(Keys, 3)),
  assertion(member(a, Keys)),
  assertion(member(b, Keys)),
  assertion(member(c, Keys)).

test(list_values) :-
  empty_map(M0),
  map_assoc(M0, a, 1, M1),
  map_assoc(M1, b, 2, M2),
  map_assoc(M2, c, 3, M3),
  map_values(M3, Values),
  assertion(length(Values, 3)),
  assertion(member(1, Values)),
  assertion(member(2, Values)),
  assertion(member(3, Values)).

:- end_tests(listing_map_entries).
