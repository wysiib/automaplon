:- module(maps,
          [ empty_map/1,
            map_is_empty/1,
            map_assoc/4,
            map_dissoc/3,
            map_get/3,
            map_get/4,
            map_pairs/2,
            map_keys/2,
            map_values/2
          ]).

%% <module> Maps for automata.
%
% Wrapper API for any arbitrary implementation of maps.
% If the underlying data structure shall be replaced, it can be done
% by modifying this file and this file only instead of everywhere.
%
% Currently works as a wrapper for SWI dicts.

%% empty_map(-Map).
%
% Unifies with an empty map structure.
empty_map(map{}).

%% map_is_empty(+Map).
%
% Succeeds if Map is bound to an empty map construct.
% In comparison to empty_map/1, this does not unify any variable given as
% argument.
map_is_empty(Map) :-
    map{} == Map.


%% map_assoc(+Map, +Key, +Value, -NewMap).
%
% Associates Key to Value in NewMap, keeping all other associations from Map.
map_assoc(Map, Key, Value, NewMap) :-
    put_dict(Key, Map, Value, NewMap).

%% map_get(+Map, +Key, -Value).
%% map_get(+Map, +Key, +Value).
%
% Succeeds if the Key-Value pair is stored in Map.
% Fails silently if Key is not associated.
map_get(Map, Key, Value) :-
    Stored = Map.get(Key),
    ( ground(Value), ! % Protect internal value to not unify with output.
    , Stored == Value
    ; Stored = Value).

%% map_get(+Map, +Key, +Default, -Value).
%
% Access the value stored for Key in Map.
% If Key is not associated in Map, Value is unified with the Default.
map_get(Map, Key, _, Value) :-
    Value=Map.get(Key),
    !. % red cut
map_get(_, _, Default, Default).

%% map_dissoc(+Map, +Key, -NewMap).
%
% Dissociate Key from Map.
% NewMap no longer has a value for Key associated.
map_dissoc(Map, Key, NewMap) :-
    del_dict(Key, Map, _, NewMap).

%% map_keys(+Map, -Keys).
%
% Returns a list of keys stored in the map.
map_keys(Map, Keys) :-
    map_pairs(Map, Pairs),
    pairs_keys(Pairs, Keys).

%% map_values(+Map, -Values).
%
% Returns a list of values stored in the map.
map_values(Map, Values) :-
    map_pairs(Map, Pairs),
    pairs_values(Pairs, Values).

%% map_pairs(+Map, -Pairs).
%
% Returns a list of Key-Value pairs (separated by minus K-V) stored in the map.
map_pairs(Map, Pairs) :-
    dict_pairs(Map, map, Pairs).
