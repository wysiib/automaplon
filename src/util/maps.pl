:- module(maps,
          [ empty_map/1,
            map_assoc/4,
            map_dissoc/3,
            map_get/3,
            map_get/4
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
