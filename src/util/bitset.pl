:- module(bitset, [new_bitset/2,
                   set_bit/2,
                   bit_is_set/2]).

%% new_bitset(+Size, -Bitset).
%
% Create a term bitset/n with variables as arguments.
new_bitset(Size, Bitset) :-
    functor(Bitset, bitset, Size).

%% set_bit(+Bitset, +N) :-
%
% Set the n-th bit of the bitset to 1.
set_bit(Bitset, N) :-
    arg(N, Bitset, 1).

%% bit_is_set(+Bitset, +N).
%
% True if the n-th bit of the bitset is 1.
bit_is_set(Bitset, N) :-
    arg(N, Bitset, Bit),
    Bit == 1.