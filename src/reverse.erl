-module(reverse).
-export([bench/0, heavy_bench/0]).

nreverse([]) ->
    [];
nreverse([H|T]) ->
    R = nreverse(T),
    lists:append(R, [H]).

reverse(L) ->
    reverse(L, []).
 
reverse([], R) ->
    R;
reverse([H|T], R) ->
    reverse(T, [H|R]).

% Reverses a list
myreverse([]) ->
    [];
myreverse([Head | Tail]) ->
    reverse(Tail) ++ [Head].

bench() ->
    Ls = [16, 32, 64, 128, 256, 512, 1024, 2048],
    N = 50,
    Bench = fun(L) ->
        S = lists:seq(1,L),
        Tn = time(N, fun() -> nreverse(S) end),
        Tr = time(N, fun() -> reverse(S) end),
        Tm = time(N, fun() -> myreverse(S) end),
        io:format("length: ~10w nrev: ~8w us rev: ~8w us myrev: ~8w us~n", [L, Tn,
        Tr, Tm])
        end,
    lists:foreach(Bench, Ls).

heavy_bench() ->
    Ls = [20000,50000,100000,500000,1000000,5000000,10000000],
    N = 50,
    Bench = fun(L) ->
        S = lists:seq(1,L),
        Tr = time(N, fun() -> reverse(S) end),
        Tm = time(N, fun() -> myreverse(S) end),
        io:format("length: ~10w rev: ~8w us myrev: ~8w us~n", [L, 
        Tr, Tm])
        end,
    lists:foreach(Bench, Ls).

time(N, F)->
    %% time in micro seconds
    T1 = now(),
    loop(N, F),
    T2 = now(),
    timer:now_diff(T2, T1).

loop(N, Fun) ->
    if N == 0 -> ok; true -> Fun(), loop(N-1, Fun) end.

