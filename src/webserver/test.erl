-module(test).
-export([bench/1, bench/2, bench/3]).

bench(Port) ->
    Addr = {127, 0, 0, 1},
    bench(Addr, Port).
bench(Host, Port) ->
    bench(100, Host, Port).
bench(N, Host, Port) -> 
    bench(10, N, Host, Port, []).
bench(0, _, _, _, Arr) ->
    collector(Arr, []);
bench(C, N, Host, Port, Arr) ->
    Me = self(),
    Cli = spawn(fun() -> client_runner(Me, N, Host, Port) end),
    bench(C-1, N, Host, Port, [Cli | Arr]).

collector([H | T], Times) ->
    receive
        {time, H, Time} -> collector(T, [Time | Times])
    end;
collector([], Times) ->
    io:format("Max: ~ws Min: ~ws Median: ~ws~n",
        [lists:max(Times) / 1000000,
         lists:min(Times) / 1000000,
         lists:nth(length(Times) div 2, lists:sort(Times)) / 1000000]).

client_runner(Parent, N, Host, Port) ->
    Start = now(),
    run(N, Host, Port),
    Finish = now(),
    Time = timer:now_diff(Finish, Start),
    Parent ! {time, self(), Time}.

run(N, Host, Port) ->
    case N of
        0 -> ok;
        _ ->
            request(Host, Port),
            run(N-1, Host, Port)
    end.

request(Host, Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    Server = case gen_tcp:connect(Host, Port, Opt) of
        {ok, Str} -> Str;
        {error, _} -> error
    end,
    gen_tcp:send(Server, http:get("foo")),
    Recv = gen_tcp:recv(Server, 0),
    case Recv of
        {ok, _} ->
            ok;
        {error, Error} ->
            io:format("test: error: ~w~n", [Error])
    end,
    gen_tcp:close(Server).

