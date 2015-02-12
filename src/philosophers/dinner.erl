-module(dinner).
-export([start/0, timedrun/0]).

timedrun() ->
    T1 = now(),
    loop(10, fun() -> init() end),
    T2 = now(),
    timer:now_diff(T2, T1).

loop(0, _) ->
    ok;
loop(N, Fun) ->
    Fun(),
    loop(N-1, Fun).

start() ->
    spawn(fun() -> init() end).

init() ->
    C1 = chopstick:start(),
    C2 = chopstick:start(),
    C3 = chopstick:start(),
    C4 = chopstick:start(),
    C5 = chopstick:start(),
    
    Ctrl = self(),
    philosopher:start(5, C1, C2, "Arendt", Ctrl),
    philosopher:start(5, C2, C3, "Hypatia", Ctrl),
    philosopher:start(5, C3, C4, "Simone", Ctrl),
    philosopher:start(5, C4, C5, "Elizabeth", Ctrl),
    philosopher:start(5, C5, C1, "Ayn", Ctrl),
    Chopsticks = [C1, C2, C3, C4, C5],
    wait(5, Chopsticks).

wait(0, Chopsticks) ->
    lists:foreach(fun(C) -> chopstick:quit(C) end, Chopsticks);
wait(N, Chopsticks) ->
    receive
        done ->
            wait(N-1, Chopsticks);
        abort ->
            exit(abort)
    end.

