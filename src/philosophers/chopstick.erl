-module(chopstick).
-export([start/0, request/1, return/1, quit/1]).

start() ->
    spawn_link(fun() -> available() end).

available() ->
    receive
        {request, From} ->
            From ! {granted, self()},
            gone(); 
        quit ->
            ok
    end.

gone() ->
    receive
        return ->
            available();
        quit ->
            ok
    end.

request(Stick) ->
    Stick ! {request, self()},
    receive
        {granted, _} ->
            ok
    after 1000 ->
            no 
    end.

return(Stick) ->
    Stick ! return,
    ok.

quit(Stick) ->
    Stick ! quit.

