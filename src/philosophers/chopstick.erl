-module(chopstick).
-export([start/0, request/1, return/1, exit/1]).

start() ->
    spawn_link(fun() -> available() end).

available() ->
    receive
        {request, From} ->
            From ! granted,
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
        granted ->
            ok
    after 1000 ->
            no 
    end.

return(Stick) ->
    Stick ! return,
    ok.

exit(Stick) ->
    Stick ! quit.

