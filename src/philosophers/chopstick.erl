-module(chopstick).
-export([start/0, request/1, return/1, quit/1, granted/0]).

start() ->
    spawn_link(fun() -> available() end).

available() ->
    receive
        {request, From} ->
            From ! {granted, self()},
            gone(); 
        return ->
            available();
        quit ->
            ok
    end.

gone() ->
    receive
        return ->
            available();
        {request, _} ->
            gone(); 
        quit ->
            ok
    end.

request(Stick) ->
    Stick ! {request, self()}.

granted() ->
    receive
        {granted, Value} ->
            {ok, Value}
    after 500 ->
            no 
    end.

return(Stick) ->
    Stick ! return,
    ok.

quit(Stick) ->
    Stick ! quit.

