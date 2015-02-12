-module(chopstick).
-export([start/0, request/1, return/1, exit/1, granted/0]).

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
    Stick ! {request, self()}.

granted() ->
    receive
        {granted, Value} ->
            {ok, Value}
    after 50 ->
            no 
    end.

return(Stick) ->
    Stick ! return,
    ok.

exit(Stick) ->
    Stick ! quit.

