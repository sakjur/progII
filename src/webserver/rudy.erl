-module(rudy).
-export([init/1]).

init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            handler(Listen),
            gen_tcp:close(Listen),
            ok;
        {error, _Error} ->
            error
    end.

handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            request(Client),
            handler(Listen);
        {error, _Error} ->
            error
    end.

request(Client) ->
    spawn(fun() ->
        Recv = gen_tcp:recv(Client, 0),
        case Recv of
            {ok, Str} ->
                Request = http:parse_request(Str),
                Response = reply(Request),
                gen_tcp:send(Client, Response);
            {error, Error} ->
                io:format("rudy: error ~w~n", [Error])
        end,
        gen_tcp:close(Client)
    end),
    ok.

reply({{get, [$/ | URI], _}, _, _}) ->
    timer:sleep(20),
    case file:read_file("content/" ++ URI) of
        {ok, Entry} -> http:ok(Entry);
        {error, eisdir} -> 
            case file:read_file("content" ++ URI ++ "/index.html") of
                {ok, Entry} -> http:ok(Entry);
                {error, _} -> http:not_found()
            end;
        {error, _} -> http:not_found()
    end.

