-module(http).
-export([parse_request/1, get/1, ok/1, not_found/0]).

parse_request(R0) ->
    {Request, R1} = request_line(R0),
    {Headers, R2} = headers(R1),
    {Body, _} = message_body(R2),
    {Request, Headers, Body}.

request_line([$G, $E, $T, 32 | R0]) ->
    {URI, R1} = request_uri(R0),
    {Ver, R2} = http_version(R1),
    [13,10|R3] = R2, % \r\n
    {{get, URI, Ver}, R3}.

request_uri([32|R0]) ->
    {[], R0};
request_uri([C|R0]) ->
    {Rest, R1} = request_uri(R0),
    {[C|Rest], R1}.

http_version([$H, $T, $T, $P, $/, $2, $., $0 | R0]) ->
    {v20, R0};
http_version([$H, $T, $T, $P, $/, $1, $., X | R0]) ->
    case X of
        $0 -> {v10, R0};
        $1 -> {v11, R0}
    end.

headers([13,10|R0]) ->
    {[], R0};
headers(R0) ->
    {Header, R1} = header(R0),
    {Rest, R2} = headers(R1),
    {[Header|Rest], R2}.

header([13,10|R0]) ->
    {[], R0};
header([C|R0]) ->
    {Rest, R1} = header(R0),
    {[C|Rest], R1}.

message_body(R) ->
    {R, []}.

ok(Body) ->
    ok(Body, "").

ok(Body, Headers) ->
    "HTTP/1.1 200 OK\r\n" 
    ++ Headers
    ++ server_headers() 
    ++ "\r\n" 
    ++ to_list(Body).

to_list(Body) when is_binary(Body) ->
    binary_to_list(Body);
to_list(Body) -> Body.

not_found() ->
    "HTTP/1.1 404 Not Found\r\n" ++ server_headers() ++ "\r\n 404 Not found".

to_client(ResponseCode, Headers, Body) ->
    ResponseCode ++ "\r\n" ++ Headers ++ "\r\n" ++ Body.

server_headers() ->
    "Server: rudy 2015-03-10\r\n".

get(URI) ->
    "GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".

