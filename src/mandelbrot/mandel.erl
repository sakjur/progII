-module(mandel).
-export([mandelbrot/6, demo/0, small_server/3]).

mandelbrot(Width, Height, X, Y, K, Depth) ->
    Trans = fun(W, H) ->
                cmplx:new(X + K*(W-1), Y-K*(H-1))
            end,
    rows(Width, Height, Trans, Depth, self()),
    collect(Height, []).

demo() ->
    small(0.2, 0.6, 0.05).
    %small(0.25, 0.001, 0.253). 

collect(0, Array) -> Array;
collect(Height, Array) ->
    receive
        {Height, Row} -> 
            NewArray = [Row | Array],
            case Height rem 10 of
                0 -> io:format("Ding ~w~n", [Height]);
                _ -> ok
            end,
            collect(Height-1, NewArray)
    end.

small(X, Y, X1) ->
    Width = 1920,
    Height = 1080,
    image(X, Y, X1, Width, Height).

small_server(X, Y, X1) ->
    Width = 1920,
    Height = 1080,
    K = (X1 - X)/Width,
    Depth = 2048,
    {ok, Server} = server:start(Width, Height, X, Y, K, Depth, "small.ppm"),
    register(server, Server).


image(X, Y, X1, Width, Height) -> 
    K = (X1 - X)/Width,
    Depth = 4096,
    T0 = now(),
    Image = mandelbrot(Width, Height, X, Y, K, Depth),
    T = timer:now_diff(now(), T0),
    io:format("picture generated in ~w ms~n", [T div 1000]),
    ppm:write("small.ppm", Image).

rows(_, 0, _, _, _) -> done;
rows(Width, Height, Trans, Depth, Ctrl) ->
    spawn_link(fun() -> report_row(Width, Height, Trans, Depth, Ctrl) end),
    rows(Width, Height-1, Trans, Depth, Ctrl).

report_row(W, H, Tr, Depth, Ctrl) ->
    Ctrl ! {H, new_row(W, H, Tr, Depth, [])}.

new_row(0, _, _, _, Array) -> Array;
new_row(Width, Height, Trans, Depth, Array) ->
    C = Trans(Width, Height),
    D = brot:mandelbrot(C, Depth),
    Color = color:convert(D, Depth),
    new_row(Width - 1, Height, Trans, Depth, [Color | Array]).

