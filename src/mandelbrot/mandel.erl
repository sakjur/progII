-module(mandel).
-export([mandelbrot/6, demo/0]).

mandelbrot(Width, Height, X, Y, K, Depth) ->
    Trans = fun(W, H) ->
                cmplx:new(X + K*(W-1), Y-K*(H-1))
            end,
    rows(Width, Height, Trans, Depth, []).

demo() ->
    small(-2.6, 1.2, 1.6).

small(X, Y, X1) ->
    Width = 960,
    Height = 540,
    K = (X1 - X)/Width,
    Depth = 64,
    T0 = now(),
    Image = mandelbrot(Width, Height, X, Y, K, Depth),
    T = timer:now_diff(now(), T0),
    io:format("picture generated in ~w ms~n", [T div 1000]),
    ppm:write("small.ppm", Image).

rows(_, 0, _, _, Array) -> Array;
rows(Width, Height, Trans, Depth, Array) ->
    NewArr = [new_row(Width, Height, Trans, Depth, []) | Array],
    rows(Width, Height-1, Trans, Depth, NewArr).

new_row(0, _, _, _, Array) -> Array;
new_row(Width, Height, Trans, Depth, Array) ->
    C = Trans(Width, Height),
    D = brot:mandelbrot(C, Depth),
    Color = color:convert(D, Depth),
    new_row(Width - 1, Height, Trans, Depth, [Color | Array]).

