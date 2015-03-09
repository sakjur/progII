-module(color).
-export([convert/2]).

convert(Depth, Max) ->
    A = (4*Depth)/Max,
    X = trunc(A),
    Y = trunc(255*(A-X)),
    case X of
        0 -> {0, 0, Y};
        1 -> {0, Y, 255-Y};
        2 -> {0, 255-Y, 0};
        3 -> {0, Y, Y};
        4 -> {255-Y, 255-Y, 255-Y}
    end.

