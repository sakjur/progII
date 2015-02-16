-module(brot).
-export([mandelbrot/2]).

mandelbrot(C, M) ->
    {Real, Complex} = C,
    Z0 = cmplx:new(Real, Complex),
    I = 0,
    test(I, Z0, Z0, M).

test(I, Zn, C, M) ->
    Val = checkN(I, Zn, C, M),
    case Val of
        0 -> 0;
        _ -> M - Val
    end.

checkN(_, _, _, 0) -> 0;
checkN(I, Zn, C, M) ->
    Zm = cmplx:add(cmplx:sqr(Zn), C),
    AbsZn = cmplx:abs(Zm),
    if
        AbsZn >= 2 ->
            M;
        true ->
            checkN(I, Zm, C, M-1)
    end.

