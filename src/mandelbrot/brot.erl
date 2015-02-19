-module(brot).
-export([mandelbrot/2]).

mandelbrot({cmplx, Real, Complex}, M) ->
    mandelbrot({Real, Complex}, M);
mandelbrot(C, M) ->
    {Real, Complex} = C,
    Z0 = cmplx:new(Real, Complex),
    test(Z0, Z0, M).

test(Zn, C, M) ->
    Val = checkN(Zn, C, M),
    case Val of
        0 -> 0;
        _ -> M - Val
    end.

checkN(_, _, 0) -> 0;
checkN(Zn, C, M) ->
    Zm = cmplx:add(cmplx:sqr(Zn), C),
    AbsZn = cmplx:abs(Zm),
    if
        AbsZn >= 2 ->
            M;
        true ->
            checkN(Zm, C, M-1)
    end.

