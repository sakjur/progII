-module(cmplx).
-export([new/2, add/2, sqr/1, abs/1]).

new(X, Y) ->
    {cmplx, X, Y}.

add({cmplx, AReal, ACmplx}, {cmplx, BReal, BCmplx}) ->
    {cmplx, AReal + BReal, ACmplx + BCmplx}. 

sqr({cmplx, Real, Cmplx}) ->
    {cmplx, (Real*Real) - (Cmplx*Cmplx), 2*Real*Cmplx}.

abs({cmplx, Real, Cmplx}) ->
    AbsSquared = (Real*Real)+(Cmplx*Cmplx),
    math:sqrt(AbsSquared).

