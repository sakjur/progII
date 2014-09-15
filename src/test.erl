-module(test).
-export([double/1, ftocelsius/1]).
-export([rectangle/2, square/1, circle/1]).
-export([product/2, exp/2, fast_exp/2]).
-export([nth/2, number/1, sum/1, duplicate/1]).
-export([unique/1, reverse/1, pack/1]).

% Doubles a number
double(A) ->
    A*2.

% Converts from fahrenheit to celsius
ftocelsius(F) ->
    (F-32)/1.8.

% The area of a rectangle
rectangle(Height, Width) ->
    Height*Width.

% The area of a square
square(Side) ->
    rectangle(Side, Side).

% The area of a circle
circle(Radius) ->
    Pi = 3.141592,
    Radius*Radius*Pi.

% The product of M times N
product(M, N) ->
    case M of 
        0 -> 0;
        _ -> N + product(M-1, N)
    end.

% A naïve power function
exp(X, Y) ->
    case Y of
        0 -> 1;
        _ -> X * exp(X, Y-1)
    end.

% A faster power function
fast_exp(X, Y) ->
    case Y of
        0 -> 1;
        1 -> X;
        _ when Y rem 2 == 0 -> 
            Tmp = fast_exp(X, Y div 2),
            Tmp*Tmp;
        _ when Y rem 2 == 1 ->
            X * fast_exp(X, Y-1)
    end.

% Finds the nth object in a list. One-indexed 
% (because first makes more sense than zeroeth)
nth(0, _) ->
    error;
nth(1, L) ->
    [Head | _] = L,
    Head;
nth(N, L) ->
    [_ | Tail] = L,
    nth(N-1, Tail).

% Sums the amount of elements in a list
number([]) ->
    0;
number([_ | Tail]) ->
    1 + number(Tail).

% Sums the elements of a list
sum([]) ->
    0;
sum([Head | Tail]) ->
    Head + sum(Tail).

% Returns a list where all the elements are multiplied by two. 
duplicate([]) ->
    [];
duplicate([Head | Tail]) ->
    [Head * 2 | duplicate(Tail)].

% Removes duplicates in a list
unique([]) ->
    [];
unique([Head | Tail]) ->
    [Head | unique(removeAllOf(Head, Tail))].

% Reverses a list
reverse([]) ->
    [];
reverse([Head | Tail]) ->
    reverse(Tail) ++ [Head].

pack([]) ->
    [];
pack(L) ->
    [Head | _] = L,
    [removeAllNotOf(Head, L) | pack(removeAllOf(Head, L))].

removeAllNotOf(_, []) ->
    [];
removeAllNotOf(Element, [Head | Tail]) ->
    if
        Head =/= Element -> removeAllNotOf(Element, Tail);
        true -> [Head | removeAllNotOf(Element, Tail)]
    end.

removeAllOf(_, []) ->
    [];
removeAllOf(Element, [Head | Tail]) ->
    if
        Head == Element -> removeAllOf(Element, Tail);
        true -> [Head | removeAllOf(Element, Tail)]
    end.

