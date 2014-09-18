-module(sort).
-export([insert/2, isort/1, isort/2, msort/1]).
-export([qsort/1]).

insert(Element, []) ->
    [Element];
insert(Element, List) ->
    [Head | Tail] = List,

    if
        Element > Head -> [Head] ++ insert(Element, Tail);
        Element =< Head -> [Element] ++ List;
        true -> error
    end.

isort([]) ->
    [];
isort(List) ->
    isort(List, []).

isort([], Sorted) ->
    Sorted;
isort(List, Sorted) ->
    [Head | Tail] = List,
    NSort = insert(Head, Sorted),
    isort(Tail, NSort).

% [FIXME] I'm pretty sure this isn't a correct msort
msort([]) ->
    [];
msort([A]) ->
    [A];
msort([A, B]) ->
    if
        A >= B -> [B, A];
        B > A -> [A, B]
    end;
msort(List) ->
    {Lower, Upper} = msplit(List, [], []),
    lists:merge(msort(Lower), msort(Upper)).

msplit([], Lower, Upper) ->
    {Lower, Upper};
msplit([Last], LowPart, Upper) ->
    Lower = LowPart ++ [Last],
    msplit([], Lower, Upper);
msplit(List, LowPart, HighPart) ->
    [HeadL | [HeadH | Tail]] = List,
    Lower = LowPart ++ [HeadL],
    Upper = HighPart ++ [HeadH],
    msplit(Tail, Lower, Upper).

qsort([]) ->
    [];
qsort(List) ->
    [Head | Tail] = List,
    Larger = [X || X <- Tail, X > Head],
    Smaller = [X || X <- Tail, X < Head],
    Equal = [X || X <- List, X == Head],
    qsort(Smaller) ++ Equal ++ qsort(Larger).
