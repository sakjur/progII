-module(huffman).

-export([sample/0, text/0, test/0, table/1, encode/2, decode/2]).

sample() -> "the quick brown fox jumps over the lazy dog
this is a sample text that we will use when we build
up a table we will only handle lower case letters and
no punctuation symbols the frequency will of course not
represent english but it is probably not that far off".

text() -> "this is something that we should encode".

test() -> 
    Sample = sample(),
    Table = table(Sample),
    Text = text(),
    Seq = encode(Text, Table),
    Text = decode(Seq, Table).

table(_Sample) ->
    FreqTable = freq(_Sample),
    Tree = huffman(FreqTable),
    Table = huffman_traverse(Tree),
    sort(Table).

encode([], _) -> [];
encode([_Head | _Tail], _Table) -> 
    Value = binary_search(_Table, _Head),
    Value ++ encode(_Tail, _Table).

decode([], _) -> [];
decode(Seq, Table) -> 
    {Char, Rest} = decode_char(Seq, 1, Table),
    [Char] ++ decode(Rest, Table).

decode_char(Seq, N, Table) ->
    {Code, Rest} = lists:split(N, Seq),
    case lists:keysearch(Code, 2, Table) of
        {value, {Char, _}} ->
            {Char, Rest};
        false ->
            decode_char(Seq, N+1, Table)
        end.

% Huffman tree creation
huffman([]) -> [];
huffman([Fst, Snd | Tail]) ->
    NodeFreq = get_freq(Fst) + get_freq(Snd),
    huffman([{node, NodeFreq, Fst, Snd}] ++ huffman(Tail));
huffman([E]) -> [E].

% Traversal of the tree
huffman_traverse([]) -> [];
huffman_traverse([X]) -> 
    huffman_traverse(X, []).

huffman_traverse({node, _, Left, Right}, Path) ->
    huffman_traverse(Left, Path ++ [<<0:1>>])
    ++
    huffman_traverse(Right, Path ++ [<<1:1>>]);
huffman_traverse({leaf, Char, _}, Path) -> [{Char, Path}].

% Extract frequency of the node or leaf
get_freq({node, Freq, _, _}) -> Freq;
get_freq({leaf, _, Freq}) -> Freq.

% Frequencies used to produce a Huffman-table
freq(Input) ->
    freq(Input, []).
freq([Head | Tail], List) ->
    freq(Tail, List ++ [Head]);
freq([], List) -> qsort_and_merge(List).

% Binary Search
% TODO Replace with a R/B tree or other effective datastructure
binary_search([], _) -> [];
binary_search(List, Key) ->
    Array = array:from_list(List), % Use an array as lists:nth is O(n)
    binary_search(Array, Key, 0, length(List)).

binary_search(Array, Key, Lo, Hi) when Lo =< Hi ->
    Mid = Lo + (Hi - Lo) div 2,
    {MidElement, Path} = array:get(Mid, Array),
    if
       Key > MidElement -> binary_search(Array, Key, Mid + 1, Hi);
       Key < MidElement -> binary_search(Array, Key, Lo, Mid - 1);
       Key == MidElement -> Path
    end;
binary_search(_, _, _, _) -> {error, "Not found"}.

% QuickSort and Merge
qsort_and_merge([]) -> [];
qsort_and_merge(Array) ->
    FrequencyArray = sort(Array),
    sort(FrequencyArray).

sort([]) -> [];
sort(X) ->
    {Smaller, Equal, Larger} = compare(X),
    sort(Smaller) ++ Equal ++ sort(Larger).

% Comparator
compare([{leaf, Value, Head} | Tail]) ->
    Smaller = [{leaf, Char, Freq} ||
        {leaf, Char, Freq} <- Tail, Freq =< Head],
    Equal = {leaf, Value, Head},
    Larger  = [{leaf, Char, Freq} ||
        {leaf, Char, Freq} <- Tail, Freq > Head],
    {Larger, [Equal], Smaller};
compare([{Fst, Snd} | Tail]) ->
    Smaller = [{X, Y} || {X, Y} <- Tail, X < Fst],
    Larger = [{X, Y} || {X, Y} <- Tail, X >= Fst],
    {Smaller, [{Fst, Snd}], Larger};
compare([Head | Tail]) ->
    Smaller = [X || X <- Tail, X < Head],
    Equal = [1 || X <- Tail, X == Head],
    Larger = [X || X <- Tail, X > Head],
    {Smaller, [{leaf, Head, length(Equal) + 1}], Larger}.

