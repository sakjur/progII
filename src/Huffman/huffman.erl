-module(huffman).

-export([sample/0, text/0, test/0, table/1, encode/2, decode/2]).
-export([timertest/0]).

% sample() -> "the quick brown fox jumps over the lazy dog
% is sample text that we will use when we build
% up a table we will only handle lower case letters and
% no punctuation symbols the frequency will of course not
% represent english but it is probably not that far off".
% 
% text() -> "this is something that we should encode".

sample() -> {ok, Binary} = file:read_file("pg1.txt"), 
    lists:seq(1, 255) ++ binary_to_list(Binary).

text() -> {ok, Binary} = file:read_file("pg1.txt"), binary_to_list(Binary).

test() -> 
    Sample = sample(),
    Table = table(Sample),
    Text = text(),
    Seq = encode(Text, Table),
    Text = decode(Seq, Table),
    {length(Seq), length(Text) * 8}.

timertest() ->
    Sample = sample(),
    Text = text(),
    {TimeTable, Table} = timer:tc(huffman, table, [Sample]),
    {TimeSeq, Seq} = timer:tc(huffman, encode, [Text, Table]),
    {TimeText, Text} = timer:tc(huffman, decode, [Seq, Table]), 
    {tableTime, TimeTable, encodeTime, TimeSeq, decodeTime, TimeText, 
        length(Seq) div 8, length(Text)}.

table(_Sample) ->
    FreqTable = freq(_Sample),
    Tree = huffman(FreqTable),
    Table = huffman_traverse(Tree),
    sort(Table).

encode([], _) -> [];
encode([Head | Tail], Table) -> 
    Value = binary_search(Table, Head),
    Value ++ encode(Tail, Table).

decode([], _) -> [];
decode(Seq, Table) -> 
    {Char, Rest} = decode_char(Seq, 1, Table),
    [Char | decode(Rest, Table)].

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
huffman(X) ->
    case X of
        [_, _ | _] ->
            [Fst, Snd | Tail] = X,
            NodeFreq = get_freq(Fst) + get_freq(Snd),
            huffman(
                insert_in_pos(Tail, {node, na, NodeFreq, Fst, Snd})
            );
        [_] -> X
    end.

insert_in_pos([Head | Tail], Node) ->
    HeadFreq = get_freq(Head),
    NodeFreq = get_freq(Node),
    if
        HeadFreq  < NodeFreq -> [Head | insert_in_pos(Tail, Node)];
        HeadFreq >= NodeFreq -> [Node, Head | Tail]
    end;
insert_in_pos([], Node) -> [Node].

% Traversal of the tree
huffman_traverse([]) -> [];
huffman_traverse([X]) -> 
    huffman_traverse(X, []).

huffman_traverse({node, na, _, Left, Right}, Path) ->
    huffman_traverse(Left, Path ++ [<<0:1>>])
    ++
    huffman_traverse(Right, Path ++ [<<1:1>>]);
huffman_traverse({node, Char, _, na, na}, Path) -> [{Char, Path}].

% Extract frequency of the node
get_freq({node, _, Freq, _, _}) -> Freq.

% Frequencies used to produce a Huffman-table
freq(Input) ->
    freq(Input, []).
freq([Head | Tail], List) ->
    freq(Tail, List ++ [Head]);
freq([], List) -> sort(List).

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

% QuickSort
sort([]) -> [];
sort(X) ->
    {Smaller, Equal, Larger} = compare(X),
    sort(Smaller) ++ Equal ++ sort(Larger).

% Comparator
compare([{node, Value, Head, L, R} | Tail]) ->
    Smaller = [{node, Char, Freq, L2, R2} ||
        {node, Char, Freq, L2, R2} <- Tail, Freq =< Head],
    Equal = {node, Value, Head, L, R},
    Larger  = [{node, Char, Freq, L2, R2} ||
        {node, Char, Freq, L2, R2} <- Tail, Freq > Head],
    {Larger, [Equal], Smaller};
compare([{Fst, Snd} | Tail]) ->
    Smaller = [{X, Y} || {X, Y} <- Tail, X < Fst],
    Larger = [{X, Y} || {X, Y} <- Tail, X >= Fst],
    {Smaller, [{Fst, Snd}], Larger};
compare([Head | Tail]) ->
    Smaller = [X || X <- Tail, X < Head],
    Equal = [1 || X <- Tail, X == Head],
    Larger = [X || X <- Tail, X > Head],
    {Smaller, [{node, Head, length(Equal) + 1, na, na}], Larger}.

