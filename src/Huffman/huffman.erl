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
    sort_huffman(Table).

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
huffman_traverse([{node, _, Left, Right}]) -> 
    huffman_traverse(Left, [<<0:1>>]) ++ huffman_traverse(Right, [<<1:1>>]);
huffman_traverse({leaf, Char, _}) -> [{Char, []}].

huffman_traverse({node, _, Left, Right}, Traversal) ->
    huffman_traverse(Left, Traversal ++ [<<0:1>>])
    ++
    huffman_traverse(Right, Traversal ++ [<<1:1>>]);
huffman_traverse({leaf, Char, _}, Traversal) -> [{Char, Traversal}].

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
binary_search([], _) -> [];
binary_search(Array, Key) ->
    binary_search(Array, Key, 0, length(Array)).

binary_search(Array, Key, Lo, Hi) when Lo =< Hi ->
    Mid = Lo + (Hi - Lo) div 2,
    {MidElement, Traversal} = lists:nth(Mid, Array),
    if
       Key > MidElement -> binary_search(Array, Key, Mid + 1, Hi);
       Key < MidElement -> binary_search(Array, Key, Lo, Mid - 1);
       Key == MidElement -> Traversal
    end;
binary_search(_, _, _, _) -> {error, "Not found"}.

% QuickSort and Merge
% TODO Try to merge the sort-algorithms
% Could be done using a compare-function?
qsort_and_merge([]) -> [];
qsort_and_merge(Array) ->
    FrequencyArray = freq_sort(Array),
    sort_by_freq(FrequencyArray).

% Sorting that counts the frequency of the element 
freq_sort([]) -> [];
freq_sort([Head | Tail]) ->
    Larger = [X || X <- Tail, X > Head],
    Smaller = [X || X <- Tail, X < Head],
    Equal = [1 || X <- Tail, X == Head],
    freq_sort(Smaller)
        ++ [{leaf, Head, length(Equal) + 1}]
        ++ freq_sort(Larger).

% Sorts leafs by frequency, largest first
sort_by_freq([]) -> [];
sort_by_freq([{leaf, Value, Head} | Tail]) ->
    Larger  = [{leaf, Char, Freq} ||
        {leaf, Char, Freq} <- Tail, Freq > Head],
    Smaller = [{leaf, Char, Freq} ||
        {leaf, Char, Freq} <- Tail, Freq =< Head],
    sort_by_freq(Larger)
        ++ [{leaf, Value, Head}]
        ++ sort_by_freq(Smaller).

% Sorts {X, Y} by X, smallest first
sort_huffman([]) -> [];
sort_huffman([{Char, Trav} | Tail]) ->
    Smaller = [{X, Y} || {X, Y} <- Tail, X < Char],
    Larger = [{X, Y} || {X, Y} <- Tail, X >= Char],
    sort_huffman(Smaller) ++ [{Char, Trav}] ++ sort_huffman(Larger).

