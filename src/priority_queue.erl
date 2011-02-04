% Simple priority queue implemented with a leftist heap.
-module(priority_queue).

-export([
    new/0,
    new/1,
    insert/2,
    merge/2,
    minimum/1,
    cons/1,
    tail/1,
    list_to_priority_queue/1,
    pop/1
]).

% Internally, this is represented by nested tuple.
% Each node contains its rank, the data and a left and right branch.
% The tree itself is represented by a root node and a comparator function.

new() ->
    {null, fun(X) -> X end}.

new(ComparativeFunction) ->
    {null, ComparativeFunction}.

insert(Data, {PriorityQueueData, ComparativeFunction}) ->
    CompareValue = ComparativeFunction(Data),
    {insert(1, Data, CompareValue, PriorityQueueData), ComparativeFunction}.

insert(Rank, Data, CompareValue, null) ->
    {Rank, Data, CompareValue, null, null};
insert(Rank, Data, CompareValue, Node) ->
    merge({Rank, Data, CompareValue, null, null}, Node).


merge(First, null) ->
    First;
merge(null, Second) ->
    Second;
merge(First, Second) ->
    {_, FirstData, FirstCompareValue, FirstLeft, FirstRight} = First, 
    {_, SecondData, SecondCompareValue, SecondLeft, SecondRight} = Second,
    case FirstCompareValue =< SecondCompareValue of
        true ->
            make_tree(FirstData, FirstCompareValue, FirstLeft, merge(FirstRight, Second));
        false ->
            make_tree(SecondData, SecondCompareValue, SecondLeft, merge(First, SecondRight))
    end.

rank(null) ->
    0;
rank({Rank, _, _, _, _}) ->
    Rank.

make_tree(Data, CompareValue, Left, Right) ->
    LeftRank = rank(Left),
    RightRank= rank(Right),
    case LeftRank >= RightRank of
        true ->
            {RightRank + 1, CompareValue, Data, Left, Right};
        false ->
            {LeftRank + 1, CompareValue, Data, Right, Left}
    end.


cons({{_, Data, _, _, _}, _}) ->
    Data.

tail({{_, _, _, Left, Right}, ComparativeFunction}) ->
    {merge(Left, Right), ComparativeFunction}.

% Returns the head and tail together.
pop({{_, Data, _, Left, Right}, ComparativeFunction}) ->
    {Data, {merge(Left, Right), ComparativeFunction}}.

list_to_priority_queue(List) ->
    list_to_priority_queue(List, new()).

list_to_priority_queue([], Result) ->
    Result;
list_to_priority_queue([Head | Tail], Result) ->
    list_to_priority_queue(Tail, insert(Head, Result)).

minimum(PriorityQueue) ->
    cons(PriorityQueue).

