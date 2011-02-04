% Simple set class, implemented as an unbalanced binary search tree.
% Degenerate lookup case is O(n) for sequentially inserted data,
% but for randomised data is will be approximately O(n / 2)
-module(finite_map).

-export([
    new/0,
    bind/3,
    member_knuth/2,
    member_andersson/2
]).

% Create a new unbalanced binary search tree.
new() -> 
    null.

% binds an item.
bind(Key, Item, null) ->
    {null, {Key, Item}, null};
bind(Key, Item, {Left, {Value, Data}, Right}) when Key < Value ->
    {bind(Key, Item, Left), {Value, Data}, Right};
bind(Key, Item, {Left, {Value, Data}, Right}) when Key > Value ->
    {Left, {Value, Data}, bind(Key, Item, Right)};
bind(Key, Item, {Left, {Key, _}, Right}) ->
    {Left, {Key, Item}, Right}.

% Tests for membership of an item using Knuth's algorithm. Up to 3 comparisons at each node.
member_knuth(_, null) ->
    false;
member_knuth(Key, {_, {Key, _}, _}) ->
    true;
member_knuth(Key, {Left, {Value, _}, _}) when Key < Value ->
    member_knuth(Key, Left);
member_knuth(Key, {_, _, Right}) ->
    member_knuth(Key, Right).

% Tests for membership of an item. Uses depth + 1 comparisons.
% Andersson, Arne, "A Note on Searching a Binary Search Tree", 
% Software — Practice and Experience, Vol 21, No. 10, October 1991, p1125-1128.
member_andersson(Key, List) ->
    member_andersson(Key, List, null).

member_andersson(Key, null, Candidate) ->
    % Reached a leaf, test the candidate.
    Key == Candidate;
member_andersson(Key, {Left, {Value, _}, _}, Candidate) when Key < Value ->
    member_andersson(Key, Left, Candidate);
member_andersson(Key, {_, {Value, _}, Right}, _) ->
    % When taking the right branch, store a new candidate.
    member_andersson(Key, Right, Value). 
