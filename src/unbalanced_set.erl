% Simple set class, implemented as an unbalanced binary search tree.
-module(unbalanaced_set).

-export([
    new/0,
    insert/2,
    member_knuth/2,
    member_andersson/2
]).

% Create a new unbalanced binary search tree.
new() -> 
    null.

% Inserts an item.
insert(Item, null) ->
    {null, Item, null};
insert(Item, {Left, Value, Right}) when Item < Value ->
    {insert(Item, Left), Value, Right};
insert(Item, {Left, Value, Right}) when Item > Value ->
    {Left, Value, insert(Item, Right)}.

% Tests for membership of an item using Knuth's algorithm. Up to 3 comparisons at each node.
member_knuth(_, null) ->
    false;
member_knuth(Item, {_, Item, _}) ->
    true;
member_knuth(Item, {Left, Value, _}) when Item < Value ->
    member_knuth(Item, Left);
member_knuth(Item, {_, _, Right}) ->
    member_knuth(Item, Right).

% Tests for membership of an item. Uses depth + 1 comparisons.
% Andersson, Arne, "A Note on Searching a Binary Search Tree", 
% Software — Practice and Experience, Vol 21, No. 10, October 1991, p1125-1128.
member_andersson(Item, List) ->
    member_andersson(Item, List, null).

member_andersson(Item, null, Candidate) ->
    % Reached a leaf, test the candidate.
    Item == Candidate;
member_andersson(Item, {Left, Value, _}, Candidate) when Item < Value ->
    member_andersson(Item, Left, Candidate);
member_andersson(Item, {_, Value, Right}, _) ->
    % When taking the right branch, store a new candidate.
    member_andersson(Item, Right, Value). 
