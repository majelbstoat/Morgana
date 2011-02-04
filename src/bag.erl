% Simple bag module, implemented as an unbalanced binary search tree.
-module(unbset).

-export([
    new/0,
    new_of_size/2,
    new_of_depth/2
]).

% Create a new unbalanced binary search tree.
new() -> 
    null.

% Create a new binary search tree of the given depth, 
% such that each node has the initial value of Element. O(Depth).
new_of_depth(Element, 1) ->
    {null, Element, null};
new_of_depth(Element, Depth) ->
    Next = new_of_depth(Element, Depth - 1),
    {Next, Element, Next}.
   
new_of_size(Element, 1) ->
    {null, Element, null};
new_of_size(Element, 2) ->
    {Element, Element, null};
new_of_size(Element, N) ->
    case N band 1 of
        1 ->
            % N is odd.
            Left = Right = new_of_size(Element, N bsr 1);
        0 ->
            % N is even.
            Split = N bsr 1,
            Left = new_of_size(Element, Split),
            Right = new_of_size(Element, Split - 1)
    end,
    {Left,Element,Right}.
