%% Author: majelbstoat
%% Created: Feb 2, 2011
%% Description: An erlang implementation of Okasaki's Random Access Lists.
%%
%% http://portal.acm.org/citation.cfm?id=224187
%%
%% Random Access Lists offer O(log n) access to the nth item in a list, as well as O(1) head and cons operations.
-module(ral).

%%
%% Include files
%%
%-type ral_node() :: {any(), ral_node(), ral_node()}.
%-type ral_tree() :: {pos_integer(), ral_node()}.
%-type ral() :: [ral_tree(),...].


%%
%% Exported Functions
%%
-export([
    % Simple list operations.
    new/0,
    head/1,
    tail/1,
    cons/2,
    length/1,
    reverse/1,

    % Iterative functions.
    map/2,
    foldl/3,
    foldr/3,

    % Random access functions.
    nth/2,
    update/3,

    % Conversion functions.
    ral_to_list/1
]).

%%
%% API Functions
%%
new() ->
    [].

% Returns the item at the head of the list.
head([{1, Value} | _]) ->
    Value;
head([{_, {Value, _, _}} | _]) ->
    Value.

tail([{1, _} | Tail]) ->
    Tail;
tail([{Size, {_, Left, Right}} | Tail]) ->
    NewSize = (Size - 1) bsr 1,
    [{NewSize, Left}, {NewSize, Right} | Tail].

% Appends an item to the front of the list.
cons(Value, [{Size, TreeFirst}, {Size, TreeSecond} | Tail]) ->
    [{Size + Size + 1, {Value, TreeFirst, TreeSecond}} | Tail];
cons(Value, List) ->
    [{1, Value} | List].

% Iterates through a random access list, calling the supplied function on the output.
map(_, []) ->
    % No more trees to search.
    [];
map(Function, [{Size, Tree} | Tail]) ->
    % Map this tree.
    [{Size, map(Function, Tree)} | map(Function, Tail)];
map(Function, {Value, Left, Right}) ->
    % Apply the function to this value and then map the left and right branches.
    {Function(Value), map(Function, Left), map(Function, Right)};
map(Function, Value) ->
    % Leaf node, so just apply the function.
    Function(Value).

% Standard foldl.
foldl(_, Accumulator, []) ->
    Accumulator;
foldl(Function, Accumulator, [{_, Tree} | Tail]) ->
    ral:foldl(Function, ral:foldl(Function, Accumulator, Tree), Tail);
foldl(Function, Accumulator, {Value, Left, Right}) ->
    % Current node first, then left node, then right node.
    ral:foldl(Function, ral:foldl(Function, Function(Value, Accumulator), Left), Right);
foldl(Function, Accumulator, Value) ->
    Function(Value, Accumulator).

% Standard foldr.
foldr(_, Accumulator, []) ->
    Accumulator;
foldr(Function, Accumulator, [{_, Tree} | Tail]) ->
    ral:foldr(Function, ral:foldr(Function, Accumulator, Tail), Tree);
foldr(Function, Accumulator, {Value, Left, Right}) ->
    % Right node first, then left node, then current node.
    Function(Value, ral:foldr(Function, ral:foldr(Function, Accumulator, Right), Left));
foldr(Function, Accumulator, Value) ->
    Function(Value, Accumulator).

% Finds the nth value.  Crashes if N is larger than the list length.
nth(N, [{Size, Tree} | _]) when N =< Size ->
    % The item is within this tree, so search through that.
    nth(N, Size, Tree);
nth(N, [{Size, _} | Tail]) ->
    % The item is not within this tree, so move to the next.
    nth(N - Size, Tail).

nth(1, _, {Value, _, _}) ->
    % Value found at a node.
    Value;
nth(1, _, Value) ->
    % Value found at a leaf.
    Value;
nth(N, Size, {_, Left, Right}) ->
    % Determine which branch to check next, remembering that we're 1-indexed, not 0-indexed.
    ShiftedSize = Size bsr 1,
    case N =< (ShiftedSize + 1) of
        true ->
            nth(N - 1, ShiftedSize, Left);
        false ->
            nth(N - 1 - ShiftedSize, ShiftedSize, Right)
    end.

% Updates the nth item with the supplied replacement. O(log N).
update(N, Replacement, [{Size, Tree} | Tail]) when N =< Size ->
    % Replacing in this tree.
    [{Size, update(N, Size, Replacement, Tree)} | Tail];
update(N, Replacement, [{Size, Tree} | Tail]) ->
    % Replacing some where in the tail.
    [{Size, Tree} | update(N - Size, Replacement, Tail)].

update(1, _, Replacement, {_, Left, Right}) ->
    % Replacing at a node.
    {Replacement, Left, Right};
update(1, _, Replacement, _) ->
    % Replacing at a leaf.
    Replacement;
update(N, Size, Replacement, {Value, Left, Right}) ->
    % Determine which brach to check next.
    ShiftedSize = Size bsr 1,
    case N =< (ShiftedSize + 1) of
        true ->
            {Value, update(N - 1, ShiftedSize, Replacement, Left), Right};
        false ->
            {Value, Left, update(N - 1 - ShiftedSize, ShiftedSize, Replacement, Right)}
    end.


% Convert a random access list to a regular list.
ral_to_list(RAL) ->
    ral:foldr(fun(Value, List) -> [Value | List] end, [], RAL).

% Reverses a random access list.
reverse(RAL) ->
    ral:foldl(fun(Value, List) -> ral:cons(Value, List) end, [], RAL).

% Returns the length of a list by summing all the sizes of the constituent trees.
length(List) ->
    lists:foldr(fun({Size, _}, Length) -> Size + Length end, 0, List).

