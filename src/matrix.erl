% Matrix module.
-module(matrix).

-export([
    add/2,
    column/2,
    cumulative_maximum/1,
    cumulative_minimum/1,
    dimensions/1,
    element_at/3,
    element_count/1,
    element_set/4,
    identity/1,
    maximise_assignment/1,
    maximise_assignment_dynamic/1,
    multiply/2,
    new/1, 
    new/2, 
    new/3,
    power/2,
    random/3,
    row_sort/2,
    scalar_multiply/2,
    sequential/2,
    sum/1,
    row/2
]).


% Types
-type matrix() :: [[any(), ...]].
-type int_matrix() :: [[integer(), ...]].
-type num_matrix() :: [[number(), ...]].

% Convenience function to create a square matrix.
-spec new(integer()) -> matrix().
new(Size) ->
    new(Size, Size).

% Creates a square matrix with all elements initialised to 0, or the value returned by the specified generator function.
-spec new(pos_integer(), fun((pos_integer(), pos_integer(), pos_integer(), pos_integer()) -> any()) | pos_integer()) -> matrix().
new(Size, ContentGenerator) when is_function(ContentGenerator, 4) ->
    new(Size, Size, ContentGenerator);
new(Columns, Rows) ->
    % Creates a rectangular matrix initialised to 0.
    [[0 || _ <- lists:seq(1, Columns)] || _ <- lists:seq(1, Rows)].

% Creates a rectangular matrix using the specified function to generate elements.
-spec new(pos_integer(), pos_integer(), 
    fun((pos_integer(), pos_integer(), pos_integer(), pos_integer()) -> any())) -> matrix().
new(Columns, Rows, ContentGenerator) ->
    [[ContentGenerator(Column, Row, Columns, Rows) || Column <- lists:seq(1, Columns)] || Row <- lists:seq(1, Rows)].

% Convenience function to create a random matrix of specified dimensions, with an upper bound of MaxValue for elements.
-spec random(pos_integer(), pos_integer(), pos_integer()) -> int_matrix().
random(Columns, Rows, MaxValue) ->
    random:seed(erlang:now()),
    new(Columns, Rows, fun(_, _, _, _) -> random:uniform(MaxValue) end).

% Convenience function to create a sequential matrix where element values ascend.
-spec sequential(pos_integer(), pos_integer()) -> int_matrix().
sequential(Columns, Rows) ->
    new(Columns, Rows, fun(Column, Row, CS, _) -> CS * (Row - 1) + Column end).

% Convenience function creates an identity matrix of the supplied size.
-spec identity(pos_integer()) -> int_matrix().
identity(Size) ->
    new(Size, Size, fun(Column, Row, _, _) -> case Column of Row -> 1; _ -> 0 end end).

% Counts the elements in a matrix.
-spec element_count(matrix()) -> pos_integer().
element_count(Matrix) ->
    length(Matrix) * length(lists:nth(1, Matrix)).

% Returns the element of a matrix in the specified position.
-spec element_at(pos_integer(), pos_integer(), matrix()) -> any().
element_at(Column, Row, Matrix) ->
    lists:nth(Column, lists:nth(Row, Matrix)).

-spec element_set(pos_integer(), pos_integer(), any(), matrix()) -> matrix().
element_set(ElementColumn, ElementRow, Value, Matrix) ->
    {Width, Height} = matrix:dimensions(Matrix),
    matrix:new(Width, Height, fun(Column, Row, _, _) ->
            case (Column == ElementColumn) andalso (Row == ElementRow) of
                true ->
                    Value;
                false ->
                    matrix:element_at(Column, Row, Matrix)
            end
        end).

% Returns the specified row from the specified matrix.
-spec row(pos_integer(), matrix()) -> [any(), ...].
row(Row, Matrix) ->
    lists:nth(Row, Matrix).

% Returns the specified column from the specified matrix.
-spec column(pos_integer(), matrix()) -> [any(), ...].
column(Column, Matrix) ->
    [lists:nth(Column, Row) || Row <- Matrix].

% Returns a tuple containing the dimensions of the supplied matrix.
-spec dimensions(matrix()) -> {pos_integer(), pos_integer()}.
dimensions(Matrix) ->
    {length(lists:nth(1, Matrix)), length(Matrix)}.

% Returns the product of two matrices.
-spec multiply(num_matrix(), num_matrix()) -> num_matrix().
multiply(A, B) ->
    Count = length(lists:nth(1, A)),
    matrix:new(Count, length(A), fun(Column, Row, _, _) ->
            sum_product(Count, Column, Row, A, B)            
        end
    ).

% Auxiliary function to calculate the matrix product for a single element.
-spec sum_product(pos_integer(), pos_integer(), pos_integer(), num_matrix(), num_matrix()) -> number().
sum_product(Count, Column, Row, A, B) ->
    sum_product(Count, Column, Row, A, B, 0).

-spec sum_product(non_neg_integer(), pos_integer(), pos_integer(), num_matrix(), num_matrix(), number()) -> number().
sum_product(0, _, _, _, _, Total) ->
    Total;
sum_product(Count, Column, Row, A, B, Total) ->
    Product = matrix:element_at(Count, Row, A) * matrix:element_at(Column, Count, B),
    sum_product(Count - 1, Column, Row, A, B, Total + Product).

% Returns the matrix raised to the specified power.  Naive implementation that just
% recursively multiplies.
-spec power(num_matrix(), pos_integer()) -> num_matrix().
power(Matrix, 1) ->
    Matrix;
power(Matrix, N) ->
    power(Matrix, N - 1, multiply(Matrix, Matrix)).

-spec power(num_matrix(), pos_integer(), num_matrix()) -> num_matrix().
power(_, 1, Result) ->
    Result;
power(Matrix, N, Partial) ->
    power(Matrix, N - 1, multiply(Matrix, Partial)).

% Returns the given matrix with each element multiplied by the supplied integer.
-spec scalar_multiply(num_matrix(), integer()) -> num_matrix().
scalar_multiply(Matrix, 1) ->
    Matrix;
scalar_multiply(Matrix, Scalar) ->
    matrix:new(length(lists:nth(1, Matrix)), length(Matrix), fun(Column, Row, _, _) ->
            Scalar * element_at(Column, Row, Matrix)
        end
    ).

% Adds two matrices together.
-spec add(num_matrix(), num_matrix()) -> num_matrix().
add(A, B) ->
    matrix:new(length(lists:nth(1, A)), length(A), fun(Column, Row, _, _) ->
            element_at(Column, Row, A) + element_at(Column, Row, B)
        end
    ).

% Returns the sum of all the elements of a matrix.
-spec sum(num_matrix()) -> number().
sum(Matrix) ->
    lists:sum(lists:flatten(Matrix)).

% Given a numeric matrix, returns a list which is formed recursively from the 
% sum of the maximum value from each row up to an including this one.
-spec cumulative_maximum(num_matrix()) -> [number(), ...].
cumulative_maximum(Matrix) ->
    cumulative(Matrix, fun lists:max/1).

cumulative_minimum(Matrix) ->
    cumulative(Matrix, fun lists:min/1).

% Accumulates a matrix, given a specified accumulator function.
-spec cumulative(matrix(), fun(([number()]) -> number())) -> [number(),...].
cumulative(Matrix, Function) ->
    {Return, _} = lists:mapfoldl(fun(Row, Sum) ->
            % Add to the running total and store to running total for next iteration.
            Partial = Function(Row) + Sum,
            {Partial, Partial}
        end,
        0,
        Matrix
    ),
    Return.

% Sorts elements within matrix rows, row by row, using the supplied sorting function.
row_sort(Sorter, Matrix) ->
   [lists:sort(Sorter, Row) || Row <- Matrix]. 

% Given a matrix, maximises the total obtainable by taking at most one element
% from each column and each row (the assignment problem).  Returns the total
% and a list of column/row pairs used in the solution.
%
% See http://en.wikipedia.org/wiki/Assignment_problem
%
% This approach is brute force but with a good early termination heuristic
% determining whether it is possible to beat the highest number from this position.
% By choosing the highest value from each row deterministically, we maximise the 
% opportunities to terminate early. The work is distributed across N processes, with
% a central controller to track the best result found by each and update the workers.
%
% This completes in under 10 seconds for most matrices where N <= 30.  However, certain
% matrix layouts take a few minutes to solve completely.  This largely depends on the
% cumulative maximum versus the number of rows in the matrix.
-spec maximise_assignment(num_matrix()) -> {float(), [{integer(), integer()}]}.
maximise_assignment(Matrix) ->
    [_ | CumulativeMaximums] = lists:reverse(cumulative_maximum(lists:reverse(Matrix))),
    {Width, Height} = matrix:dimensions(Matrix),
    [Row | PreparedMatrix] = matrix:row_sort(fun({_,X}, {_,Y}) -> X > Y end,
        matrix:new(Width, Height, 
            fun(Column, Row, Columns, _) ->
                    {1 bsl (Columns - Column), matrix:element_at(Column, Row, Matrix)}
            end
        )
    ),
    
    parallelise_assignment(fun({Mask, Value}, ParentPid) ->
            maximise_assignment(PreparedMatrix, Mask, Value, 0, CumulativeMaximums, ParentPid)
        end,
        Row
    ).


update_best(BestSoFar) ->
    receive
        {new_minimum, BestFromController} ->
            case BestFromController > BestSoFar of
                true ->
                    %io:format("Pid ~p got new best ~p from controller~n", [self(), BestFromController]),
                    update_best(BestFromController);
                false ->
                    update_best(BestSoFar)
            end
    after 0 ->
            BestSoFar
    end.

maximise_assignment([], _, Total, BestSoFar, _, ParentPid) when Total > BestSoFar ->
    % We reached the end and this better than the best so far, so return that value.
    ParentPid ! {new_best, self(), Total},
    Total;
maximise_assignment([], _, _, BestSoFar, _, _) ->
    % We reached the end, but it's less than the best so far, so skip it.
    BestSoFar;
maximise_assignment([Row | Matrix], ColumnBitMask, Total, BestSoFar, [MaxFromHere | CumulativeMaximums], ParentPid) ->
    NewBest = update_best(BestSoFar),
    % Early Termination Test 1:
    % Do a quick check to see what the maximum possible is from this position,
    % assuming we took the maximum from each subsequent row.  (We know this
    % value because we pre-calculated cumulative maximums).  If we can't do
    % better than the best, we don't need to go any further along this branch.
    case (Total + MaxFromHere) =< NewBest of
        true ->
            % Can't do better than the best from here, so terminate early.
            NewBest;
        false ->
            % Otherwise, we have to keep going, so filter out those that already 
            % are used according to the column bitmask.
            SortedAvailableColumns = lists:filter(fun({Mask, _}) -> 
                    (Mask band ColumnBitMask) == 0
                end,
                Row
            ),

            lists:foldl(fun({Mask, Value}, BestSoFarIncrement) ->
                    PathAnswer = maximise_assignment(Matrix, ColumnBitMask bor Mask, Total + Value, BestSoFarIncrement, CumulativeMaximums, ParentPid),
                    case PathAnswer > BestSoFarIncrement of
                        true ->
                            % The path starting from this column is better than the
                            % best so far so store that result as the answer.
                            PathAnswer;
                        false ->
                            % The path starting from this column is less valuable 
                            % than the best so far, so stick with what we have.
                            BestSoFarIncrement
                    end
                end,
                NewBest,
                SortedAvailableColumns
            )
    end.

parallelise_assignment(Function, List) ->
    % Distributes mapping function to multiple child processes,
    % then gathers the results and returns the values. After code in the 
    % Erlang book by Joe Armstrong.
    Self = self(),
    Pids = lists:map(fun(Item) ->
        spawn(fun() -> execute(Self, Function, Item) end)
    end, List),
    assignment_results_gather(Pids).

execute(Parent, Function, Item) ->
    % Perform the function and send the result back to the parent.
    Parent ! {finished, self(), (catch Function(Item, Parent))}.

assignment_results_gather(Pids) ->
    assignment_results_gather(Pids, 0).

assignment_results_gather([], Result) ->
    Result;
assignment_results_gather(Pids, BestSoFar) ->
    % Gather up all the results into a result list.
    receive
        {new_best, Pid, Best} -> 
            %io:format("Received new best ~p from ~p~n", [Best, Pid]),
            update_processes(lists:delete(Pid, Pids), new_minimum, Best),
            assignment_results_gather(Pids, BestSoFar);
        {finished, Pid, Result} ->
            io:format("Pid ~p finished with result ~p~n", [Pid, Result]),
            case Result > BestSoFar of
                true ->
                    assignment_results_gather(lists:delete(Pid, Pids), Result);
                false ->
                    assignment_results_gather(lists:delete(Pid, Pids), BestSoFar)
            end
    end.

update_processes([], Message, _) ->
    ok;
update_processes([Pid | Pids], Message, Best) ->
    Pid ! {Message, Best},
    update_processes(Pids, Message, Best).



% Given a matrix, maximises the total obtainable by taking at most one element
% from each column and each row (the assignment problem).  Returns the total
% and a list of column/row pairs used in the solution.
%
% See http://en.wikipedia.org/wiki/Assignment_problem
%
% This approach uses dynamic programming and memoises partial results in an
% ets table.  This works fine for N < 20, but is expensive for memory as it
% is not tail recursive.  This solution is not highly parallelisable because
% each worker will need the same kind of bitmasks at about the right time.  With
% the overhead of message passing back to a controller to save each partial answer
% it actually turns out faster in many cases to just run a single thread.
%
% Algorithm after Larry's answer on Stack Overflow:
% http://stackoverflow.com/questions/2809334/best-way-to-get-the-highest-sum-from-a-matrix-using-java-but-algorithm-is-the-is/2810604#2810604
%
% Use maximise_assignment/1 instead.
maximise_assignment_dynamic(Matrix) ->
    {Width, Height} = matrix:dimensions(Matrix),
    PreparedMatrix = matrix:new(Width, Height, 
        fun(Column, Row, Columns, _) ->
            {1 bsl (Columns - Column), matrix:element_at(Column, Row, Matrix)}
        end
    ),
    ets:new(assignment, [private, named_table]),
    Answer = maximise_assignment_dynamic(PreparedMatrix, 0),
    ets:delete(assignment),
    Answer.

maximise_assignment_dynamic([], _) ->
    0;
maximise_assignment_dynamic([Row | Matrix], ColumnBitMask) ->
    case ets:lookup(assignment, ColumnBitMask) of
        [{ColumnBitMask, Value}] ->
            Value;
        [] ->
            SortedAvailableColumns = lists:filter(fun({Mask, _}) -> 
                    (Mask band ColumnBitMask) == 0
                end,
                Row
            ),

            PathTotal = lists:foldl(fun({Mask, Value}, MaxValue) ->
                    PathAnswer = maximise_assignment_dynamic(Matrix, ColumnBitMask bor Mask) + Value,
                    case PathAnswer > MaxValue of
                        true ->
                            % The path starting from this column is better than the
                            % best so far so store that result as the answer.
                            PathAnswer;
                        false ->
                            % The path starting from this column is less valuable 
                            % than the best so far, so stick with what we have.
                            MaxValue
                    end
                end,
                0,
                SortedAvailableColumns
            ),
            ets:insert(assignment, {ColumnBitMask, PathTotal}),
            PathTotal
    end.

