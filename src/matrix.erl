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
    multiply/2,
    new/1, 
    new/2, 
    new/3,
    power/2,
    random/3,
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


% Given a matrix, maximises the total obtainable by taking at most one element
% from each column and each row (the assignment problem).  Returns the total
% and a list of column/row pairs used in the solution.
%
% See http://en.wikipedia.org/wiki/Assignment_problem
-spec maximise_assignment(num_matrix()) -> {float(), [{integer(), integer()}]}.
maximise_assignment(Matrix) ->
    Matrix.
