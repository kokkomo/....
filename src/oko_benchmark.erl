-module(oko_benchmark).
-export([run_benchmark/0, assign_node_type/1]).

% The benchmark function simulates some CPU-bound work.
benchmark_function() ->
    lists:foldl(fun(X, Acc) -> X + Acc end, 0, lists:seq(1, 1000000)).

% Runs the benchmark function and measures the time taken.
run_benchmark() ->
    {Time, _Result} = timer:tc(fun benchmark_function/0),
    assign_node_type(Time).

% Assigns a node type based on the execution time.
assign_node_type(Time) when Time < 1000000 -> % Less than 1 second
    alpha;
assign_node_type(Time) when Time >= 1000000, Time < 3000000 -> % 1 to 3 seconds
    beta;
assign_node_type(_Time) -> % More than 3 seconds
    gamma.
