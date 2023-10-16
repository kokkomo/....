-module(oko_node_init).
-export([initialize_node/1, authenticate_node/1, assign_node_layer/0]).

% List of valid secrets for authentication
valid_secrets() ->
    ["secret1", "secret2", "secret3"].

% Function to authenticate a node using a secret key
authenticate_node(Secret) ->
    lists:member(Secret, valid_secrets()).

% Function to initialize a node based on provided secret
initialize_node(Secret) when is_binary(Secret) ->
    case authenticate_node(binary_to_list(Secret)) of
        true -> 
            assign_node_layer();
        false -> 
            {error, "Authentication failed. Invalid secret."}
    end;
initialize_node(Secret) ->
    case authenticate_node(Secret) of
        true -> 
            assign_node_layer();
        false -> 
            {error, "Authentication failed. Invalid secret."}
    end.

% Function to assign node layer based on benchmark results
assign_node_layer() ->
    BenchmarkResult = oko_benchmark:run_benchmark(),
    case BenchmarkResult of
        alpha -> {success, "Node assigned to Alpha layer"};
        beta -> {success, "Node assigned to Beta layer"};
        gamma -> {success, "Node assigned to Gamma layer"};
        _ -> {error, "Failed to assign node layer"}
    end.
