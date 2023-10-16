-module(oko_data).
-export([submit_data/2]).

% Node submits data for training
submit_data(NodeId, DataPackage) when is_binary(DataPackage) ->
    io:format("Data received from Node: ~p~n", [NodeId]),    
    case validate_data(DataPackage) of
        true ->
            % Data is valid, add to blockchain
            NewBlock = oko_block:add_block(DataPackage), % This function needs to be adjusted based on your blockchain implementation
            {success, NewBlock};
        false ->
            % Data is not valid
            {error, invalid_data}
    end.

validate_data(DataPackage) ->
    % Simple validation for MVP: Check if data starts with "VALID"
    "VALID" == binary:part(DataPackage, {0, 5}).
