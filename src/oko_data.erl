-module(oko_data).
-export([submit_data/2]).

% Node submits data for training
submit_data(NodeId, DataPackage) when is_binary(DataPackage) ->
    io:format("Data received from Node: ~p~n", [NodeId]),    
    case validate_data(DataPackage) of
        true ->
            % Data is valid, add to blockchain
            % For simplicity, assuming a function get_current_chain/0 fetches the current state of the blockchain
            CurrentChain = get_current_chain(),
            NewBlock = oko_block:add_block(CurrentChain, DataPackage),
            {success, NewBlock};
        false ->
            % Data is not valid
            {error, invalid_data}
    end.

validate_data(DataPackage) ->
    % Simple validation for MVP: Check if data starts with "VALID"
    "VALID" == binary:part(DataPackage, {0, 5}).

% For the purpose of this demonstration, I'm assuming a dummy blockchain state
get_current_chain() ->
    [oko_block:create_genesis_block()].
