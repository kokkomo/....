-module(oko_nodes).
-export([create/2, submit_data/2, validate_data/2, train_model/2, generate_unique_id/0, generate_wallet/0]).

-record(node, {
    id = generate_unique_id() :: binary(),
    role :: contributor | validator | trainer,
    status = idle :: idle | active | busy,
    wallet_address = generate_wallet() :: binary()
}).

% Unique ID Generation
generate_unique_id() ->
    erlang:unique_integer([positive]).

% Wallet Address Generation
generate_wallet() ->
    "WALLET_" ++ integer_to_list(erlang:unique_integer([positive])).

% Node Creation
create(Role, WalletAddress) ->
    #node{
        role = Role,
        wallet_address = WalletAddress
    }.

% Contributor Behavior
submit_data(Node, Data) when Node#node.role =:= contributor ->
    io:format("Data ~p submitted by contributor node ~p~n", [Data, Node#node.id]),
    ok.

% Validator Behavior
validate_data(Node, Data) when Node#node.role =:= validator ->
    io:format("Data ~p validated by validator node ~p~n", [Data, Node#node.id]),
    ok.

% Trainer Behavior
train_model(Node, Data) when Node#node.role =:= trainer ->
    io:format("Model trained with data ~p by trainer node ~p~n", [Data, Node#node.id]),
    ok.
