-module(oko_contract).
-export([generate_contract/1, update_contract_reward/2]).

-include("oko_shared.hrl").

% Generate a contract for valid data submission
generate_contract(DataPackage) when is_record(DataPackage, data) ->
    % Extract data package details
    NodeId = DataPackage#data.node_id,
    Timestamp = DataPackage#data.timestamp,

    % Create initial contract with default reward of 1
    Contract = #{
        node_id => NodeId,
        data_package => DataPackage,
        timestamp => Timestamp,
        proposed_reward => 1
    },
    {success, Contract}.

% Update the contract reward based on data quality (1 to 5)
update_contract_reward(Contract, Quality) when Quality >= 1, Quality =< 5 ->
    UpdatedContract = Contract#{proposed_reward => Quality},
    {success, UpdatedContract};
update_contract_reward(_, _) ->
    {error, invalid_quality}.
