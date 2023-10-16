-module(oko_chain).
-export([create_chain/0, add_block/2, validate_chain/1, get_chain_length/1, create_chain_with_genesis_blocks/1]). % <-- Add the new function here

-define(GENESIS_BLOCK_COUNT, 333).

create_chain() ->
    InitialChain = [oko_block:create_genesis_block()],
    create_multiple_genesis_blocks(?GENESIS_BLOCK_COUNT - 1, InitialChain).

create_multiple_genesis_blocks(0, Chain) -> Chain;
create_multiple_genesis_blocks(Count, Chain) when Count > 0 ->
    Data = <<"Genesis Block ", (integer_to_binary(length(Chain) + 1))/binary>>,
    NewChain = oko_block:add_block(Chain, Data),
    create_multiple_genesis_blocks(Count - 1, NewChain).

create_chain_with_genesis_blocks(Count) ->  % <-- New function
    InitialChain = [oko_block:create_genesis_block()],
    create_multiple_genesis_blocks(Count, InitialChain).

add_block(Chain, Data) ->
    oko_block:add_block(Chain, Data).

validate_chain(Chain) ->
    oko_block:validate_chain(Chain).

get_chain_length(Chain) ->
    length(Chain).
