-module(oko_block).
-export([create_genesis_blocks/0, new/4, create_genesis_block/0, add_block/2, validate_chain/1,
         get_previous_hash/1, get_timestamp/1, get_layer/1, get_data/1,
         get_hash/1, get_pot/1, create_sha256_genesis_block/0, create_reserved_block/0, compute_hash/1]).

-include("oko_shared.hrl").

-record(block, {
    timestamp = erlang:system_time(millisecond) :: non_neg_integer(),
    data = <<>> :: binary(),
    prev_hash = <<>> :: binary(),
    hash = <<>> :: binary(),
    pot = #pot{},
    layer = gamma :: alpha | beta | gamma
}).

% Creates a new block
new(PreviousHash, Layer, Data, Pot) ->
    Timestamp = erlang:system_time(millisecond),
    HashInput = <<Data/binary, PreviousHash/binary, Timestamp:64>>,
    Hash = compute_hash(HashInput),
    #block{
        timestamp = Timestamp,
        data = Data,
        prev_hash = PreviousHash,
        hash = Hash,
        layer = Layer,
        pot = Pot
    }.

% Creates the genesis block
create_genesis_block() ->
    Data = <<"Genesis Block">>,
    HashInput = <<Data/binary>>,
    Hash = compute_hash(HashInput),
    #block{
        timestamp = erlang:system_time(millisecond),
        data = Data,
        prev_hash = <<>>,
        hash = Hash,
        layer = gamma,
        pot = #pot{}
    }.

% Adds a block to the blockchain
add_block(Chain, Data) ->
    PrevBlock = hd(Chain),
    PrevHash = PrevBlock#block.hash,
    NewBlock = new(PrevHash, gamma, Data, #pot{}),
    [NewBlock | Chain].

% Validates the entire blockchain
validate_chain([_GenesisBlock]) -> true; % only the genesis block is always valid
validate_chain([Block, PrevBlock | Rest]) ->
    ValidHash = Block#block.prev_hash == PrevBlock#block.hash,
    ValidChain = validate_chain([PrevBlock | Rest]),
    ValidHash andalso ValidChain.

% Utility functions
get_previous_hash(Block) -> Block#block.prev_hash.
get_timestamp(Block) -> Block#block.timestamp.
get_layer(Block) -> Block#block.layer.
get_data(Block) -> Block#block.data.
get_hash(Block) -> Block#block.hash.
get_pot(Block) -> Block#block.pot.

% Genesis blocks for the 72 blocks protocol
create_genesis_blocks() ->
    [create_sha256_genesis_block() | [create_reserved_block() || _ <- lists:seq(1, 71)]].

create_sha256_genesis_block() ->
    Data = <<"Genesis Block SHA256">>,
    HashInput = <<Data/binary>>,
    Hash = compute_hash(HashInput),
    #block{
        timestamp = erlang:system_time(millisecond),
        data = Data,
        prev_hash = <<>>,
        hash = Hash,
        layer = gamma,
        pot = #pot{}
    }.

create_reserved_block() ->
    Data = <<"Reserved Genesis Block">>,
    HashInput = <<Data/binary>>,
    Hash = compute_hash(HashInput),
    #block{
        timestamp = erlang:system_time(millisecond),
        data = Data,
        prev_hash = <<>>,
        hash = Hash,
        layer = gamma,
        pot = #pot{}
    }.

compute_hash(Data) ->
    Digest = crypto:hash(sha256, Data),
    base64:encode(Digest).

