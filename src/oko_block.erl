-module(oko_block).
-export([new/4, create_genesis_block/0, add_block/2, validate_chain/1,
         get_previous_hash/1, get_timestamp/1, get_layer/1, get_data/1,
         get_hash/1, get_pot_tag/1]).

-record(block, {
    timestamp = 0 :: non_neg_integer(),
    data = <<>> :: binary(),
    prev_hash = <<>> :: binary(),
    hash = <<>> :: binary(),
    pot_tag = undefined :: binary() | undefined,
    layer = gamma :: alpha | beta | gamma
}).

% Creates a new block
new(PreviousHash, Layer, Data, PotTag) ->
    Timestamp = erlang:system_time(millisecond),
    HashInput = <<Data/binary, PreviousHash/binary, Timestamp:64>>,
    Hash = compute_hash(HashInput),
    #block{
        timestamp = Timestamp,
        data = Data,
        prev_hash = PreviousHash,
        hash = Hash,
        layer = Layer,
        pot_tag = PotTag
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
        pot_tag = <<>>
    }.

% Adds a block to the blockchain
add_block(Chain, Data) ->
    PrevBlock = hd(Chain),
    PrevHash = PrevBlock#block.hash,
    NewBlock = new(PrevHash, gamma, Data, <<>>),
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
get_pot_tag(Block) -> Block#block.pot_tag.

compute_hash(Data) ->
    Digest = crypto:hash(sha256, Data),
    base64:encode(Digest).
