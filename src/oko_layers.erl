-module(oko_layers).
-export([get_current_layer/1, set_layer/2, transition_to_next_layer/1, validate_layer_transition/2]).

%% Define the layers
-define(GAMMA, gamma).
-define(BETA, beta).
-define(ALPHA, alpha).

%% Retrieve the current layer of a given block
get_current_layer(Block) ->
    case maps:get(layer, Block, undefined) of
        undefined -> 
            {error, no_layer_defined};
        Layer -> 
            {ok, Layer}
    end.

%% Set a specific layer for a block
set_layer(Block, Layer) when Layer =:= ?GAMMA; Layer =:= ?BETA; Layer =:= ?ALPHA ->
    UpdatedBlock = maps:put(layer, Layer, Block),
    {ok, UpdatedBlock};
set_layer(_, _) ->
    {error, invalid_layer}.

%% Transition a block to the next layer
transition_to_next_layer(Block) ->
    CurrentLayer = get_current_layer(Block),
    NextLayer = determine_next_layer(CurrentLayer),
    validate_layer_transition(CurrentLayer, NextLayer),
    set_layer(Block, NextLayer).

%% Determine the next layer based on the current layer
determine_next_layer({ok, ?GAMMA}) -> 
    ?BETA;
determine_next_layer({ok, ?BETA}) -> 
    ?ALPHA;
determine_next_layer({ok, ?ALPHA}) -> 
    {error, no_further_layers};
determine_next_layer(_) -> 
    {error, invalid_current_layer}.

%% Validate if transitioning from one layer to another is allowed
validate_layer_transition(?GAMMA, ?BETA) ->
    ok;
validate_layer_transition(?BETA, ?ALPHA) ->
    ok;
validate_layer_transition(_, _) ->
    {error, invalid_layer_transition}.

