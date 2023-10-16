%%%-------------------------------------------------------------------
%% @doc oko_blockchain public API
%% @end
%%%-------------------------------------------------------------------

-module(oko_blockchain_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    oko_blockchain_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
