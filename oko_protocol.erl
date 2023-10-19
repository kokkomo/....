-module(oko_protocol).
-export([handle_info/2, init/1, terminate/2, handle_data/3]).

%% This function is called when a new stream is initiated.
init(TcpOpts) ->
    {ok, #{}}.

%% Handle incoming data on the stream.
handle_data(Stream, Data, State) ->
    %% Process the incoming Data here
    %% For now, we just print it to the console
    io:format("Received data: ~p~n", [Data]),
    {ok, State}.

%% This function is called when there's a non-message event to handle.
handle_info(_Info, State) ->
    {ok, State}.

%% This function is called when the stream is terminated.
terminate(_Reason, _State) ->
    ok.
