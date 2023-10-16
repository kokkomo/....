-module(oko_data_classifier).
-export([classify_data/1]).

% Define some basic patterns for demonstration purposes
-define(POSITIVE_PATTERN, "OKO_POSITIVE").
-define(MALICIOUS_PATTERN, "OKO_MALICIOUS").

classify_data(Data) when is_binary(Data) ->
    % Check for the predefined patterns
    case binary:matches(Data, ?POSITIVE_PATTERN) of
        [_ | _] -> positive;  % Changed Match to _
        _ ->
            case binary:matches(Data, ?MALICIOUS_PATTERN) of
                [_ | _] -> malicious;  % Changed Match to _
                _ -> outlier_decision()
            end
    end.

outlier_decision() ->
    % Randomly classify 7% as outlier
    Rand = rand:uniform(100),
    if Rand =< 7 -> outlier;
       true -> undefined
    end.
