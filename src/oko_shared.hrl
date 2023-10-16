-record(data, {
    node_id,
    data_package,
    timestamp = erlang:system_time(millisecond) % Defaults to current time in milliseconds
}).
-record(pot, {
    classification = undefined :: positive | malicious | outlier | undefined,
    field2 = undefined :: any(),
    field3 = undefined :: any(),
    field4 = undefined :: any(),
    field5 = undefined :: any()
}).
