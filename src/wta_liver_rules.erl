-module(wta_liver_rules).

-export([
    init_custom_rules/0,
    time/3,
    datetime/3,
    day_nums_comma_separated/3
]).

%% ====================================================================
%% API
%% ====================================================================

-spec init_custom_rules() -> ok.
init_custom_rules() ->
    ok = liver:add_rule(time, ?MODULE),
    ok = liver:add_rule(datetime, ?MODULE),
    ok = liver:add_rule(day_nums_comma_separated, ?MODULE).

time(_Args, <<BinH:2/binary, ":", BinMin:2/binary, ":", BinS:2/binary>>, _Opts) ->
    try
        {true, Time} = validate_time(BinH, BinMin, BinS),
        {ok, Time}
    catch _:_:_ ->
        {error, format_error}
    end;
time(_Args, _Value, _Opts) ->
    {error, format_error}.

datetime(_Args, <<BinY:4/binary, "-", BinM:2/binary, "-", BinD:2/binary, "T",
    BinH:2/binary, ":", BinMin:2/binary, ":", BinS:2/binary, "Z">>, _Opts) ->
    try
        {{true, Date}, {true, Time}} = {validate_date(BinY, BinM, BinD), validate_time(BinH, BinMin, BinS)},
        {ok, {Date, Time}}
    catch _:_:_ ->
        {error, format_error}
    end;
datetime(_Args, _Value, _Opts) ->
    {error, format_error}.

day_nums_comma_separated(_Args, Value, _Opts) when is_binary(Value) ->
    try
        DayNums = [binary_to_integer(Item) || Item <- binary:split(Value, <<",">>, [global])],
        true = lists:max(DayNums) >= 1 andalso lists:max(DayNums) =< 7 andalso lists:min(DayNums) =< 7
            andalso lists:min(DayNums) >= 1 andalso is_unique_list(DayNums),
        {ok, DayNums}
    catch _:_:_ ->
        {error, format_error}
    end;
day_nums_comma_separated(_Args, _Value, _Opts) ->
    {error, format_error}.

%% ====================================================================
%% Internal functions
%% ====================================================================

validate_date(BinY, BinM, BinD) ->
    Y = binary_to_integer(BinY),
    M = binary_to_integer(BinM),
    D = binary_to_integer(BinD),
    case calendar:valid_date({Y, M, D}) of
        true -> {true, {Y, M, D}};
        false -> false
    end.

validate_time(BinH, BinMin, BinS) ->
    H = binary_to_integer(BinH),
    Min = binary_to_integer(BinMin),
    S = binary_to_integer(BinS),
    case (H >= 0 andalso H < 24) andalso (Min >= 0 andalso Min < 60) andalso (S >= 0 andalso S < 60) of
        true -> {true, {H, Min, S}};
        false -> false
    end.

is_unique_list(List) ->
    erlang:length(List) == sets:size(sets:from_list(List)).
