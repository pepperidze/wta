-module(wta_liver_rules_tests).

-include_lib("eunit/include/eunit.hrl").

time_test_() ->
    ok = wta_liver_rules:init_custom_rules(),
    [
        ?_assertMatch({ok, #{<<"value">> := {12, 0, 0}}},
            liver:validate(#{<<"value">> => [time]}, #{<<"value">> => <<"12:00:00">>})),
        ?_assertMatch({ok, #{<<"value">> := {0, 0, 0}}},
            liver:validate(#{<<"value">> => [time]}, #{<<"value">> => <<"00:00:00">>})),
        ?_assertMatch({error, #{<<"value">> := <<"FORMAT_ERROR">>}},
            liver:validate(#{<<"value">> => [time]}, #{<<"value">> => <<"24:00:00">>})),
        ?_assertMatch({error, #{<<"value">> := <<"FORMAT_ERROR">>}},
            liver:validate(#{<<"value">> => [time]}, #{<<"value">> => <<"00:60:00">>})),
        ?_assertMatch({error, #{<<"value">> := <<"FORMAT_ERROR">>}},
            liver:validate(#{<<"value">> => [time]}, #{<<"value">> => <<"00:00:60">>})),
        ?_assertMatch({error, #{<<"value">> := <<"FORMAT_ERROR">>}},
            liver:validate(#{<<"value">> => [time]}, #{<<"value">> => <<"00 00 00">>}))
    ].

datetime_test_() ->
    ok = wta_liver_rules:init_custom_rules(),
    [
        ?_assertMatch({ok, #{<<"value">> := {{2024, 1, 1}, {12, 0, 0}}}},
            liver:validate(#{<<"value">> => [datetime]}, #{<<"value">> => <<"2024-01-01T12:00:00Z">>})),
        ?_assertMatch({ok, #{<<"value">> := {{2024, 2, 29}, {23, 1, 59}}}},
            liver:validate(#{<<"value">> => [datetime]}, #{<<"value">> => <<"2024-02-29T23:01:59Z">>})),
        ?_assertMatch({error, #{<<"value">> := <<"FORMAT_ERROR">>}},
            liver:validate(#{<<"value">> => [datetime]}, #{<<"value">> => <<"2024-02-30T09:00:00Z">>})),
        ?_assertMatch({error, #{<<"value">> := <<"FORMAT_ERROR">>}},
            liver:validate(#{<<"value">> => [datetime]}, #{<<"value">> => <<"2024-13-29T09:00:00Z">>})),
        ?_assertMatch({error, #{<<"value">> := <<"FORMAT_ERROR">>}},
            liver:validate(#{<<"value">> => [datetime]}, #{<<"value">> => <<"24-02-29T09:00:00Z">>})),
        ?_assertMatch({error, #{<<"value">> := <<"FORMAT_ERROR">>}},
            liver:validate(#{<<"value">> => [datetime]}, #{<<"value">> => <<"2024-01-01T24:00:00Z">>})),
        ?_assertMatch({error, #{<<"value">> := <<"FORMAT_ERROR">>}},
            liver:validate(#{<<"value">> => [datetime]}, #{<<"value">> => <<"2024-01-01T09:60:00Z">>})),
        ?_assertMatch({error, #{<<"value">> := <<"FORMAT_ERROR">>}},
            liver:validate(#{<<"value">> => [datetime]}, #{<<"value">> => <<"2024-01-01T09:00:60Z">>})),
        ?_assertMatch({error, #{<<"value">> := <<"FORMAT_ERROR">>}},
            liver:validate(#{<<"value">> => [datetime]}, #{<<"value">> => <<"2024-01-01 09:00:00Z">>})),
        ?_assertMatch({error, #{<<"value">> := <<"FORMAT_ERROR">>}},
            liver:validate(#{<<"value">> => [datetime]}, #{<<"value">> => <<"2024-01-01T09:00:00">>}))
    ].

day_nums_comma_separated_test_() ->
    ok = wta_liver_rules:init_custom_rules(),
    [
        ?_assertMatch({ok, #{<<"value">> := [1, 2, 3]}},
            liver:validate(#{<<"value">> => [day_nums_comma_separated]}, #{<<"value">> => <<"1,2,3">>})),
        ?_assertMatch({ok, #{<<"value">> := [1]}},
            liver:validate(#{<<"value">> => [day_nums_comma_separated]}, #{<<"value">> => <<"1">>})),
        ?_assertMatch({ok, #{<<"value">> := [1, 2, 3, 4, 5, 6, 7]}},
            liver:validate(#{<<"value">> => [day_nums_comma_separated]}, #{<<"value">> => <<"1,2,3,4,5,6,7">>})),
        ?_assertMatch({error, #{<<"value">> := <<"FORMAT_ERROR">>}},
            liver:validate(#{<<"value">> => [day_nums_comma_separated]}, #{<<"value">> => <<"-1,2,3">>})),
        ?_assertMatch({error, #{<<"value">> := <<"FORMAT_ERROR">>}},
            liver:validate(#{<<"value">> => [day_nums_comma_separated]}, #{<<"value">> => <<"8">>})),
        ?_assertMatch({error, #{<<"value">> := <<"FORMAT_ERROR">>}},
            liver:validate(#{<<"value">> => [day_nums_comma_separated]}, #{<<"value">> => <<"1,1,2">>}))
    ].
