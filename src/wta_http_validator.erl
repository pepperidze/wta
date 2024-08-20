-module(wta_http_validator).

-include("wta.hrl").

-export([
    schema/1
]).

%% ====================================================================
%% API
%% ====================================================================

schema(<<"/employee/create">>) -> #{
    <<"last_name">> => [required, string, {length_between, [1, 255]}],
    <<"first_name">> => [required, string, {length_between, [1, 255]}],
    <<"middle_name">> => [required, string, {length_between, [1, 255]}],
    <<"contract_id">> => [required, positive_integer]
};
schema(<<"/card/create">>) -> #{
    <<"comment">> => [required, string, {length_between, [1, 255]}]
};
schema(<<"/card/assign">>) -> #{
    <<"employee_id">> => [required, positive_integer],
    <<"card_uid">> => [required, string, {length_equal, 36}]
};
schema(<<"/card/delete">>) -> #{
    <<"card_uid">> => [required, string, {length_equal, 36}]
};
schema(<<"/card/delete_all_by_employee">>) -> #{
    <<"employee_id">> => [required, positive_integer]
};
schema(<<"/card/list_by_employee">>) -> #{
    <<"employee_id">> => [required, positive_integer]
};
schema(<<"/card/touch">>) -> #{
    <<"card_uid">> => [required, string, {length_equal, 36}]
};
schema(<<"/work_time/set">>) -> #{
    <<"employee_id">> => [required, positive_integer],
    <<"start_time">> => [required, time],
    <<"end_time">> => [required, time],
    <<"work_days">> => [required, day_nums_comma_separated],
    <<"is_free_schedule">> => [required, is_boolean]
};
schema(<<"/work_time/get">>) -> #{
    <<"employee_id">> => [required, positive_integer]
};
schema(<<"/work_time/add_exclusion">>) -> #{
    <<"employee_id">> => [required, positive_integer],
    <<"exclusion_type">> => [required, {one_of, ?WTA_EXCLUSIONS}],
    <<"start_datetime">> => [required, datetime],
    <<"end_datetime">> => [required, datetime]
};
schema(<<"/work_time/get_exclusion">>) -> #{
    <<"employee_id">> => [required, positive_integer]
};
schema(<<"/work_time/history_by_employee">>) -> #{
    <<"period">> => [#{'or' => [{one_of, ?WTA_PERIODS}, is_null]}, {default, ?WTA_PERIOD_MONTH}],
    <<"employee_id">> => [required, positive_integer]
};
schema(<<"/work_time/history">>) -> #{
    <<"period">> => [#{'or' => [{one_of, ?WTA_PERIODS}, is_null]}, {default, ?WTA_PERIOD_MONTH}],
    <<"limit">> => [required, {number_between, [1, 100]}],
    <<"offset">> => [required, integer, {min_number, 0}]
};
schema(<<"/work_time/statistics_by_employee">>) -> #{
    <<"period">> => [#{'or' => [{one_of, ?WTA_PERIODS}, is_null]}, {default, ?WTA_PERIOD_MONTH}],
    <<"employee_id">> => [required, positive_integer]
};
schema(<<"/work_time/statistics">>) -> #{
    <<"period">> => [#{'or' => [{one_of, ?WTA_PERIODS}, is_null]}, {default, ?WTA_PERIOD_MONTH}],
    <<"limit">> => [required, {number_between, [1, 100]}],
    <<"offset">> => [required, integer, {min_number, 0}]
};
schema(_Path) -> #{
}.

%% ====================================================================
%% Internal functions
%% ====================================================================
