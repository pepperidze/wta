-module(wta_api).

-include("wta.hrl").

-export([
    employee_create/1,
    card_create/1,
    card_assign/1,
    card_delete/1,
    card_delete_all_by_employee/1,
    card_list_by_employee/1,
    card_touch/1,
    work_time_set/1,
    work_time_get/1,
    work_time_add_exclusion/1,
    work_time_get_exclusion/1,
    work_time_history_by_employee/1,
    work_time_history/1,
    work_time_statistics_by_employee/1,
    work_time_statistics/1
]).

%% ====================================================================
%% API
%% ====================================================================

employee_create(Args) ->
    #{<<"last_name">> := LastName,
        <<"first_name">> := FirstName,
        <<"middle_name">> := MiddleName,
        <<"contract_id">> := ContractId
    } = Args,
    case wta_db:employee_create(LastName, FirstName, MiddleName, ContractId) of
        {ok, EmployeeId} ->
            {ok, #{<<"employee_id">> => EmployeeId}};
        {error, Error} ->
            {error, Error}
    end.

card_create(Args) ->
    #{<<"comment">> := Comment} = Args,
    {ok, CardUID} = wta_db:card_create(Comment),
    {ok, #{<<"card_uid">> => CardUID}}.

card_assign(Args) ->
    #{<<"employee_id">> := EmployeeId,
        <<"card_uid">> := CardUID
    } = Args,
    case {wta_db:employee_exists(EmployeeId), wta_db:card_exists(CardUID)} of
        {true, true} ->
            card_assign_int(EmployeeId, CardUID);
        {false, _} ->
            {error, employee_not_exists};
        {_, false} ->
            {error, card_not_exists}
    end.

card_assign_int(EmployeeId, CardUID) ->
    case wta_db:card_is_valid_to_assign(CardUID) of
        true ->
            ok = wta_db:card_assign(EmployeeId, CardUID),
            {ok, #{<<"employee_id">> => EmployeeId, <<"card_uid">> => CardUID}};
        false ->
            {error, card_not_valid_to_assign}
    end.

card_delete(Args) ->
    #{<<"card_uid">> := CardUID} = Args,
    case wta_db:card_delete_by_uid(CardUID) of
        {ok, EmployeeId} ->
            {ok, #{<<"employee_id">> => EmployeeId, <<"card_uid">> => CardUID}};
        {error, Error} ->
            {error, Error}
    end.

card_delete_all_by_employee(Args) ->
    #{<<"employee_id">> := EmployeeId} = Args,
    case wta_db:employee_exists(EmployeeId) of
        true ->
            {ok, CardUIDs} = wta_db:card_delete_by_employee_id(EmployeeId),
            {ok, #{EmployeeId => CardUIDs}};
        false ->
            {error, employee_not_exists}
    end.

card_list_by_employee(Args) ->
    #{<<"employee_id">> := EmployeeId} = Args,
    case wta_db:employee_exists(EmployeeId) of
        true ->
            {ok, CardUIDs} = wta_db:card_list_by_employee_id(EmployeeId),
            {ok, #{EmployeeId => CardUIDs}};
        false ->
            {error, employee_not_exists}
    end.

card_touch(Args) ->
    #{<<"card_uid">> := CardUID} = Args,
    case wta_db:employee_get_by_card_uid(CardUID) of
        {ok, EmployeeId} ->
            wta_db:card_touch(EmployeeId, CardUID);
        {error, Error} ->
            {error, Error}
    end.

work_time_set(Args) ->
    #{<<"employee_id">> := EmployeeId,
        <<"start_time">> := StartTime,
        <<"end_time">> := EndTime,
        <<"work_days">> := WorkDays,
        <<"is_free_schedule">> := IsFreeSchedule
    } = Args,
    case wta_db:employee_exists(EmployeeId) of
        true ->
            wta_db:work_time_set(EmployeeId, StartTime, EndTime, WorkDays, IsFreeSchedule);
        false ->
            {error, employee_not_exists}
    end.

work_time_get(Args) ->
    #{<<"employee_id">> := EmployeeId} = Args,
    case wta_db:employee_exists(EmployeeId) of
        true ->
            work_time_get_int(EmployeeId);
        false ->
            {error, employee_not_exists}
    end.

work_time_get_int(EmployeeId) ->
    case wta_db:work_time_get(EmployeeId) of
        {ok, {StartTime, EndTime, WorkDays, IsFreeSchedule}} ->
            {ok, #{
                <<"employee_id">> => EmployeeId,
                <<"start_time">> => StartTime,
                <<"end_time">> => EndTime,
                <<"work_days">> => WorkDays,
                <<"is_free_schedule">> => IsFreeSchedule
            }};
        {error, Error} ->
            {error, Error}
    end.

work_time_add_exclusion(Args) ->
    #{<<"employee_id">> := EmployeeId,
        <<"exclusion_type">> := ExclusionType,
        <<"start_datetime">> := StartDatetime,
        <<"end_datetime">> := EndDatetime
    } = Args,
    case wta_db:employee_exists(EmployeeId) of
        true ->
            work_time_add_exclusion_int(EmployeeId, ExclusionType, StartDatetime, EndDatetime);
        false ->
            {error, employee_not_exists}
    end.

work_time_add_exclusion_int(EmployeeId, ExclusionType, StartDatetime, EndDatetime) ->
    case validate_exclusion(ExclusionType, StartDatetime, EndDatetime) of
        {ok, {Dates, StartTime, EndTime}} ->
            [wta_db:work_time_add_exclusion(EmployeeId, Date, ExclusionType, StartTime, EndTime) || Date <- Dates],
            ok;
        {error, Error} ->
            {error, Error}
    end.

work_time_get_exclusion(Args) ->
    #{<<"employee_id">> := EmployeeId} = Args,
    case wta_db:employee_exists(EmployeeId) of
        true ->
            {ok, Rows} = wta_db:work_time_get_exclusion(EmployeeId),
            {ok, #{EmployeeId => [#{
                <<"date">> => Date,
                <<"exclusion_type">> => ExclusionType,
                <<"start_time">> => StartTime,
                <<"end_time">> => EndTime
            } || {Date, ExclusionType, StartTime, EndTime} <- Rows]}};
        false ->
            {error, employee_not_exists}
    end.

work_time_history_by_employee(Args) ->
    #{<<"period">> := Period,
        <<"employee_id">> := EmployeeId
    } = Args,
    case wta_db:employee_exists(EmployeeId) of
        true ->
            {ok, Rows} = wta_db:work_time_history(Period, EmployeeId, null, null),
            {ok, #{<<"history">> => pack_history(Rows)}};
        false ->
            {error, employee_not_exists}
    end.

work_time_history(Args) ->
    #{<<"period">> := Period,
        <<"limit">> := Limit,
        <<"offset">> := Offset
    } = Args,
    {ok, Rows} = wta_db:work_time_history(Period, null, Limit, Offset),
    {ok, #{
        <<"history">> => pack_history(Rows),
        <<"limit">> => Limit,
        <<"offset">> => Offset
    }}.

pack_history(Rows) ->
    [#{<<"employee_id">> => EmployeeId,
        <<"date">> => Date,
        <<"logged_start_time">> => LogStart,
        <<"logged_end_time">> => LogEnd,
        <<"logged_time">> => LogTime,
        <<"exclusion">> => Ex,
        <<"exclusion_start_time">> => ExStart,
        <<"exclusion_end_time">> => ExEnd,
        <<"exclusion_time">> => ExTime,
        <<"is_free_schedule">> => IsFreeSchedule,
        <<"work_days">> => WorkDays,
        <<"schedule_start_time">> => ScheduleStart,
        <<"schedule_end_time">> => ScheduleEnd,
        <<"planned_time">> => PlannedTime
    } || {EmployeeId, Date, LogStart, LogEnd, LogTime, Ex, ExStart, ExEnd, ExTime,
        IsFreeSchedule, WorkDays, ScheduleStart, ScheduleEnd, PlannedTime} <- Rows].

work_time_statistics_by_employee(Args) ->
    #{<<"period">> := Period,
        <<"employee_id">> := EmployeeId
    } = Args,
    case wta_db:employee_exists(EmployeeId) of
        true ->
            {ok, Rows} = wta_db:work_time_statistics(Period, EmployeeId, null, null),
            work_time_statistics_by_employee_int(EmployeeId, Rows);
        false ->
            {error, employee_not_exists}
    end.

work_time_statistics_by_employee_int(EmployeeId, []) ->
    {ok, #{EmployeeId => #{}}};
work_time_statistics_by_employee_int(_EmployeeId, [Row]) ->
    {ok, pack_statistics(Row)}.

work_time_statistics(Args) ->
    #{<<"period">> := Period,
        <<"limit">> := Limit,
        <<"offset">> := Offset
    } = Args,
    {ok, Rows} = wta_db:work_time_statistics(Period, null, Limit, Offset),
    {ok, #{
        <<"statistics">> => [pack_statistics(Row) || Row <- Rows],
        <<"limit">> => Limit,
        <<"offset">> => Offset
    }}.

pack_statistics(Row) ->
    {EmployeeId, PlannedTime, LoggedTime, ComeLate, ComeLateEx, LeaveEarly, LeaveEarlyEx} = Row,
    #{EmployeeId => #{
        <<"planned_time">> => PlannedTime,
        <<"logged_time">> => LoggedTime,
        <<"come_late">> => ComeLate,
        <<"come_late_exclusion">> => ComeLateEx,
        <<"leave_early">> => LeaveEarly,
        <<"leave_early_exclusion">> => LeaveEarlyEx
    }}.

%% ====================================================================
%% Internal functions
%% ====================================================================

validate_exclusion(_ExType, StartDatetime, EndDatetime)
    when StartDatetime >= EndDatetime ->
    {error, datetime_not_valid};
validate_exclusion(ExType, {StartDate, _StartTime}, {EndDate, _EndTime})
    when ExType == ?WTA_EXCLUSION_SICK_LEAVE
    orelse ExType == ?WTA_EXCLUSION_DAY_OFF
    orelse ExType == ?WTA_EXCLUSION_VACATION ->
    {ok, {days_between(StartDate, EndDate), null, null}};
validate_exclusion(ExType, {StartDate, StartTime}, {EndDate, EndTime})
    when ExType == ?WTA_EXCLUSION_COME_LATE
    orelse ExType == ?WTA_EXCLUSION_LEAVE_EARLY ->
    {ok, {days_between(StartDate, EndDate), StartTime, EndTime}}.

days_between(StartDate, EndDate) when StartDate =< EndDate ->
    Interval = calendar:date_to_gregorian_days(EndDate) - calendar:date_to_gregorian_days(StartDate),
    [calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(StartDate) + Day) || Day <- lists:seq(0, Interval)].
