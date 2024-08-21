-module(wta_db).

-include("wta.hrl").

-export([
    employee_create/4,
    employee_exists/1,
    employee_get_by_card_uid/1,
    card_create/1,
    card_exists/1,
    card_is_valid_to_assign/1,
    card_assign/2,
    card_delete_by_uid/1,
    card_delete_by_employee_id/1,
    card_list_by_employee_id/1,
    card_touch/2,
    work_time_set/5,
    work_time_get/1,
    work_time_add_exclusion/5,
    work_time_get_exclusion/1,
    work_time_history/4,
    work_time_statistics/4
]).

%% ====================================================================
%% API
%% ====================================================================

-spec employee_create(LastName, FirstName, MiddleName, ContractId) -> Result when
    LastName :: binary(),
    FirstName :: binary(),
    MiddleName :: binary(),
    ContractId :: pos_integer(),
    Result :: {ok, EmployeeId :: wta:employee_id()} | {error, employee_already_exists}.
employee_create(LastName, FirstName, MiddleName, ContractId) ->
    Sql = "INSERT INTO public.employees (last_name, first_name, middle_name, contract_id)
           VALUES ($1, $2, $3, $4)
           ON CONFLICT DO NOTHING
           RETURNING id;",
    Args = [LastName, FirstName, MiddleName, ContractId],
    case pgapp:equery(Sql, Args) of
        {ok, 1, _Columns, [{EmployeeId}]} ->
            {ok, EmployeeId};
        {ok, 0, _Columns, _Rows} ->
            {error, employee_already_exists}
    end.

-spec employee_exists(EmployeeId) -> Result when
    EmployeeId :: wta:employee_id(),
    Result :: boolean().
employee_exists(EmployeeId) ->
    Sql = "SELECT EXISTS (SELECT * FROM public.employees WHERE id = $1);",
    Args = [EmployeeId],
    {ok, _Columns, [{Exists}]} = pgapp:equery(Sql, Args),
    Exists.

-spec employee_get_by_card_uid(CardUID) -> Result when
    CardUID :: wta:uid(),
    Result :: {ok, EmployeeId :: wta:employee_id()} | {error, card_not_assigned} | {error, card_not_exists}.
employee_get_by_card_uid(CardUID) ->
    Sql = "SELECT employee_id
           FROM public.cards
           WHERE uid = $1 AND is_deleted = false;",
    Args = [CardUID],
    case pgapp:equery(Sql, Args) of
        {ok, _Columns, [{null}]} ->
            {error, card_not_assigned};
        {ok, _Columns, [{EmployeeId}]} ->
            {ok, EmployeeId};
        {ok, _Columns, []} ->
            {error, card_not_exists}
    end.

-spec card_create(Comment) -> Result when
    Comment :: binary(),
    Result :: {ok, CardUID :: wta:uid()}.
card_create(Comment) ->
    Sql = "INSERT INTO public.cards (comment)
           VALUES ($1)
           RETURNING uid;",
    Args = [Comment],
    {ok, 1, _Columns, [{CardUID}]} = pgapp:equery(Sql, Args),
    {ok, CardUID}.

-spec card_exists(CardUID) -> Result when
    CardUID :: wta:uid(),
    Result :: boolean().
card_exists(CardUID) ->
    Sql = "SELECT EXISTS (SELECT * FROM public.cards WHERE uid = $1 AND is_deleted = false);",
    Args = [CardUID],
    {ok, _Columns, [{Exists}]} = pgapp:equery(Sql, Args),
    Exists.

-spec card_is_valid_to_assign(CardUID) -> Result when
    CardUID :: wta:uid(),
    Result :: boolean().
card_is_valid_to_assign(CardUID) ->
    Sql = "SELECT employee_id
           FROM public.cards
           WHERE uid = $1 AND is_deleted = false;",
    Args = [CardUID],
    case pgapp:equery(Sql, Args) of
        {ok, _Columns, [{null}]} ->
            true;
        {ok, _Columns, _Rows} ->
            false
    end.

-spec card_assign(EmployeeId, CardUID) -> Result when
    EmployeeId :: wta:employee_id(),
    CardUID :: wta:uid(),
    Result :: ok | {error, not_updated}.
card_assign(EmployeeId, CardUID) ->
    Sql = "UPDATE public.cards SET
           employee_id = $1,
           last_update = CURRENT_TIMESTAMP(0)
           WHERE uid = $2 AND employee_id IS NULL AND is_deleted = false;",
    Args = [EmployeeId, CardUID],
    case pgapp:equery(Sql, Args) of
        {ok, 1} ->
            ok;
        {ok, 0} ->
            {error, not_updated}
    end.

-spec card_delete_by_uid(CardUID) -> Result when
    CardUID :: wta:uid(),
    Result :: {ok, EmployeeId :: wta:employee_id()} | {error, card_not_exists}.
card_delete_by_uid(CardUID) ->
    Sql = "UPDATE public.cards SET
           is_deleted = true,
           last_update = CURRENT_TIMESTAMP(0)
           WHERE uid = $1 AND is_deleted = false
           RETURNING employee_id;",
    Args = [CardUID],
    case pgapp:equery(Sql, Args) of
        {ok, 1, _Columns, [{EmployeeId}]} ->
            {ok, EmployeeId};
        {ok, 0, _Columns, []} ->
            {error, card_not_exists}
    end.

-spec card_delete_by_employee_id(EmployeeId) -> Result when
    EmployeeId :: wta:employee_id(),
    Result :: {ok, CardUIDs :: [wta:uid()]}.
card_delete_by_employee_id(EmployeeId) ->
    Sql = "UPDATE public.cards SET
           is_deleted = true,
           last_update = CURRENT_TIMESTAMP(0)
           WHERE employee_id = $1 AND is_deleted = false
           RETURNING uid;",
    Args = [EmployeeId],
    {ok, _, _Columns, Rows} = pgapp:equery(Sql, Args),
    CardUIDs = [CardUID || {CardUID} <- Rows],
    {ok, CardUIDs}.

-spec card_list_by_employee_id(EmployeeId) -> Result when
    EmployeeId :: wta:employee_id(),
    Result :: {ok, CardUIDs :: [wta:uid()]}.
card_list_by_employee_id(EmployeeId) ->
    Sql = "SELECT uid
           FROM public.cards
           WHERE employee_id = $1 AND is_deleted = false;",
    Args = [EmployeeId],
    {ok, _Columns, Rows} = pgapp:equery(Sql, Args),
    CardUIDs = [CardUID || {CardUID} <- Rows],
    {ok, CardUIDs}.

-spec card_touch(EmployeeId, CardUID) -> Result when
    EmployeeId :: wta:employee_id(),
    CardUID :: wta:uid(),
    Result :: ok.
card_touch(EmployeeId, CardUID) ->
    Sql = "INSERT INTO public.logs (employee_id, card_uid, start_time)
           VALUES ($1, $2, CURRENT_TIME(0))
           ON CONFLICT (employee_id, date)
           DO UPDATE SET
           card_uid = CASE WHEN (public.logs.card_uid IS NULL) THEN $2
               ELSE public.logs.card_uid END,
           start_time = CASE WHEN (public.logs.start_time IS NULL) THEN CURRENT_TIME(0)
               ELSE public.logs.start_time END,
           end_time = CASE WHEN (public.logs.start_time IS NOT NULL) AND (public.logs.end_time IS NULL) THEN CURRENT_TIME(0)
               ELSE public.logs.end_time END,
           last_update = CASE WHEN (public.logs.start_time IS NOT NULL) AND (public.logs.end_time IS NOT NULL) THEN public.logs.last_update
               ELSE CURRENT_TIMESTAMP(0) END;",
    Args = [EmployeeId, CardUID],
    {ok, 1} = pgapp:equery(Sql, Args),
    ok.

-spec work_time_set(EmployeeId, StartTime, EndTime, WorkDays, IsFreeSchedule) -> Result when
    EmployeeId :: wta:employee_id(),
    StartTime :: calendar:time(),
    EndTime :: calendar:time(),
    WorkDays :: [calendar:daynum()] | null,
    IsFreeSchedule :: boolean(),
    Result :: ok | {error, schedule_already_exists}.
work_time_set(EmployeeId, StartTime, EndTime, WorkDays, IsFreeSchedule) ->
    Sql = "INSERT INTO public.schedules (employee_id, start_time, end_time, work_days, is_free_schedule)
           VALUES ($1, $2, $3, $4::INTEGER[], $5)
           ON CONFLICT DO NOTHING;",
    Args = [EmployeeId, StartTime, EndTime, WorkDays, IsFreeSchedule],
    case pgapp:equery(Sql, Args) of
        {ok, 1} ->
            ok;
        {ok, 0} ->
            {error, schedule_already_exists}
    end.

-spec work_time_get(EmployeeId) -> Result when
    EmployeeId :: wta:employee_id(),
    Result :: {ok, Response :: tuple()} | {error, schedule_not_exists}.
work_time_get(EmployeeId) ->
    Sql = "SELECT start_time::TEXT, end_time::TEXT, work_days, is_free_schedule
           FROM public.schedules
           WHERE employee_id = $1;",
    Args = [EmployeeId],
    case pgapp:equery(Sql, Args) of
        {ok, _Columns, [Row]} ->
            {ok, Row};
        {ok, _Columns, []} ->
            {error, schedule_not_exists}
    end.

-spec work_time_add_exclusion(EmployeeId, Date, ExType, ExStartTime, ExEndTime) -> Result when
    EmployeeId :: wta:employee_id(),
    Date :: calendar:date(),
    ExType :: binary(),
    ExStartTime :: calendar:time() | null,
    ExEndTime :: calendar:time() | null,
    Result :: ok | {error, not_updated}.
work_time_add_exclusion(EmployeeId, Date, ExType, ExStartTime, ExEndTime) ->
    Sql = "INSERT INTO public.logs (employee_id, date, ex_type, ex_start_time, ex_end_time)
           SELECT $1, $2, $3, $4, $5 FROM public.schedules s
           WHERE s.employee_id = $1
           AND EXTRACT(ISODOW FROM $2::DATE) = ANY(s.work_days)
           AND (is_free_schedule = false OR (is_free_schedule = true AND $3::exclusion NOT IN ('come_late', 'leave_early')))
           ON CONFLICT (employee_id, date)
           DO UPDATE SET
           ex_type = $3,
           ex_start_time = $4,
           ex_end_time = $5,
           last_update = CURRENT_TIMESTAMP(0);",
    Args = [EmployeeId, Date, ExType, ExStartTime, ExEndTime],
    case pgapp:equery(Sql, Args) of
        {ok, 1} ->
            ok;
        {ok, 0} ->
            {error, not_updated};
        {error, _} ->
            {error, not_updated}
    end.

-spec work_time_get_exclusion(EmployeeId) -> Result when
    EmployeeId :: wta:employee_id(),
    Result :: {ok, Response :: [tuple()]}.
work_time_get_exclusion(EmployeeId) ->
    Sql = "SELECT date::TEXT, ex_type, ex_start_time::TEXT, ex_end_time::TEXT
           FROM public.logs
           WHERE employee_id = $1 AND ex_type IS NOT NULL;",
    Args = [EmployeeId],
    {ok, _Columns, Rows} = pgapp:equery(Sql, Args),
    {ok, Rows}.

-spec work_time_history(Period, EmployeeId, Limit, Offset) -> Result when
    Period :: binary(),
    EmployeeId :: wta:employee_id() | null,
    Limit :: pos_integer() | null,
    Offset :: non_neg_integer() | null,
    Result :: {ok, Response :: [tuple()]}.
work_time_history(Period, EmployeeId, Limit, Offset) ->
    Sql = "SELECT l.employee_id, l.date::TEXT, l.start_time::TEXT, l.end_time::TEXT,
               (l.end_time - l.start_time)::TEXT AS logged_time,
               l.ex_type, l.ex_start_time::TEXT, l.ex_end_time::TEXT,
               (l.ex_end_time - l.ex_start_time)::TEXT AS ex_time,
               s.is_free_schedule, s.work_days, s.start_time::TEXT, s.end_time::TEXT,
               (CASE WHEN check_if_work_day(l.ex_type, l.date, s.work_days)
                   THEN s.end_time - s.start_time END)::TEXT AS planned_time
           FROM public.logs l
           INNER JOIN public.schedules s
               ON l.employee_id = s.employee_id
           WHERE CASE WHEN $1 THEN l.date <= CURRENT_DATE
               ELSE l.date > CURRENT_DATE - INTERVAL '1 " ++ period(Period) ++ "' AND l.date <= CURRENT_DATE END "
           ++ work_time_history_sql_part(EmployeeId, Limit, Offset),
    Args = [use_all_period(Period)],
    {ok, _Columns, Rows} = pgapp:equery(Sql, Args),
    {ok, Rows}.

work_time_history_sql_part(EmployeeId, null, null) ->
    io_lib:format("AND l.employee_id = ~B;", [EmployeeId]);
work_time_history_sql_part(null, Limit, Offset) ->
    io_lib:format("ORDER BY l.employee_id, l.date LIMIT ~B OFFSET ~B;", [Limit, Offset]).

-spec work_time_statistics(Period, EmployeeId, Limit, Offset) -> Result when
    Period :: binary(),
    EmployeeId :: wta:employee_id() | null,
    Limit :: pos_integer() | null,
    Offset :: non_neg_integer() | null,
    Result :: {ok, Response :: [tuple()]}.
work_time_statistics(Period, EmployeeId, Limit, Offset) ->
    Sql = "SELECT e.id,
               SUM(CASE WHEN check_if_work_day(l.ex_type, l.date, s.work_days)
                   THEN s.end_time - s.start_time END)::TEXT AS planned_time,
               SUM(l.end_time - l.start_time)::TEXT AS logged_time,
               COUNT(CASE WHEN (l.start_time > s.start_time) AND check_if_work_day(l.ex_type, l.date, s.work_days)
                   AND s.is_free_schedule = false THEN 1 END) AS come_late,
               COUNT(CASE WHEN (l.start_time > s.start_time) AND l.ex_type = 'come_late'
                   AND l.start_time <= l.ex_end_time THEN 1 END) AS come_late_exclusion,
               COUNT(CASE WHEN (l.end_time < s.end_time) AND check_if_work_day(l.ex_type, l.date, s.work_days)
                   AND s.is_free_schedule = false THEN 1 END) AS leave_early,
               COUNT(CASE WHEN (l.end_time < s.end_time) AND l.ex_type = 'leave_early'
                   AND l.end_time >= l.ex_start_time THEN 1 END) AS leave_early_exclusion
           FROM public.employees e
           INNER JOIN public.logs l
               ON e.id = l.employee_id
           INNER JOIN public.schedules s
               ON e.id = s.employee_id
           WHERE CASE WHEN $1 THEN l.date <= CURRENT_DATE
               ELSE l.date > CURRENT_DATE - INTERVAL '1 " ++ period(Period) ++ "' AND l.date <= CURRENT_DATE END "
           ++ work_time_statistics_sql_part(EmployeeId, Limit, Offset),
    Args = [use_all_period(Period)],
    {ok, _Columns, Rows} = pgapp:equery(Sql, Args),
    {ok, Rows}.

work_time_statistics_sql_part(EmployeeId, null, null) ->
    io_lib:format("AND e.id = ~B GROUP BY e.id;", [EmployeeId]);
work_time_statistics_sql_part(null, Limit, Offset) ->
    io_lib:format("GROUP BY e.id ORDER BY e.id LIMIT ~B OFFSET ~B;", [Limit, Offset]).

%% ====================================================================
%% Internal functions
%% ====================================================================

use_all_period(?WTA_PERIOD_ALL) -> true;
use_all_period(_) -> false.

period(?WTA_PERIOD_WEEK) -> "week";
period(?WTA_PERIOD_MONTH) -> "month";
period(?WTA_PERIOD_YEAR) -> "year";
period(?WTA_PERIOD_ALL) -> "".
