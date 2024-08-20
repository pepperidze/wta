-module(wta_db_tests).

-include_lib("eunit/include/eunit.hrl").

-define(non_existing_id, 999).
-define(non_existing_uid, <<"999">>).

setup() ->
    {ok, _} = application:ensure_all_started(pgapp),
    {ok, _, _} = pgapp:equery("TRUNCATE TABLE public.employees CASCADE;", []).

cleanup(_) ->
    ok = application:stop(pgapp).

complex_test_() -> {setup, fun setup/0, fun cleanup/1, [
    {"Complex employee & card Test",
        fun() ->
            false = wta_db:employee_exists(?non_existing_id),
            {ok, EmployeeId} = wta_db:employee_create(<<"A">>, <<"B">>, <<"C">>, 1001),
            {error, employee_already_exists} = wta_db:employee_create(<<"A">>, <<"B">>, <<"C">>, 1001),
            true = wta_db:employee_exists(EmployeeId),
            {ok, CardUID1} = wta_db:card_create(<<"Internal_1">>),
            {error, card_not_assigned} = wta_db:employee_get_by_card_uid(CardUID1),
            {error, card_not_exists} = wta_db:employee_get_by_card_uid(?non_existing_uid),
            false = wta_db:card_exists(?non_existing_uid),
            true = wta_db:card_exists(CardUID1),
            true = wta_db:card_is_valid_to_assign(CardUID1),
            ok = wta_db:card_assign(EmployeeId, CardUID1),
            {error, not_updated} = wta_db:card_assign(EmployeeId, CardUID1),
            {error, _} = wta_db:card_assign(EmployeeId, ?non_existing_uid),
            false = wta_db:card_is_valid_to_assign(CardUID1),
            {ok, EmployeeId} = wta_db:employee_get_by_card_uid(CardUID1),
            {ok, [CardUID1]} = wta_db:card_list_by_employee_id(EmployeeId),
            {ok, EmployeeId} = wta_db:card_delete_by_uid(CardUID1),
            {error, card_not_exists} = wta_db:card_delete_by_uid(CardUID1),
            {error, card_not_exists} = wta_db:card_delete_by_uid(?non_existing_uid),
            {ok, []} = wta_db:card_delete_by_employee_id(EmployeeId),
            {ok, []} = wta_db:card_list_by_employee_id(EmployeeId),
            {ok, CardUID2} = wta_db:card_create(<<"Internal_2">>),
            ok = wta_db:card_assign(EmployeeId, CardUID2),
            {ok, [CardUID2]} = wta_db:card_delete_by_employee_id(EmployeeId),
            false = wta_db:card_exists(CardUID2),
            {ok, CardUID3} = wta_db:card_create(<<"Internal_3">>),
            ok = wta_db:card_touch(EmployeeId, CardUID3),
            timer:sleep(1000),
            ok = wta_db:card_touch(EmployeeId, CardUID3)
        end
    }
]}.

init_test_() -> {setup, fun setup/0, fun cleanup/1, [
    {"", fun() -> check_work_time_history_statistics() end},
    {"", fun() -> not_insert_exclusion_for_non_working_days() end},
    {"", fun() -> not_insert_exclusion_for_free_schedule() end}
]}.

init_logs_for_stat(EmployeeId, CardUID) ->
    Sql = "INSERT INTO public.logs
           (employee_id, card_uid, date, start_time, end_time, ex_type, ex_start_time, ex_end_time) VALUES

            -- without late or early
           ($1, $2, '2024-01-01', '09:00:00', '18:00:00', null, null, null),
           ($1, $2, '2024-01-02', '08:00:00', '18:00:00', null, null, null),
           ($1, $2, '2024-01-03', '09:00:00', '19:00:00', null, null, null),
           ($1, $2, '2024-01-04', '08:00:00', '19:00:00', null, null, null),

           -- late without exlusion
           ($1, $2, '2024-01-05', '09:30:00', '18:00:00', null, null, null),

           -- late with exlusion
           ($1, $2, '2024-01-06', '09:30:00', '18:00:00', 'come_late', '09:00:00', '10:00:00'),
           ($1, $2, '2024-01-07', '10:00:00', '18:00:00', 'come_late', '09:00:00', '10:00:00'),

           -- late is more than exlusion
           ($1, $2, '2024-01-08', '10:30:00', '18:00:00', 'come_late', '09:00:00', '10:00:00'),

           -- without late
           ($1, $2, '2024-01-09', '09:00:00', '18:00:00', 'come_late', '09:00:00', '10:00:00'),
           ($1, $2, '2024-01-10', '08:30:00', '18:00:00', 'come_late', '09:00:00', '10:00:00'),

           -- late, but another exlusion
           ($1, $2, '2024-01-11', '09:30:00', '18:00:00', 'leave_early', '17:00:00', '18:00:00'),

           -- early without exlusion
           ($1, $2, '2024-01-12', '09:00:00', '17:30:00', null, null, null),

           -- early with exlusion
           ($1, $2, '2024-01-13', '09:00:00', '17:30:00', 'leave_early', '17:00:00', '18:00:00'),
           ($1, $2, '2024-01-14', '09:00:00', '17:00:00', 'leave_early', '17:00:00', '18:00:00'),

           -- early is more than exlusion
           ($1, $2, '2024-01-15', '09:00:00', '16:30:00', 'leave_early', '17:00:00', '18:00:00'),

           -- without early
           ($1, $2, '2024-01-16', '09:00:00', '18:00:00', 'leave_early', '17:00:00', '18:00:00'),
           ($1, $2, '2024-01-17', '09:00:00', '18:30:00', 'leave_early', '17:00:00', '18:00:00'),

           -- early, but another exlusion
           ($1, $2, '2024-01-18', '09:00:00', '17:30:00', 'come_late', '09:00:00', '10:00:00'),

           -- both without exlusion
           ($1, $2, '2024-01-19', '09:30:00', '17:30:00', null, null, null),
           ($1, $2, '2024-01-20', '09:30:00', '17:30:00', null, null, null),

           -- vacation with time before/after schedule
           ($1, $2, '2024-01-21', '06:00:00', '08:00:00', 'vacation', null, null),
           ($1, $2, '2024-01-22', '21:00:00', '23:00:00', 'vacation', null, null)
    ;",
    Args = [EmployeeId, CardUID],
    {ok, _} = pgapp:equery(Sql, Args),
    ok.

check_work_time_history_statistics() ->
    StartTime = {9, 0, 0},
    EndTime = {18, 0, 0},
    WorkDays = [1, 2, 3, 4, 5, 6, 7],
    IsFreeSchedule = false,

    {ok, EmployeeId} = wta_db:employee_create(<<"A">>, <<"B">>, <<"C">>, 1001),
    {ok, CardUID} = wta_db:card_create(<<"Internal">>),
    ok = wta_db:card_assign(EmployeeId, CardUID),
    ok = wta_db:work_time_set(EmployeeId, StartTime, EndTime, WorkDays, IsFreeSchedule),
    ok = init_logs_for_stat(EmployeeId, CardUID),
    {ok, [{EmployeeId, <<"180:00:00">>, <<"179:00:00">>, 7, 2, 7, 2}]
    } = wta_db:work_time_statistics(<<"all">>, EmployeeId, null, null),
    {ok,[{EmployeeId, <<"2024-01-22">>, <<"21:00:00">>, <<"23:00:00">>, <<"02:00:00">>, <<"vacation">>, null, null, null,
        false, [1,2,3,4,5,6,7], <<"09:00:00">>, <<"18:00:00">>, null}]} = wta_db:work_time_history(<<"all">>, null, 1, 21).

not_insert_exclusion_for_non_working_days() ->
    StartTime = {9, 0, 0},
    EndTime = {18, 0, 0},
    WorkDays = [1, 2, 3],
    IsFreeSchedule = false,
    ContractId = 1002,

    EmployeeId = init_employee(ContractId, StartTime, EndTime, WorkDays, IsFreeSchedule),

    ok = wta_db:work_time_add_exclusion(EmployeeId, {2024, 8, 19}, <<"come_late">>, {9, 0, 0}, {10, 0, 0}),
    ok = wta_db:work_time_add_exclusion(EmployeeId, {2024, 8, 20}, <<"leave_early">>, {17, 0, 0}, {18, 0, 0}),
    ok = wta_db:work_time_add_exclusion(EmployeeId, {2024, 8, 21}, <<"vacation">>, null, null),

    {error, not_updated} = wta_db:work_time_add_exclusion(EmployeeId, {2024, 8, 22}, <<"come_late">>, {9, 0, 0}, {10, 0, 0}),
    {error, not_updated} = wta_db:work_time_add_exclusion(EmployeeId, {2024, 8, 23}, <<"leave_early">>, {17, 0, 0}, {18, 0, 0}),
    {error, not_updated} = wta_db:work_time_add_exclusion(EmployeeId, {2024, 8, 24}, <<"vacation">>, null, null).

not_insert_exclusion_for_free_schedule() ->
    StartTime = {9, 0, 0},
    EndTime = {18, 0, 0},
    WorkDays = [1, 2, 3],
    IsFreeSchedule = true,
    ContractId = 1003,

    EmployeeId = init_employee(ContractId, StartTime, EndTime, WorkDays, IsFreeSchedule),

    {error, not_updated} = wta_db:work_time_add_exclusion(EmployeeId, {2024, 8, 19}, <<"come_late">>, {9, 0, 0}, {10, 0, 0}),
    {error, not_updated} = wta_db:work_time_add_exclusion(EmployeeId, {2024, 8, 20}, <<"leave_early">>, {17, 0, 0}, {18, 0, 0}),
    ok = wta_db:work_time_add_exclusion(EmployeeId, {2024, 8, 21}, <<"vacation">>, null, null),

    {error, not_updated} = wta_db:work_time_add_exclusion(EmployeeId, {2024, 8, 22}, <<"come_late">>, {9, 0, 0}, {10, 0, 0}),
    {error, not_updated} = wta_db:work_time_add_exclusion(EmployeeId, {2024, 8, 23}, <<"leave_early">>, {17, 0, 0}, {18, 0, 0}),
    {error, not_updated} = wta_db:work_time_add_exclusion(EmployeeId, {2024, 8, 24}, <<"vacation">>, null, null).

init_employee(ContractId, StartTime, EndTime, WorkDays, IsFreeSchedule) ->
    {ok, EmployeeId} = wta_db:employee_create(<<"A">>, <<"B">>, <<"C">>, ContractId),
    {ok, CardUID} = wta_db:card_create(<<"Internal">>),
    ok = wta_db:card_assign(EmployeeId, CardUID),
    ok = wta_db:work_time_set(EmployeeId, StartTime, EndTime, WorkDays, IsFreeSchedule),
    EmployeeId.
