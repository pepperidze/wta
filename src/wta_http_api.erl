-module(wta_http_api).

-include("wta.hrl").

-export([
    handle/2
]).

%% ====================================================================
%% API
%% ====================================================================

handle(Args, Path) ->
    case Path of
        <<"/employee/create">> ->
            wta_api:employee_create(Args);
        <<"/card/create">> ->
            wta_api:card_create(Args);
        <<"/card/assign">> ->
            wta_api:card_assign(Args);
        <<"/card/touch">> ->
            wta_api:card_touch(Args);
        <<"/card/delete">> ->
            wta_api:card_delete(Args);
        <<"/card/delete_all_by_employee">> ->
            wta_api:card_delete_all_by_employee(Args);
        <<"/card/list_by_employee">> ->
            wta_api:card_list_by_employee(Args);
        <<"/work_time/set">> ->
            wta_api:work_time_set(Args);
        <<"/work_time/get">> ->
            wta_api:work_time_get(Args);
        <<"/work_time/add_exclusion">> ->
            wta_api:work_time_add_exclusion(Args);
        <<"/work_time/get_exclusion">> ->
            wta_api:work_time_get_exclusion(Args);
        <<"/work_time/history_by_employee">> ->
            wta_api:work_time_history_by_employee(Args);
        <<"/work_time/history">> ->
            wta_api:work_time_history(Args);
        <<"/work_time/statistics_by_employee">> ->
            wta_api:work_time_statistics_by_employee(Args);
        <<"/work_time/statistics">> ->
            wta_api:work_time_statistics(Args);
        Path ->
            {error, not_found}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================
