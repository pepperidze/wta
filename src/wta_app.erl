-module(wta_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

%% ====================================================================
%% Application callbacks
%% ====================================================================

start(_Type, _Args) ->
    wta_liver_rules:init_custom_rules(),
    start_listener(),
    wta_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(wta_http_listener).

%% ====================================================================
%% Internal functions
%% ====================================================================

start_listener() ->
    Port = wta_conf:port(),
    Routes = [
        {'_', [
            {"/[...]", wta_http_handler, []}
        ]}
    ],
    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_clear(wta_http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ).
