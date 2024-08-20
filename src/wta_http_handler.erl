-module(wta_http_handler).

-behavior(cowboy_handler).

-include("wta.hrl").

-export([
    init/2,
    terminate/3
]).

%% ====================================================================
%% cowboy_handler callbacks
%% ====================================================================

init(Req0, State) ->
    #{
        method := Method,
        path := Path,
        has_body := HasBody
    } = Req0,
    Method = cowboy_req:method(Req0),
    HasBody = cowboy_req:has_body(Req0),
    Path = cowboy_req:path(Req0),
    Req = step_check_method(Method, HasBody, Path, Req0),
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%% ====================================================================
%% Internal
%% ====================================================================

step_check_method(<<"POST">>, true, Path, Req0) ->
    {ok, ReqBody, Req} = cowboy_req:read_urlencoded_body(Req0),
    {RspCode, RspHeaders, RspBody} = try
        step_validate(ReqBody, Path)
    catch C:R:S ->
        lager:error("[wta] internal error ~p", [{C, R, S}]),
        {500, #{}, <<"Internal Server Error">>}
    end,
    cowboy_req:reply(RspCode, RspHeaders, RspBody, Req);
step_check_method(<<"POST">>, false, _Path, Req0) ->
    cowboy_req:reply(400, #{}, <<"Missing Body">>, Req0);
step_check_method(_Method, _HasBody, _Path, Req0) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

step_validate(ReqBody, Path) ->
    Schema = wta_http_validator:schema(Path),
    case liver:validate(Schema, transform_req_body(ReqBody), #{return => map}) of
        {ok, Args} ->
            step_handle(Args, Path);
        {error, Error} ->
            lager:error("[wta] validation error ~p~n", [Error]),
            {400, #{}, <<"Validation Error">>}
    end.

step_handle(Args, Path) ->
    case wta_http_api:handle(Args, Path) of
        {error, not_found} ->
            {404, #{}, <<"Endpoint Not Found">>};
        Result ->
            RspTerm = wrap_response(Result),
            Rsp = jsx:encode(RspTerm),
            lager:info("[wta] http req ~p~nwith args ~p~nrsp ~p", [Path, Args, RspTerm]),
            {200, #{<<"Content-Type">> => <<"application/json">>}, Rsp}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

wrap_response(ok) ->
    wrap_response({ok, #{}});
wrap_response({ok, Data}) ->
    [{<<"status">>, <<"ok">>}, {<<"data">>, Data}];
wrap_response({error, Message}) ->
    [{<<"status">>, <<"error">>}, {<<"message">>, Message}].

transform_req_body(ReqBody) ->
    [{Key, transform(Value)} || {Key, Value} <- ReqBody].

transform(<<"null">>) -> null;
transform(<<"true">>) -> true;
transform(<<"false">>) -> false;
transform(Value) -> Value.
