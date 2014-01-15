-module(exapi).

-behaviour(gen_server).

-include("exapi.hrl").

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([start_link/2]).
-export([start_link/3]).
-export([start_link/4]).
-export([stop/1]).

-export([request/2]).
-export([request/3]).

start_link(Host, Password) ->
    start_link(Host, Password, ?USER).

start_link(Host, Password, Login) ->
    start_link(Host, Password, Login, ?PORT).

start_link(Host, Password, Login, Port) ->
    gen_server:start_link(?MODULE, {Host, Password, Login, Port}, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init({Host, Password, Login, Port}) ->
    SessionRef = xapi_get_session(Host, Password, Login, Port),
    {ok, #state{sref = SessionRef, host = Host, port = Port}}.

handle_call({call, Req, Params}, _From, State) ->
    Result = xapi_request(Req, Params, State),
    {reply, Result, State};

handle_call(stop, _From, State) ->
    _Result = xapi_request(?LOGOUT, [], State),
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

request(Pid, Method) ->
    request(Pid, Method, []).

request(Pid, Method, Params) ->
    gen_server:call(Pid, {call, Method, Params}, infinity).

xapi_get_session(Host, Password, Login, Port) ->
    {ok, {response, [{struct, Result}]}} = xmlrpc:call(Host, Port, "/", {call, ?LOGIN, [Login, Password]}),
    proplists:get_value('Value', Result).

xapi_request(Method, Params, State) ->
    xapi_request(State#state.host, State#state.port, Method, Params, State).

xapi_request(Host, Port, Method, Params, State) ->
    ReqResult = xmlrpc:call(Host, Port, "/", {call, Method, [State#state.sref | Params]}),
    %% io:format("~p~n", [ReqResult]),
    Result = case parse_xapi_request(ReqResult) of
                 {error, unknown} -> error;
                 PreResult -> case proplists:get_value('Value', PreResult) of
                                  {array, List} -> List;
                                  {struct, Struct} -> Struct;
                                  String -> String
                              end
             end,
    Result.

parse_xapi_request(ReqResult) ->
    Result = case ReqResult of
                 {ok,{response,[{struct, PreResult}]}} -> PreResult;
                 _Other -> {error, unknown}
             end,
    Result.
