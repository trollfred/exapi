-module(exapi).

-behaviour(gen_server).

-include("exapi.hrl").

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([start_link/1]).
-export([start_link/2]).
-export([start_link/3]).
-export([start_link/4]).
-export([stop/1]).

-export([request/2]).
-export([request/3]).

start_link(Path) ->
    gen_server:start_link(?MODULE, {local, {Path, ?USER}}, []).

start_link(Host, Password) ->
    start_link(Host, Password, ?USER).

start_link(Host, Password, Login) ->
    start_link(Host, Password, Login, ?PORT).

start_link(Host, Password, Login, Port) ->
    gen_server:start_link(?MODULE, {remote, {Host, Password, Login, Port}}, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init({local, {Path, Login}}) ->
    SessionRef = xapi_get_session(Path, Login),
    {ok, #state{sref = SessionRef, connection = {local, {Path, Login}}}};

init({remote,{Host, Password, Login, Port}}) ->
    SessionRef = xapi_get_session(Host, Password, Login, Port),
    {ok, #state{sref = SessionRef, connection = {remote, {Host, Password, Login, Port}}}}.

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

xapi_get_session(Path, Login) ->
    {ok, {response, [{struct, Result}]}} = xmlrpc:call(Path, "/", {call, ?LOGIN, [Login, ""]}),
    proplists:get_value("Value", Result).

xapi_get_session(Host, Password, Login, Port) ->
    {ok, {response, [{struct, Result}]}} = xmlrpc:call(Host, Port, "/", {call, ?LOGIN, [Login, Password]}),
    proplists:get_value("Value", Result).

xapi_request(Method, Params, #state{connection = {local, {Path, _Login}}} = State) ->
    xapi_request(Path, Method, Params, State);

xapi_request(Method, Params, #state{connection = {remote, {Host, _Password, _Login, Port}}} = State) ->
    xapi_request(Host, Port, Method, Params, State).

xapi_request(Path, Method, Params, State) ->
    ReqResult = xmlrpc:call(Path, "/", {call, Method, [State#state.sref | Params]}),
    parse_xapi_request(ReqResult).

xapi_request(Host, Port, Method, Params, State) ->
    ReqResult = xmlrpc:call(Host, Port, "/", {call, Method, [State#state.sref | Params]}),
    parse_xapi_request(ReqResult).

parse_xapi_request(ReqResult) ->
    One = case ReqResult of
                 {ok,{response,[{struct, PreResult}]}} -> PreResult;
                 _Other -> {error, unknown}
             end,
    Two = case One of
                 {error, unknown} -> {error, unknown};
                 Three -> case proplists:get_value("Value", Three) of
                                  {array, List} -> List;
                                  {struct, Struct} -> Struct;
                                  String -> String
                              end
             end,
    Two.