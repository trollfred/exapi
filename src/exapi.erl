-module(exapi).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/2, stop/0]).
-export([start_sync/0]).
-export([request/1, request/2]).

-record(sref, {sref = "", host = ""}).

start(Host, AuthInfo) ->
    {ok, _Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, [Host, AuthInfo], []),
    ok.

start_sync() ->
    application:start(sync).

stop() ->
    gen_server:cast(?SERVER, logout).

request(Req) ->
    Result = gen_server:call(?SERVER, {call, Req}),
    Result.

request(Req, Params) ->
    Result = gen_server:call(?SERVER, {call, Req, Params}),
    Result.

init([Host, AuthInfo]) ->
    {ok, {response, [{struct, Result}]}} = xmlrpc:call(Host, 80, "/", {call, session.login_with_password, AuthInfo}),
    SessionRef = proplists:get_value('Value', Result),
    {ok, #sref{sref = SessionRef, host = Host}}.

handle_call({call, Req}, _From, State) ->
    {ok,{response,[{struct,[_, {_,{struct, [{_, {struct, Result}}]}}]}]}} = xmlrpc:call(State#sref.host, 80, "/", {call, Req, [State#sref.sref]}),
    {reply, Result, State};

handle_call({call, Req, Params}, _From, State) ->
    Result = xmlrpc:call(State#sref.host, 80, "/", {call, Req, [State#sref.sref, Params]}),
    {reply, Result, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
