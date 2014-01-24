-module(exapi_network).

-include("exapi.hrl").

-export([create/2]).
-export([get_VIFs/2]).
-export([get_PIFs/2]).

create(Pid, {Data}) ->
    exapi:request(Pid, ?NETWORK_CREATE, [{Data}]).

get_VIFs(Pid, Ref) ->
    exapi:request(Pid, ?NETWORK_GET_VIFS, [Ref]).

get_PIFs(Pid, Ref) ->
    exapi:request(Pid, ?NETWORK_GET_PIFS, [Ref]).
