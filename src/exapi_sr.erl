-module(exapi_sr).

-include("exapi.hrl").

-export([get_uuid/2]).

get_uuid(Pid, Ref) ->
    exapi:request(Pid, ?SR_GET_UUID, [Ref]).