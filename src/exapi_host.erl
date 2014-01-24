-module(exapi_host).

-include("exapi.hrl").

-export([call_plugin/5]).
-export([get_by_name_label/2]).
-export([get_record/2]).

call_plugin(Pid, Ref, Plugin, Function, {Data}) ->
    exapi:request(Pid, ?HOST_CALL_PLUGIN, [Ref, Plugin, Function, {Data}]).

get_by_name_label(Pid, Name) ->
    exapi:request(Pid, ?HOST_GET_BY_NAME_LABEL, [Name]).

get_record(Pid, Ref) ->
    exapi:request(Pid, ?HOST_GET_RECORD, [Ref]).
