-module(exapi_vm).

-include("exapi.hrl").

-export([start/4]).
-export([clean_shutdown/2]).
-export([clean_reboot/2]).
-export([get_all/1]).
-export([get_by_name_label/2]).
-export([get_record/2]).

start(Pid, Ref, StartPaused, Force) ->
    exapi:request(Pid, ?START, [Ref, StartPaused, Force]).

clean_shutdown(Pid, Ref) ->
    exapi:request(Pid, ?CLEAN_SHUTDOWN, [Ref]).

clean_reboot(Pid, Ref) ->
    exapi:request(Pid, ?CLEAN_REBOOT, [Ref]).

get_by_name_label(Pid, Name) ->
    exapi:request(Pid, ?GET_BY_NAME_LABEL, [Name]).

get_all(Pid) ->
    exapi:request(Pid, ?VM_GET_ALL).

get_record(Pid, Ref) ->
    exapi:request(Pid, ?VM_GET_RECORD, [Ref]).
