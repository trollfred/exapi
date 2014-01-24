-module(exapi_vbd).

-include("exapi.hrl").

-export([create/2]).
-export([destroy/2]).
-export([get_currently_attached/2]).
-export([get_mode/2]).
-export([get_record/2]).
-export([get_type/2]).
-export([get_userdevice/2]).
-export([get_VDI/2]).
-export([plug/2]).
-export([unplug/2]).
-export([unplug_force/2]).

create(Pid, {Data}) ->
    exapi:request(Pid, ?VBD_CREATE, [{Data}]).

destroy(Pid, Ref) ->
    exapi:request(Pid, ?VBD_DESTROY, [Ref]).

get_currently_attached(Pid, Ref) ->
    exapi:request(Pid, ?VBD_GET_CURRENTLY_ATTACHED, [Ref]).

get_mode(Pid, Ref) ->
    exapi:request(Pid, ?VBD_GET_MODE, [Ref]).

get_record(Pid, Ref) ->
    exapi:request(Pid, ?VBD_GET_RECORD, [Ref]).

get_type(Pid, Ref) ->
    exapi:request(Pid, ?VBD_GET_TYPE, [Ref]).

get_userdevice(Pid, Ref) ->
    exapi:request(Pid, ?VBD_GET_USERDEVICE, [Ref]).

get_VDI(Pid, Ref) ->
    exapi:request(Pid, ?VBD_GET_VDI, [Ref]).

plug(Pid, Ref) ->
    exapi:request(Pid, ?VBD_PLUG, [Ref]).

unplug(Pid, Ref) ->
    exapi:request(Pid, ?VBD_UNPLUG, [Ref]).

unplug_force(Pid, Ref) ->
    exapi:request(Pid, ?VBD_UNPLUG_FORCE, [Ref]).
