-module(exapi_vdi).

-include("exapi.hrl").

-export([clone/3]).
-export([copy/3]).
-export([create/2]).
-export([destroy/2]).
-export([get_by_name_label/2]).
-export([get_is_a_snapshot/2]).
-export([get_uuid/2]).
-export([get_VBDs/2]).
-export([get_virtual_size/2]).
-export([resize/3]).
-export([set_name_description/3]).
-export([set_name_label/3]).
-export([snapshot/3]).


clone(Pid, Ref, {Map}) ->
    exapi:request(Pid, ?VDI_CLONE, [Ref, {Map}]).

copy(Pid, Ref, SR) ->
    exapi:request(Pid, ?VDI_COPY, [Ref, SR]).

create(Pid, {Data}) ->
    exapi:request(Pid, ?VDI_CREATE, [{Data}]).

destroy(Pid, Ref) ->
    exapi:request(Pid, ?VDI_DESTROY, [Ref]).

get_by_name_label(Pid, Name) ->
    exapi:request(Pid, ?VDI_GET_BY_NAME_LABEL, [Name]).

get_is_a_snapshot(Pid, Ref) ->
    exapi:request(Pid, ?VDI_GET_IS_A_SNAPSHOT, [Ref]).

get_uuid(Pid, Ref) ->
    exapi:request(Pid, ?VDI_GET_UUID, [Ref]).

get_VBDs(Pid, Ref) ->
    exapi:request(Pid, ?VDI_GET_VBDS, [Ref]).

get_virtual_size(Pid, Ref) ->
    exapi:request(Pid, ?VDI_GET_VIRTUAL_SIZE, [Ref]).

resize(Pid, Ref, Value) ->
    exapi:request(Pid, ?VDI_RESIZE, [Ref, Value]).

set_name_description(Pid, Ref, Value) ->
    exapi:request(Pid, ?VDI_SET_NAME_DESCRIPTION, [Ref, Value]).

set_name_label(Pid, Ref, Value) ->
    exapi:request(Pid, ?VDI_SET_NAME_LABEL, [Ref, Value]).

snapshot(Pid, Ref, {Map}) ->
    exapi:request(Pid, ?VDI_SNAPSHOT, [Ref, {Map}]).
