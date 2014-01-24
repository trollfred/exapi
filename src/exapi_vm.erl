-module(exapi_vm).

-include("exapi.hrl").

-export([start/4]).
-export([clean_shutdown/2]).
-export([clean_reboot/2]).
-export([get_all/1]).
-export([get_by_name_label/2]).
-export([get_record/2]).
-export([clone/3]).
-export([hard_reboot/2]).
-export([hard_shutdown/2]).
-export([add_to_other_config/4]).
-export([destroy/2]).
-export([get_domid/2]).
-export([get_HVM_boot_policy/2]).
-export([get_power_state/2]).
-export([get_PV_args/2]).
-export([get_VBDs/2]).
-export([get_VIFs/2]).
-export([provision/2]).
-export([remove_from_other_config/3]).
-export([set_HVM_boot_policy/3]).
-export([set_memory_limits/3]).
-export([set_name_description/3]).
-export([set_name_label/3]).
-export([set_PV_args/3]).
-export([set_PV_bootloader/3]).
-export([set_VCPUs_at_startup/3]).
-export([set_VCPUs_max/3]).
-export([set_VCPUs_params/3]).

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

clone(Pid, Ref, CloneName) ->
    exapi:request(Pid, ?VM_CLONE, [Ref, CloneName]).

hard_reboot(Pid, Ref) ->
    exapi:request(Pid, ?VM_HARD_REBOOT, [Ref]).

hard_shutdown(Pid, Ref) ->
    exapi:request(Pid, ?VM_HARD_SHUTDOWN, [Ref]).

add_to_other_config(Pid, Ref, Field, {Value}) ->
    exapi:request(Pid, ?VM_ADD_TO_OTHER_CONFIG, [Ref, Field, {Value}]).

destroy(Pid, Ref) ->
    exapi:request(Pid, ?VM_DESTROY, [Ref]).

get_domid(Pid, Ref) ->
    exapi:request(Pid, ?VM_GET_DOMID, [Ref]).

get_HVM_boot_policy(Pid, Ref) ->
    exapi:request(Pid, ?VM_GET_HVM_BOOT_POLICY, [Ref]).

get_power_state(Pid, Ref) ->
    exapi:request(Pid, ?VM_GET_POWER_STATE, [Ref]).

get_PV_args(Pid, Ref) ->
    exapi:request(Pid, ?VM_GET_PV_ARGS, [Ref]).

get_VBDs(Pid, Ref) ->
    exapi:request(Pid, ?VM_GET_VBDS, [Ref]).

get_VIFs(Pid, Ref) ->
    exapi:request(Pid, ?VM_GET_VIFS, [Ref]).

provision(Pid, Ref) ->
    exapi:request(Pid, ?VM_PROVISION, [Ref]).

remove_from_other_config(Pid, Ref, Field) ->
    exapi:request(Pid, ?VM_REMOVE_FROM_OTHER_CONFIG, [Ref, Field]).

set_HVM_boot_policy(Pid, Ref, Value) ->
    exapi:request(Pid, ?VM_SET_HVM_BOOT_POLICY, [Ref, Value]).

set_memory_limits(Pid, Ref, Value) ->
    exapi:request(Pid, ?VM_SET_MEMORY_LIMITS, [Ref, Value]).

set_name_description(Pid, Ref, Value) ->
    exapi:request(Pid, ?VM_SET_NAME_DESCRIPTION, [Ref, Value]).

set_name_label(Pid, Ref, Value) ->
    exapi:request(Pid, ?VM_SET_NAME_LABEL, [Ref, Value]).

set_PV_args(Pid, Ref, Value) ->
    exapi:request(Pid, ?VM_SET_PV_ARGS, [Ref, Value]).

set_PV_bootloader(Pid, Ref, Value) ->
    exapi:request(Pid, ?VM_SET_PV_BOOTLOADER, [Ref, Value]).

set_VCPUs_at_startup(Pid, Ref, Value) ->
    exapi:request(Pid, ?VM_SET_VCPUS_AT_STARTUP, [Ref, Value]).

set_VCPUs_max(Pid, Ref, Value) ->
    exapi:request(Pid, ?VM_SET_VCPUS_MAX, [Ref, Value]).

set_VCPUs_params(Pid, Ref, Value) ->
    exapi:request(Pid, ?VM_SET_VCPUS_PARAMS, [Ref, Value]).
