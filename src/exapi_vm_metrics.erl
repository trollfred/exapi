-module(exapi_vm_metrics).

-include("exapi.hrl").

-export([get_record/2]).

get_record(Pid, Ref) ->
    exapi:request(Pid, ?VM_METRICS_GET_RECORD, [Ref]).
