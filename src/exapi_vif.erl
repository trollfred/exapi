-module(exapi_vif).

-include("exapi.hrl").

-export([add_ipv4_allowed/3]).
-export([add_ipv6_allowed/3]).
-export([create/2]).
-export([destroy/2]).
-export([get_MAC/2]).
-export([get_record/2]).
-export([remove_ipv4_allowed/3]).
-export([set_ipv4_allowed/3]).
-export([set_locking_mode/3]).
-export([set_qos_algorithm_params/3]).

add_ipv4_allowed(Pid, Ref, Address) ->
    exapi:request(Pid, ?VIF_ADD_IPV4_ALLOWED, [Ref, Address]).

add_ipv6_allowed(Pid, Ref, Address) ->
    exapi:request(Pid, ?VIF_ADD_IPV6_ALLOWED, [Ref, Address]).

create(Pid, {Data}) ->
    exapi:request(Pid, ?VIF_CREATE, [{Data}]).

destroy(Pid, Ref) ->
    exapi:request(Pid, ?VIF_DESTROY, [Ref]).

get_MAC(Pid, Ref) ->
    exapi:request(Pid, ?VIF_GET_MAC, [Ref]).

get_record(Pid, Ref) ->
    exapi:request(Pid, ?VIF_GET_RECORD, [Ref]).

remove_ipv4_allowed(Pid, Ref, Address) ->
    exapi:request(Pid, ?VIF_REMOVE_IPV4_ALLOWED, [Ref, Address]).

set_ipv4_allowed(Pid, Ref, Address) ->
    exapi:request(Pid, ?VIF_SET_IPV4_ALLOWED, [Ref, Address]).

set_locking_mode(Pid, Ref, Mode) ->
    exapi:request(Pid, ?VIF_SET_LOCKING_MODE, [Ref, Mode]).

set_qos_algorithm_params(Pid, Ref, {Params}) ->
    exapi:request(Pid, ?VIF_SET_QOS_ALGORITHM_PARAMS, [Ref, {Params}]).
