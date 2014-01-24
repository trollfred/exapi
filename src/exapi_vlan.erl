-module(exapi_vlan).

-include("exapi.hrl").

-export([create/2]).

create(Pid, {Data}) ->
    exapi:request(Pid, ?VLAN_CREATE, [{Data}]).