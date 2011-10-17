-module(admin_cluster).
-export([routes/0,
         init/1,
         content_types_provided/2,
         to_json/2,
         is_authorized/2
        ]).

%% webmachine dependencies
-include_lib("webmachine/include/webmachine.hrl").

%% mappings to the various content types supported for this resource
-define(CONTENT_TYPES,[{"application/json",to_json}]).

%% defines the webmachine routes this module handles
routes () ->
    [{admin_routes:cluster_route(),?MODULE,[]}].

%% entry-point for the resource from webmachine
init ([]) ->
    {ok,Ring}=riak_core_ring_manager:get_my_ring(),
    {ok,riak_core_ring:all_member_status(Ring)}.

%% redirect to SSL port and authenticate
is_authorized (RD,C) ->
    {true,RD,C}.

%% return the list of available content types for webmachine
content_types_provided (Req,Ring) ->
    {?CONTENT_TYPES,Req,Ring}.

%% check to see if the node is reachable (down, partitioned, etc)
get_port (Node) ->
    case rpc:call(Node,application,get_env,[riak_core,http]) of
        {ok,[{_,Port}|_]} ->
            [{"reachable",true},{"port",Port}];
        _ -> 
            [{"reachable",false}]
    end.

%% valid | invalid | joining | leaving | exiting
to_json (Req,Ring) ->
    NodeStatus=[{struct,get_port(N) ++ [{"name",N},
                                        {"status",S}
                                       ]} 
                || {N,S} <- Ring],
    {mochijson2:encode(NodeStatus),Req,Ring}.

