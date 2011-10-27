-module(admin_ring).
-export([routes/0,
         init/1,
         content_types_provided/2,
         to_json/2,
         is_authorized/2,
         service_available/2,
         node_ring_details/2
        ]).

%% webmachine dependencies
-include_lib("webmachine/include/webmachine.hrl").

%% mappings to the various content types supported for this resource
-define(CONTENT_TYPES,[{"application/json",to_json}]).

%% the different vnode types we care about
-define(VNODE_TYPES,[riak_kv,riak_pipe,riak_search]).

%% defines the webmachine routes this module handles
routes () ->
    [{admin_routes:ring_route(["partitions"]),?MODULE,all},
     {admin_routes:ring_route(["partitions","filter","none"]),?MODULE,none},
     {admin_routes:ring_route(["partitions","filter","home"]),?MODULE,home},
     {admin_routes:ring_route(["partitions","filter","away"]),?MODULE,away},
     {admin_routes:ring_route(["partitions","filter","node",node]),?MODULE,node}
    ].

%% entry-point for the resource from webmachine
init (Filter) ->
    {ok,Ring}=riak_core_ring_manager:get_my_ring(),
    {ok,{Ring,Filter}}.

%% redirect to SSL port if using HTTP
service_available (RD,C) ->
    riak_control_security:scheme_is_available(RD,C).

%% validate username and password
is_authorized (RD,C) ->
    riak_control_security:enforce_auth(RD,C).

%% return the list of available content types for webmachine
content_types_provided (Req,C) ->
    {?CONTENT_TYPES,Req,C}.

%% valid | invalid | joining | leaving | exiting
to_json (Req,C={Ring,Filter}) ->
    Partitions=riak_core_ring:all_owners(Ring),
    PS=filter_partitions(Req,Partitions,Filter),
    Details=[{struct,node_ring_details(Ring,P)} || P <- PS],
    {mochijson2:encode(Details),Req,C}.

%% filter a ring based on a given filter name
filter_partitions (Req,PS,node) ->
    Node=list_to_atom(dict:fetch(node,wrq:path_info(Req))),
    [P || P={_,N} <- PS, N==Node];
filter_partitions (_Req,PS,all) ->
    PS;
filter_partitions (_Req,PS,home) ->
    PS; % todo
filter_partitions (_Req,PS,away) ->
    PS; % todo
filter_partitions (_Req,_PS,_) ->
    [].

%% return a proplist of details for a given index
node_ring_details (Ring,{Idx,Node}) ->
    [{i,partition_index(Ring,Idx)},
     {vnodes,get_vnode_statuses(Ring,Idx)},
     {node,Node},
     {handoffs,get_handoffs(Ring,Idx)},
     {online,net_adm:ping(Node) == pong}
    ].

%% get the partition number of a given index
partition_index (Ring,Idx) ->
    NumPartitions=riak_core_ring:num_partitions(Ring),
    Inc=chash:ring_increment(NumPartitions),
    ((Idx div Inc) + 1) rem NumPartitions.

%% queries all the nodes in the cluster about their handoffs (SLOW!!)
get_handoffs (Ring,Idx) ->
    Nodes=riak_core_ring:all_members(Ring),
    {Res,_}=rpc:multicall(Nodes,riak_core_handoff_manager,get_handoffs,[Idx]),
    Hoffs=lists:append([Hoffs || {ok,Hoffs} <- Res]),
    {struct,Hoffs}.

%% gets a list of vnodes for a given index
get_vnode_statuses (Ring,Idx) ->
    [get_vnode_status(Ring,Idx,Type) || Type <- ?VNODE_TYPES].

%% lookup the status for a given vnode type for a partition
get_vnode_status (Ring,Idx,VnodeType) ->
    UpNodes=riak_core_node_watcher:nodes(VnodeType),
    case riak_core_apl:get_apl_ann(<<(Idx-1):160>>,1,Ring,UpNodes) of
        [{{_,Node},Status}|_] -> 
            case is_vnode_running(Node,Idx,VnodeType) of
                true -> {VnodeType,Status};
                false -> {VnodeType,offline}
            end;
        [] -> {VnodeType,offline}
    end.

%% check to see if a particular index vnode worker is running
is_vnode_running (Node,Idx,Type) ->
    case rpc:call(Node,riak_core_vnode_manager,all_vnodes,[]) of
        {badrpc,_Reason} -> 
            false;
        Vnodes ->
            WorkerType=proplists:get_value(Type,riak_core:vnode_modules()),

            %% just check for any worker vnode that matches the type
            [P || {T,I,P} <- Vnodes, (T==WorkerType) and (I==Idx)] =/= []
    end.
