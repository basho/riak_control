-module(admin_partitions).
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

-record(ring_context,{ring,
                      modules
                     }).

%% defines the webmachine routes this module handles
routes () ->
    [{admin_routes:ring_route(["partitions"]),?MODULE,[]},
     {admin_routes:ring_route(["partitions",nodename]),?MODULE,node}
    ].

%% context information shared by any wm partition resource
shared_context () ->
    {ok,Ring}=riak_core_ring_manager:get_my_ring(),
    Modules=riak_core:vnode_modules(),
    #ring_context{ring=Ring,modules=Modules}.

%% entry-point for the resource from webmachine
init ([]) ->
    {ok,shared_context()}.

%% redirect to SSL port and authenticate
is_authorized (RD,C) ->
    {true,RD,C}.

%% return the list of available content types for webmachine
content_types_provided (Req,C) ->
    {?CONTENT_TYPES,Req,C}.

%% valid | invalid | joining | leaving | exiting
to_json (Req,C=#ring_context{ring=Ring}) ->
    Indexes=riak_core_ring:all_owners(Ring),
    XS=[{struct,[{i,list_to_binary(integer_to_list(Index))},
                 {vnodes,get_vnodes(Ring,Index)},
                 {node,Node},
                 {handoffs,get_handoffs(Ring,Index)},
                 {home,net_adm:ping(Node) == pong}
                ]} || 
           {Index,Node} <- Indexes],
    {mochijson2:encode(XS),Req,C}.

%% queries all the nodes in the cluster about their handoffs (SLOW!!)
get_handoffs (Ring,Idx) ->
    Nodes=riak_core_ring:all_members(Ring),
    {Res,_}=rpc:multicall(Nodes,riak_core_handoff_manager,get_handoffs,[Idx]),
    Hoffs=lists:append([Hoffs || {ok,Hoffs} <- Res]),
    {struct,Hoffs}.

%% gets a list of vnodes for a given index
get_vnodes (Ring,Idx) ->
    Nodes=riak_core_ring:all_members(Ring),
    {Vnodes,_}=rpc:multicall(Nodes,riak_core_vnode_manager,all_vnodes,[]),
    [Type || {Type,I,_} <- lists:append(Vnodes), I == Idx].
     
