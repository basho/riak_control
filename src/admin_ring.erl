-module(admin_ring).
-export([routes/0,
         init/1,
         content_types_provided/2,
         to_json/2,
         is_authorized/2,
         service_available/2
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
     {admin_routes:ring_route(["partitions","filter","node",node]),?MODULE,node}
    ].

%% entry-point for the resource from webmachine
init (Filter) ->
    {ok,_V,Partitions}=riak_control_session:get_partitions(),
    {ok,{Partitions,Filter}}.

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
to_json (Req,C={Partitions,Filter}) ->
    PS=filter_partitions(Req,Partitions,Filter),
    Details=[{struct,node_ring_details(P)} || P <- PS],
    {mochijson2:encode(Details),Req,C}.

%% filter a ring based on a given filter name
filter_partitions (Req,PS,node) ->
    Node=list_to_existing_atom(dict:fetch(node,wrq:path_info(Req))),
    [P || P={_,N,_} <- PS, N==Node];
filter_partitions (_Req,PS,all) ->
    PS;
filter_partitions (_Req,_PS,_) ->
    [].

%% return a proplist of details for a given index
node_ring_details (_P={I,OwnerNode,Vnodes}) ->
    [{i,I},
     {node,OwnerNode},
     {vnodes,Vnodes}
    ].

%% %% queries all the nodes in the cluster about their handoffs (SLOW!!)
%% get_handoffs (Ring,Idx) ->
%%     Nodes=riak_core_ring:all_members(Ring),
%%     {Res,_}=rpc:multicall(Nodes,riak_core_handoff_manager,get_handoffs,[Idx]),
%%     Hoffs=lists:append([Hoffs || {ok,Hoffs} <- Res]),
%%     {struct,Hoffs}.
