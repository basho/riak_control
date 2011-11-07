-module(admin_node).
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

%% defines the webmachine routes this module handles
routes () ->
    [{admin_routes:node_route(["ping"]),?MODULE,ping},
     {admin_routes:node_route(["stats"]),?MODULE,stats},
     {admin_routes:node_route(["details"]),?MODULE,details},
     {admin_routes:node_route(["stop"]),?MODULE,stop},
     {admin_routes:node_route(["leave"]),?MODULE,leave},
     {admin_routes:node_route(["add"]),?MODULE,add}].

%% entry-point for the resource from webmachine
init (Action) -> {ok,Action}.

%% redirect to SSL port if using HTTP
service_available (RD,C) ->
    riak_control_security:scheme_is_available(RD,C).

%% validate username and password
is_authorized (RD,C) ->
    riak_control_security:enforce_auth(RD,C).

%% return the list of available content types for webmachine
content_types_provided (Req,C) ->
    {?CONTENT_TYPES,Req,C}.

%% get the target node for the action
target_node (Req) ->
    list_to_atom(dict:fetch(node,wrq:path_info(Req))).

%% most node actions are simple rpc calls
to_json (Req,C=ping) ->
    perform_rpc_action(Req,C,net_adm,ping,[node()]);
to_json (Req,C=stats) ->
    get_node_stats(Req,C);
to_json (Req,C=details) ->
    get_node_details(Req,C);
to_json (Req,C=stop) ->
    perform_rpc_action(Req,C,riak_core,stop,[]);
to_json (Req,C=leave) ->
    perform_rpc_action(Req,C,riak_core,leave,[]);
to_json (Req,C=add) ->
    perform_rpc_action(Req,C,riak_core,join,[node()]);
to_json (Req,C) ->
    node_action_result({error,{struct,[{unknown_action,C}]}},Req,C).

%% stats aren't perfectly formatted json (TODO: fix that!)
get_node_stats (Req,C) ->
    Node=target_node(Req),
    Result=rpc:call(Node,riak_kv_stat,get_stats,[]),
    Stats=proplists:delete(disk,Result),
    {mochijson2:encode({struct,Stats}),Req,C}.

%% details are ring information for this particular node
get_node_details (Req,C) ->
    Node=target_node(Req),
    {ok,Ring}=riak_core_ring_manager:get_my_ring(),
    Indices=rpc:call(Node,riak_core_ring,my_indices,[Ring]),
    PS=[{struct,admin_ring:node_ring_details(Ring,{P,Node})} || P <- Indices],
    {mochijson2:encode(PS),Req,C}.

%% remote to the target node, perform the action, and return
perform_rpc_action (Req,C,Module,Fun,Args) ->
    Node=target_node(Req),
    Result=case rpc:call(Node,Module,Fun,Args) of
               {badrpc,Error} -> {error,Error};
               Ok -> Ok
           end,
    node_action_result(Result,Req,C).

%% all actions return the same format
node_action_result ({error,Reason},Req,C) ->
    {{error,Reason},Req,C};
node_action_result (_,Req,C) ->
    {mochijson2:encode({struct,[{result,ok}]}),Req,C}.
