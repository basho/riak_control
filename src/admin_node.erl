-module(admin_node).
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
    [{admin_routes:node_route(["stop"]),?MODULE,stop},
     {admin_routes:node_route(["leave"]),?MODULE,leave},
     {admin_routes:node_route(["add"]),?MODULE,add}].

%% entry-point for the resource from webmachine
init (Action) -> {ok,Action}.

%% redirect to SSL port and authenticate
is_authorized (Req,C) ->
    {true,Req,C}.

%% return the list of available content types for webmachine
content_types_provided (Req,C) ->
    {?CONTENT_TYPES,Req,C}.

%% get the target node for the action
target_node (Req) ->
    list_to_atom(dict:fetch(node,wrq:path_info(Req))).

%% most node actions are simple rpc calls
to_json (Req,C=stop) ->
    perform_node_action(Req,C,riak_core,stop,[]);
to_json (Req,C=leave) ->
    perform_node_action(Req,C,riak_core,leave,[]);
to_json (Req,C=add) ->
    perform_node_action(Req,C,riak_core,join,[node()]);
to_json (Req,C) ->
    node_action_result({error,{struct,[{unknown_action,C}]}},Req,C).

%% remote to the target node, perform the action, and return
perform_node_action (Req,C,Module,Fun,Args) ->
    Node=target_node(Req),
    Result=case rpc:call(Node,Module,Fun,Args) of
               {badrpc,Error} -> {error,Error};
               Ok -> Ok
           end,
    node_action_result(Result,Req,C).

%% all actions return the same format
node_action_result (Error={error,_Reason},Req,C) ->
    {mochijson2:encode({struct,[{result,error},Error]}),Req,C};
node_action_result (_,Req,C) ->
    {mochijson2:encode({struct,[{result,ok}]}),Req,C}.
