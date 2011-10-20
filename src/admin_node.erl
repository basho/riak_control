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
    [{admin_routes:node_route([name,action]),?MODULE,[]}].

%% entry-point for the resource from webmachine
init ([]) -> {ok,undefined}.

%% redirect to SSL port and authenticate
is_authorized (Req,C) ->
    {true,Req,C}.

%% return the list of available content types for webmachine
content_types_provided (Req,C) ->
    {?CONTENT_TYPES,Req,C}.

%% perform the action requested
to_json (Req,_Context) ->
    NodeStr=dict:fetch(name,wrq:path_info(Req)),
    ActionStr=dict:fetch(action,wrq:path_info(Req)),
    NodeAction={list_to_atom(NodeStr),list_to_atom(ActionStr)},
    perform_node_action(Req,NodeAction).

%% take a node down in the ring if it's offline
perform_node_action (Req,C={Node,down}) ->
    Result=riak_core:down(Node),
    node_action_result(Result,Req,C);

%% abruptly take a node offline -- THIS IS NOT GRACEFUL! USE LEAVE!
perform_node_action (Req,C={Node,kill}) ->
    case rpc:call(Node,erlang,halt,[]) of
        {badrpc,Reason} -> node_action_result({error,Reason},Req,C);
        _ -> node_action_result(ok,Req,C)
    end;

%% attempt to join another node
perform_node_action (Req,C={Node,join}) ->
    Result=riak_core:join(Node),
    node_action_result(Result,Req,C);

%% attempt to make a node in the cluster leave
perform_node_action (Req,C={Node,leave}) ->
    Result=rpc:call(Node,riak_core,leave,[]),
    node_action_result(Result,Req,C);

%% unknown action
perform_node_action (Req,C={_Node,_Action}) ->
    node_action_result({error,unknown_action},Req,C).

%% all actions return the same format
node_action_result (Error={error,_Reason},Req,C) ->
    {mochijson2:encode({struct,[{result,error},Error]}),Req,C};
node_action_result (_,Req,C) ->
    {mochijson2:encode({struct,[{result,ok}]}),Req,C}.

