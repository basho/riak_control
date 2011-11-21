-module(ring_resource).
-export([init/1,
         content_types_provided/2,
         to_json/2,
         to_text/2,
         is_authorized/2
        ]).

%% webmachine dependencies
-include_lib("webmachine/include/webmachine.hrl").

%% mappings to the various content types supported for this resource
-define(CONTENT_TYPES,[{"application/json",to_json},
                       {"text/plain",to_text}
                      ]).

%% all the statuses we care about
-define(NODE_STATUSES,[valid,invalid,leaving,exiting]).

init ([]) ->
    {ok,Ring}=riak_core_ring_manager:get_my_ring(),
    {ok,Ring}.

content_types_provided (RD,C) ->
    {?CONTENT_TYPES,RD,C}.

to_json (RD,C) ->
    Nodes=riak_core_ring:all_member_status(C),
    Callback=wrq:get_qs_value("callback",RD),
    Owner={owner,riak_core_ring:owner_node(C)},
    Statuses=[Owner|[{S,node_status_list(Nodes,S)} || S <- ?NODE_STATUSES]],
    Result=wrap_result(Callback,mochijson2:encode({struct,Statuses})),
    {Result,RD,C}.

node_status_list (Nodes,Status) ->
    [Node || {Node,S} <- Nodes, S==Status].

wrap_result (undefined,Json) ->
    Json;
wrap_result (Callback,Json) ->
    io_lib:format("~s(~s);",[Callback,Json]).

to_text (RD,C) ->
    Nodes=[io_lib:format("~s~n",[Node]) || Node <- C],
    {lists:flatten(Nodes),RD,C}.

is_authorized (RD,C) ->
    {true,RD,C}.
