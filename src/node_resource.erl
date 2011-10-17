-module(node_resource).
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

init ([]) ->
    {ok,undefined}.

content_types_provided (RD,C) ->
    {?CONTENT_TYPES,RD,C}.

to_json (RD,C) ->
    Node=dict:fetch(node,wrq:path_info(RD)),
    Rpc=rpc:call(list_to_atom(Node),riak_kv_stat,get_stats,[]),
    Stats=lists:keydelete(disk,1,Rpc),
    Callback=wrq:get_qs_value("callback",RD),
    Result=wrap_result(Callback,mochijson2:encode({struct,Stats})),
    {Result,RD,C}.

wrap_result (undefined,Json) ->
    Json;
wrap_result (Callback,Json) ->
    io_lib:format("~s(~s);",[Callback,Json]).

to_text (RD,C) ->
    {"TODO:",RD,C}.

is_authorized (RD,C) ->
    {true,RD,C}.
