-module(admin_ring).
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
    [{admin_routes:ring_route(["membership"]),?MODULE,membership}].

%% entry-point for the resource from webmachine
init (Query) -> 
    {ok,Ring}=riak_core_ring_manager:get_my_ring(),
    {ok,{Query,Ring}}.

%% redirect to SSL port and authenticate
is_authorized (Req,Ctx) ->
    {true,Req,Ctx}.

%% return the list of available content types for webmachine
content_types_provided (Req,Ctx) ->
    {?CONTENT_TYPES,Req,Ctx}.

%% valid | invalid | joining | leaving | exiting
to_json (Req,Ctx={membership,Ring}) ->
    Members=riak_core_ring:all_members(Ring),
    {mochijson2:encode(tl(Members) =/= []),Req,Ctx}.
