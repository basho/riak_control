-module(admin).
-export([routes/0,
         init/1,
         content_types_provided/2,
         to_html/2,
         is_authorized/2
        ]).

%% webmachine dependencies
-include_lib("webmachine/include/webmachine.hrl").

%% mappings to the various content types supported for this resource
-define(CONTENT_TYPES,[{"text/html",to_html}]).

%% defines the webmachine routes this module handles
routes () ->
    [{["admin"],?MODULE,[]}].

%% entry-point for the resource from webmachine
init ([]) -> 
    {ok,undefined}.

%% redirect to SSL port and authenticate
is_authorized (Req,Ctx) ->
    {true,Req,Ctx}.

%% return the list of available content types for webmachine
content_types_provided (Req,Ctx) ->
    {?CONTENT_TYPES,Req,Ctx}.

%% valid | invalid | joining | leaving | exiting
to_html (Req,Ctx) ->
    Index=filename:join([riak_control:priv_dir(),"admin","index.html"]),
    Source=file:read_file(Index),
    {Source,Req,Ctx}.
