%% -------------------------------------------------------------------
%%
%% Copyright (c) 2011 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

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
