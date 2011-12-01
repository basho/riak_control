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

-module(admin_overview).
-export([routes/0,
         init/1,
         content_types_provided/2,
         to_json/2,
         is_authorized/2,
         service_available/2
        ]).

%% riak_control and webmachine dependencies
-include_lib("riak_control/include/riak_control.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%% mappings to the various content types supported for this resource
-define(CONTENT_TYPES,[{"application/json",to_json}]).

%% defines the webmachine routes this module handles
routes () -> [{admin_routes:admin_route(["overview"]),?MODULE,[]}].

%% entry-point for the resource from webmachine
init ([]) ->
    {ok,undefined}.

%% redirect to SSL port if using HTTP
service_available (RD,C) ->
    riak_control_security:scheme_is_available(RD,C).

%% validate username and password
is_authorized (RD,C) ->
    riak_control_security:enforce_auth(RD,C).

%% return the list of available content types for webmachine
content_types_provided (Req,C) ->
    {?CONTENT_TYPES,Req,C}.

%% get a list of all the nodes in the ring and their status
to_json (Req,C) ->
    {ok,_V,Nodes}=riak_control_session:get_nodes(),
    Json=[{unreachable_nodes, get_unreachable_nodes(Nodes)},
          {down_nodes, get_down_nodes(Nodes)}
         ],
    {mochijson2:encode({struct,Json}),Req,C}.

%% get a list of all the nodes that are current partitioned
get_unreachable_nodes (Nodes) ->
    [Node || #member_info{node=Node,reachable=false} <- Nodes].

%% get a list of all nodes currently marked down
get_down_nodes (Nodes) ->
    [Node || #member_info{node=Node,status=down} <- Nodes].
