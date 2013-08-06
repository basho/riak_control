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

-module(admin_nodes).
-export([routes/0,
         init/1,
         content_types_provided/2,
         to_json/2,
         is_authorized/2,
         service_available/2,
         forbidden/2
        ]).

%% riak_control and webmachine dependencies
-include_lib("riak_control/include/riak_control.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%% mappings to the various content types supported for this resource
-define(CONTENT_TYPES,[{"application/json",to_json}]).

%% defines the webmachine routes this module handles
routes() ->
    [{admin_routes:nodes_route(),?MODULE,[]}].

%% entry-point for the resource from webmachine
init(Action) ->
    {ok,Action}.

%% validate origin
forbidden(RD, C) ->
    {riak_control_security:is_null_origin(RD), RD, C}.

%% redirect to SSL port if using HTTP
service_available(RD,C) ->
    riak_control_security:scheme_is_available(RD,C).

%% validate username and password
is_authorized(RD,C) ->
    riak_control_security:enforce_auth(RD,C).

%% return the list of available content types for webmachine
content_types_provided(Req,C) ->
    {?CONTENT_TYPES,Req,C}.

%% get a list of all the nodes in the ring and their status
to_json(Req,C) ->
    {ok,_V,RawNodes}=riak_control_session:get_nodes(),
    Nodes=[jsonify_node(Node) || Node=#member_info{} <- RawNodes],
    {mochijson2:encode({struct, [{nodes, Nodes}]}),Req,C}.

jsonify_node(Node) ->
    LWM=app_helper:get_env(riak_control,low_mem_watermark,0.1),
    MemUsed = Node#member_info.mem_used,
    MemTotal = Node#member_info.mem_total,
    Reachable = Node#member_info.reachable,
    LowMem = low_mem(Reachable, MemUsed, MemTotal, LWM),
    {struct,[{"name",Node#member_info.node},
             {"status",Node#member_info.status},
             {"reachable",Reachable},
             {"ring_pct",Node#member_info.ring_pct},
             {"pending_pct",Node#member_info.pending_pct},
             {"mem_total",MemTotal},
             {"mem_used",MemUsed},
             {"mem_erlang",Node#member_info.mem_erlang},
             {"low_mem",LowMem},
             {"me",Node#member_info.node == node()}]}.

%% @doc Determine if a node has low memory.
-spec low_mem(boolean(), number() | atom(), number() | atom(), number())
    -> boolean().
low_mem(Reachable, MemUsed, MemTotal, LWM) ->
    case Reachable of
        false ->
            false;
        true ->
            %% There is a race where the node is online, but memsup is
            %% still starting so memory is unavailable.
            case MemTotal of
                undefined ->
                    false;
                _ ->
                    1.0 - (MemUsed/MemTotal) < LWM
            end
    end.
