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

-module(admin_cluster).
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
routes () ->
    [{admin_routes:cluster_route(["list"]),?MODULE,list}].

%% entry-point for the resource from webmachine
init (Action) ->
    {ok,Action}.

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
to_json (Req,C=list) ->
    {ok,_V,Nodes}=riak_control_session:get_nodes(),
    Status=[{struct,[{"name",Node#member_info.node},
                     {"status",Node#member_info.status},
                     {"reachable",Node#member_info.reachable},
                     {"ring_pct",Node#member_info.ring_pct},
                     {"pending_pct",Node#member_info.pending_pct},
                     {"mem_total",Node#member_info.mem_total},
                     {"mem_used",Node#member_info.mem_used},
                     {"mem_erlang",Node#member_info.mem_erlang},
                     {"me",Node#member_info.node == node()}
                    ]}
            || Node=#member_info{} <- Nodes],
    {mochijson2:encode(Status),Req,C};

%% mark a node in the cluster as down
to_json (Req,C=down) ->
    NodeStr=dict:fetch(node,wrq:path_info(Req)),
    Node=list_to_existing_atom(NodeStr),
    Result=riak_core:down(Node),
    riak_control_formatting:cluster_action_result(Result,Req,C).

