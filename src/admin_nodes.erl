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
%%
%% @doc
%%
%% Responsible for viewing and modifying node properties.
%%
%% GET  /nodes returns the current and staged clusters.
%%
%% @end

-module(admin_nodes).

-export([routes/0,
         init/1,
         to_json/2,
         forbidden/2,
         is_authorized/2,
         service_available/2,
         content_types_provided/2]).

-include_lib("riak_control/include/riak_control.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%% @doc Return routes this resource should respond to.
-spec routes() -> [webmachine_dispatcher:matchterm()].
routes() ->
    [{admin_routes:nodes_route(), ?MODULE, []}].

%% @doc Initialize resource.
-spec init([]) -> {ok, undefined}.
init([]) ->
    {ok, undefined}.

%% @doc Prevent requests coming from an invalid origin.
-spec forbidden(wrq:reqdata(), undefined) ->
    {boolean(), wrq:reqdata(), undefined}.
forbidden(ReqData, Context) ->
    {riak_control_security:is_protected(ReqData, Context), ReqData, Context}.

%% @doc Handle SSL requests.
-spec service_available(wrq:reqdata(), undefined) ->
    {boolean(), wrq:reqdata(), undefined}.
service_available(ReqData, Context) ->
    riak_control_security:scheme_is_available(ReqData, Context).

%% @doc Ensure user has access.
-spec is_authorized(wrq:reqdata(), undefined) ->
    {boolean(), wrq:reqdata(), undefined}.
is_authorized(ReqData, Context) ->
    riak_control_security:enforce_auth(ReqData, Context).

%% @doc Return content-types which are provided.
-spec content_types_provided(wrq:reqdata(), undefined) ->
    {list(), wrq:reqdata(), undefined}.
content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

%% @doc Return the current cluster, along with a plan if it's available.
-spec to_json(wrq:reqdata(), undefined) -> {binary(), wrq:reqdata(), undefined}.
to_json(ReqData, Context) ->
    %% Get the current node list.
    {ok, _V, RawNodes} = riak_control_session:get_nodes(),

    Nodes = [jsonify_node(Node) || Node=#member_info{} <- RawNodes],
    Encoded = mochijson2:encode({struct, [{nodes, Nodes}]}),

    {Encoded, ReqData, Context}.

%% @doc Turn a node into a proper struct for serialization.
-spec jsonify_node(#member_info{}) -> {struct, list()}.
jsonify_node(Node) ->
    LWM=app_helper:get_env(riak_control,low_mem_watermark,0.1),
    MemUsed = Node#member_info.mem_used,
    MemTotal = Node#member_info.mem_total,
    Reachable = Node#member_info.reachable,
    LowMem = case Reachable of
        false ->
            false;
        true ->
            1.0 - (MemUsed/MemTotal) < LWM
    end,
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
