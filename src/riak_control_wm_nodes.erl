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
%% @doc Returns a list of all nodes, and information about their
%%      membership in the cluster.

-module(riak_control_wm_nodes).

-export([routes/0,
         init/1,
         to_json/2,
         forbidden/2,
         is_authorized/2,
         service_available/2,
         content_types_provided/2]).

-include("riak_control.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%% @doc Return routes this resource should respond to.
-spec routes() -> [webmachine_dispatcher:matchterm()].
routes() ->
    [{riak_control_routes:nodes_route(), ?MODULE, []}].

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

    Nodes = [jsonify_node(Node) || Node=?MEMBER_INFO{} <- RawNodes],
    Encoded = mochijson2:encode({struct, [{nodes, Nodes}]}),

    {Encoded, ReqData, Context}.

%% @doc Turn a node into a proper struct for serialization.
-spec jsonify_node(member()) -> {struct, list()}.
jsonify_node(Node) ->
    LWM=app_helper:get_env(riak_control,low_mem_watermark,0.1),
    MemUsed = Node?MEMBER_INFO.mem_used,
    MemTotal = Node?MEMBER_INFO.mem_total,
    Reachable = Node?MEMBER_INFO.reachable,
    LowMem = low_mem(Reachable, MemUsed, MemTotal, LWM),
    {struct,[{"name",Node?MEMBER_INFO.node},
             {"status",Node?MEMBER_INFO.status},
             {"reachable",Reachable},
             {"ring_pct",Node?MEMBER_INFO.ring_pct},
             {"pending_pct",Node?MEMBER_INFO.pending_pct},
             {"mem_total",MemTotal},
             {"mem_used",MemUsed},
             {"mem_erlang",Node?MEMBER_INFO.mem_erlang},
             {"low_mem",LowMem},
             {"me",Node?MEMBER_INFO.node == node()},
             {"action",Node?MEMBER_INFO.action},
             {"replacement",Node?MEMBER_INFO.replacement}]}.

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
