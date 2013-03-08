%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
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

-author('Christopher Meiklejohn <cmeiklejohn@basho.com>').

-export([routes/0,
         init/1,
         to_json/2,
         forbidden/2,
         is_authorized/2,
         delete_resource/2,
         service_available/2,
         content_types_provided/2]).

-include_lib("riak_control/include/riak_control.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%% @doc Return routes this resource should respond to.
-spec routes() -> list().
routes() ->
    [{admin_routes:cluster_route(), ?MODULE, []}].

%% @doc Initialize resource.
-spec init([]) -> {ok, undefined}.
init([]) ->
    {ok, undefined}.

%% @doc Prevent requests coming from an invalid origin.
-spec forbidden(#wm_reqdata{}, undefined) ->
    {boolean(), #wm_reqdata{}, undefined}.
forbidden(ReqData, Context) ->
    {riak_control_security:is_null_origin(ReqData), ReqData, Context}.

%% @doc Handle SSL requests.
-spec service_available(#wm_reqdata{}, undefined) ->
    {boolean(), #wm_reqdata{}, undefined}.
service_available(ReqData, Context) ->
    riak_control_security:scheme_is_available(ReqData, Context).

%% @doc Ensure user has access.
-spec is_authorized(#wm_reqdata{}, undefined) ->
    {boolean(), #wm_reqdata{}, undefined}.
is_authorized(ReqData, Context) ->
    riak_control_security:enforce_auth(ReqData, Context).

%% @doc Return the available contents.
-spec content_types_provided(#wm_reqdata{}, undefined) ->
    {list(), #wm_reqdata{}, undefined}.
content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

%% @doc Remove the staged plan.
-spec delete_resource(#wm_reqdata{}, undefined) ->
    {true, #wm_reqdata{}, undefined}.
delete_resource(ReqData, Context) ->
    Result = try riak_core_claimant:clear() of
        ok ->
            true
    catch
        _:_ ->
            false
    end,
    {Result, ReqData, Context}.

%% @doc Return the current cluster, along with a plan if it's available.
-spec to_json(#wm_reqdata{}, undefined) ->
    {binary(), #wm_reqdata{}, undefined}.
to_json(ReqData, Context) ->
    {ok, _V, RawNodes} = riak_control_session:get_nodes(),

    Plan = try riak_core_claimant:plan() of
        {error, Error} ->
            Error;
        {ok, Changes, NextRings} ->
            _FinalRing = lists:last(NextRings),
            [jsonify_change(Change) || Change <- Changes]
    catch
        _:_ ->
            unknown
    end,

    Cluster = [jsonify_node(Node) || Node=#member_info{} <- RawNodes],
    ClusterInformation = [{cluster, Cluster}, {plan, Plan}],
    {mochijson2:encode({struct, ClusterInformation}), ReqData, Context}.

%% @doc Turn a list of changes into a proper structure for serialization.
-spec jsonify_change(list()) -> list().
jsonify_change(Change) ->
    FormattedChange = case Change of
        {Node, Operation, Argument} ->
            [{"name", Node}, {"operation", Operation}, {"argument", Argument}];
        {Node, Operation} ->
            [{"name", Node}, {"operation", Operation}]
    end,
    {struct, FormattedChange}.

%% @doc Turn a node into a proper struct for serialization.
-spec jsonify_node(#member_info{}) -> {struct, list()}.
jsonify_node(Node) ->
    MemUsed = Node#member_info.mem_used,
    MemTotal = Node#member_info.mem_total,
    Reachable = Node#member_info.reachable,
    {struct,[{"name",Node#member_info.node},
             {"status",Node#member_info.status},
             {"reachable",Reachable},
             {"ring_pct",Node#member_info.ring_pct},
             {"mem_total",MemTotal},
             {"mem_used",MemUsed},
             {"me",Node#member_info.node == node()}]}.
