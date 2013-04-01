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
         allowed_methods/2,
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

%% @doc Allowed methods.
-spec allowed_methods(#wm_reqdata{}, undefined) ->
    {list(atom()), #wm_reqdata{}, undefined}.
allowed_methods(ReqData, Context) ->
    {['GET', 'DELETE'], ReqData, Context}.

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
    Result = case riak_control_session:clear_plan() of
        {ok, ok} ->
            true;
        {ok, error} ->
            false
    end,
    {Result, ReqData, Context}.

%% @doc Return the current cluster, along with a plan if it's available.
-spec to_json(#wm_reqdata{}, undefined) ->
    {binary(), #wm_reqdata{}, undefined}.
to_json(ReqData, Context) ->

    %% Get the current node list.
    {ok, _V, Nodes} = riak_control_session:get_nodes(),
    Current = [jsonify_node(Node) || Node=#member_info{} <- Nodes],

    %% Get the current list of planned changes and updated claim.
    Planned = case riak_control_session:get_plan() of
        {error, Error} ->
            Error;
        {ok, [], _Claim} ->
            [];
        {ok, Changes, Claim} ->
            merge_transitions(Nodes, Changes, Claim)
    end,

    %% Generate a list of two clusters, current, and future with
    %% annotated upates.
    Clusters = [{current, Current}, {staged, Planned}],

    {mochijson2:encode({struct,[{cluster,Clusters}]}), ReqData, Context}.

%% @doc Generate a new "planned" cluster which outlines transitions.
-spec merge_transitions(list(#member_info{}), list(), list()) ->
    [{struct, list()}].
merge_transitions(Nodes, Changes, Claim) ->
    [jsonify_node(apply_changes(Node, Changes, Claim)) || Node <- Nodes].

%% @doc Merge change into member info record.
-spec apply_changes(#member_info{}, list(), list()) -> #member_info{}.
apply_changes(Node, Changes, Claim) ->
    apply_status_change(apply_claim_change(Node, Claim), Changes).

%% @doc Merge change into member info record.
-spec apply_status_change(#member_info{}, list()) -> #member_info{}.
apply_status_change(Node, Changes) ->
    Name = Node#member_info.node,

    case lists:keyfind(Name, 1, Changes) of
        false ->
            Node;
        {_, {Action, Replacement}} ->
            Node#member_info{action=Action, replacement=Replacement};
        {_, Action} ->
            Node#member_info{action=Action}
    end.

%% @doc Merge change into member info record.
-spec apply_claim_change(#member_info{}, list()) -> #member_info{}.
apply_claim_change(Node, Claim) ->
    Name = Node#member_info.node,

    case lists:keyfind(Name, 1, Claim) of
        false ->
            Node;
        {_, {_, Future}} ->
            %% @doc Hack until core returns normalized values.
            Normalized = if
                Future > 0 ->
                    Future / 100;
                true ->
                    Future
            end,
            Node#member_info{ring_pct=Normalized}
    end.

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
             {"mem_total",MemTotal},
             {"mem_used",MemUsed},
             {"mem_erlang",Node#member_info.mem_erlang},
             {"low_mem",LowMem},
             {"me",Node#member_info.node == node()},
             {"action",Node#member_info.action},
             {"replacement",Node#member_info.replacement}]}.
