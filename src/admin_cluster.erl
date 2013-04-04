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
         from_json/2,
         forbidden/2,
         process_post/2,
         is_authorized/2,
         allowed_methods/2,
         delete_resource/2,
         service_available/2,
         content_types_provided/2,
         content_types_accepted/2]).

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
-spec allowed_methods(wrq:reqdata(), undefined) ->
    {list(atom()), wrq:reqdata(), undefined}.
allowed_methods(ReqData, Context) ->
    {['GET', 'POST', 'PUT', 'DELETE'], ReqData, Context}.

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

%% @doc Return content-types which are acceptable.
-spec content_types_accepted(wrq:reqdata(), undefined) ->
    {list(), wrq:reqdata(), undefined}.
content_types_accepted(ReqData, Context) ->
    {[{"application/json", from_json}], ReqData, Context}.

%% @doc Stage a series of changes, and commit the plan immediately.
-spec process_post(wrq:reqdata(), undefined) ->
    {boolean(), wrq:reqdata(), undefined}.
process_post(ReqData, Context) ->
    {Status, Errors} = stage_changes(ReqData, Context),

    case Status of
        true ->
            case riak_control_session:commit_plan() of
                ok ->
                    {true, ReqData, Context};
                _ ->
                    {false, ReqData, Context}
            end;
        false ->
            Response = error_response(ReqData, Errors),
            {{halt, 500}, Response, Context}
    end.

%% @doc Stage a series of changes.
-spec from_json(wrq:reqdata(), undefined) ->
    {boolean(), wrq:reqdata(), undefined}.
from_json(ReqData, Context) ->
    {Status, Errors} = stage_changes(ReqData, Context),

    case Status of
        false ->
            Response = error_response(ReqData, Errors),
            {{halt, 500}, Response, Context};
        true ->
            {true, ReqData, Context}
    end.

%% @doc Encode and return error response.
-spec error_response(wrq:reqdata(), list()) -> wrq:reqdata().
error_response(ReqData, Errors) ->
    EncodedErrors = mochijson2:encode({struct, [{errors, Errors}]}),
    wrq:set_resp_body(EncodedErrors, ReqData).

%% @doc Stage changes; called by both the PUT and POST methods.
-spec stage_changes(wrq:reqdata(), undefined) -> {boolean(), list()}.
stage_changes(ReqData, Context) ->
    case wrq:req_body(ReqData) of
        <<"">> ->
            {true, []};
        _ ->
            Changes = extract_changes(ReqData, Context),
            lists:foldl(fun stage_individual_change/2, {true, []}, Changes)
    end.

%% @doc Stage individual change, used in fold.
-spec stage_individual_change({struct, term()}, boolean()) -> boolean().
stage_individual_change({struct, Change}, Exit) ->
    Node = atomized_get_value(node, Change),
    Action = atomized_get_value(action, Change),
    Replacement = atomized_get_value(replacement, Change, undefined),
    {Status, Errors} = Exit,
    case riak_control_session:stage_change(Node, Action, Replacement) of
        ok ->
            {Status andalso true, Errors};
        {error, Error} ->
            {Status andalso false, Errors ++ [format_error(Error)]}
    end.

%% @doc Extract changes out of a request object.
-spec extract_changes(wrq:reqdata(), undefined) -> list().
extract_changes(ReqData, _Context) ->
    Decoded = mochijson2:decode(wrq:req_body(ReqData)),
    Atomized = atomize(Decoded),
    {struct, [{changes, Changes}]} = Atomized,
    Changes.

%% @doc Remove the staged plan.
-spec delete_resource(wrq:reqdata(), undefined) ->
    {true, wrq:reqdata(), undefined}.
delete_resource(ReqData, Context) ->
    Result = case riak_control_session:clear_plan() of
        {ok, ok} ->
            true;
        {ok, error} ->
            false
    end,
    {Result, ReqData, Context}.

%% @doc Return the current cluster, along with a plan if it's available.
-spec to_json(wrq:reqdata(), undefined) -> {binary(), wrq:reqdata(), undefined}.
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
    lists:foldl(fun(Node, TransitionedNodes) ->
                ChangedNode = apply_changes(Node, Changes, Claim),
                TransitionedNodes ++ [jsonify_node(ChangedNode)]
        end, [], Nodes).

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
            Node#member_info{ring_pct=0};
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

%% @doc Given a struct/proplist that we've received via JSON,
%% recursively turn the keys into atoms from binaries.
atomize({struct, L}) ->
    {struct, [{binary_to_atom(I, utf8), atomize(J)} || {I, J} <- L]};
atomize(L) when is_list(L) ->
    [atomize(I) || I <- L];
atomize(X) ->
    X.

%% @doc Return a value from a proplist, and ensure it's an atom.
atomized_get_value(Key, List) ->
    Result = proplists:get_value(Key, List),
    case is_binary(Result) of
        true ->
            binary_to_atom(Result, utf8);
        false ->
            Result
    end.

%% @doc Return a value from a proplist, and ensure it's an atom.
atomized_get_value(Key, List, Default) when is_atom(Default) ->
    Result = proplists:get_value(Key, List, Default),
    case is_binary(Result) of
        true ->
            binary_to_atom(Result, utf8);
        false ->
            Result
    end.

%% @doc Format error messages.
-spec format_error(stage_error()) -> iolist().
format_error(Error) ->
    ErrorMessage = case Error of
        already_leaving ->
            "Node is already leaving the cluster.";
        not_member ->
            "Node is not a member of this cluster.";
        only_member ->
            "Node is the only member of this cluster.";
        is_claimant ->
            "Node is currently the cluster claimant.";
        invalid_replacement ->
            "Node is an invalid replacement.";
        already_replacement ->
            "Node is already replacing another node.";
        not_reachable ->
            "Node is not reachable.";
        not_single_node ->
            "Node is already in another cluster.";
        self_join ->
            "Node can not be joined to itself."
    end,
    list_to_binary(ErrorMessage).
