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

%% webmachine dependencies
-include_lib("webmachine/include/webmachine.hrl").

%% mappings to the various content types supported for this resource
-define(CONTENT_TYPES,[{"application/json",to_json}]).

%% defines the webmachine routes this module handles
routes () ->
    [{admin_routes:cluster_route(["list"]),?MODULE,list},
     {admin_routes:cluster_route(["join",node]),?MODULE,join},
     {admin_routes:cluster_route(["down",node]),?MODULE,down}].

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

%% all actions return the same format
cluster_action_result (Error={error,_},Req,C) ->
    {{error,mochijson2:encode({struct,[Error]})},Req,C};
cluster_action_result (Error={badrpc,_},Req,C) ->
    {{error,mochijson2:encode({struct,[Error]})},Req,C};
cluster_action_result (_,Req,C) ->
    {mochijson2:encode({struct,[{result,ok}]}),Req,C}.

%% get a list of all the nodes in the ring and their status
to_json (Req,C=list) ->
    {ok,_V,Nodes}=riak_control_session:get_nodes(),
    Status=[{struct,[{"name",N},
                     {"status",S},
                     {"reachable",Online},
                     {"me",N == node()}
                    ]}
            || {N,S,Online,_} <- Nodes],
    {mochijson2:encode(Status),Req,C};

%% join this node to the cluster of another ring
to_json (Req,C=join) ->
    {ok,Ring}=riak_core_ring_manager:get_my_ring(),
    NodeStr=dict:fetch(node,wrq:path_info(Req)),
    Node=list_to_atom(NodeStr),

    %% if we're a member of a single-node cluster (us) then we're
    %% going to join the other node's ring, otherwise we'll make
    %% the target node join our ring.
    case riak_core_ring:all_members(Ring) of
        [_Me] ->
            %% we're a single-node cluster, join the other guy...
            Result=riak_core:join(Node),
            cluster_action_result(Result,Req,C);
        _ ->
            %% we have a cluster, make them join us
            Result=rpc:call(Node,riak_core,join,[node()]),
            cluster_action_result(Result,Req,C)
    end;

%% mark a node in the cluster as down
to_json (Req,C=down) ->
    NodeStr=dict:fetch(node,wrq:path_info(Req)),
    Node=list_to_existing_atom(NodeStr),
    Result=riak_core:down(Node),
    cluster_action_result(Result,Req,C).

