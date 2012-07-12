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

-module(admin_cluster_join).
-export([routes/0,
         init/1,
         allowed_methods/2,
         content_types_provided/2,
         process_post/2,
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
    [{admin_routes:cluster_route(["join",node]),?MODULE,[]}].

%% entry-point for the resource from webmachine
init([]) -> {ok,undefined}.

%% alow post
allowed_methods(RD, C) ->
    {['POST'], RD, C}.

%% redirect to SSL port if using HTTP
service_available(RD,C) ->
    riak_control_security:scheme_is_available(RD,C).

%% validate username and password
is_authorized(RD,C) ->
    riak_control_security:enforce_auth(RD,C).

%% validate csfr_token
forbidden(RD, C) ->
    {not riak_control_security:is_valid_csrf_token(RD, C), RD, C}.

%% return the list of available content types for webmachine
content_types_provided(Req,C) ->
    {?CONTENT_TYPES,Req,C}.

%% join this node to the cluster of another ring
process_post(Req,C) ->
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
            riak_control_formatting:cluster_action_result(Result,Req,C);
        _ ->
            %% we have a cluster, make them join us
            Result=rpc:call(Node,riak_core,join,[node()]),
            riak_control_formatting:cluster_action_result(Result,Req,C)
    end.
