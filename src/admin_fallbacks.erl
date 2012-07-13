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

-module(admin_fallbacks).
-export([routes/0,
         init/1,
         content_types_provided/2,
         to_resource/2,
         is_authorized/2,
         service_available/2,
         forbidden/2
        ]).

%% riak_control and webmachine dependencies
-include_lib("riak_control/include/riak_control.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%% mappings to the various content types supported for this resource
-define(CONTENT_TYPES,[{"application/json", to_resource}]).

%% defines the webmachine routes this module handles
routes() -> [{admin_routes:admin_route(["fallbacks"]),?MODULE,[]}].

%% entry-point for the resource from webmachine
init([]) -> {ok,undefined}.

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

%% true if the node is running riak_control
redirect_loc(Node) ->
    rpc:call(Node,riak_control_security,https_redirect_loc,[[]]).

%% don't use this node as a fallback, must be valid and reachable
find_fallbacks(Nodes) ->
    lists:foldl(fun (#member_info{node=Node,status=valid,reachable=true},Acc) ->
                        case Node == node() of
                            true -> Acc;
                            false ->
                                case redirect_loc(Node) of
                                    {ok,Loc} ->
                                        URI=lists:flatten(Loc),
                                        [list_to_binary(URI)|Acc];
                                    _ -> Acc
                                end
                        end;
                    (_,Acc) -> Acc
                end,
                [],
                Nodes).

%% find another node in the cluster that is running the GUI
to_resource(Req,Ctx) ->
    {ok,_V,Nodes}=riak_control_session:get_nodes(),
    NodeURIs=find_fallbacks(Nodes),
    {mochijson2:encode(NodeURIs),Req,Ctx}.
