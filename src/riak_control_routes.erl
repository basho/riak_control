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
%% @doc Route helpers.

-module(riak_control_routes).

-export([admin_route/1,
         stats_route/0,
         nodes_route/0,
         cluster_route/0,
         handoffs_route/0,
         partitions_route/0]).

%% @doc Provide a resource that all resources sit under.
-spec admin_route(list()) -> list().
admin_route(Rest) ->
    ["admin"|Rest].

%% @doc Return route for node resource.
-spec stats_route() -> list().
stats_route() ->
    admin_route(["stats"]).

%% @doc Return route for node resource.
-spec nodes_route() -> list().
nodes_route() ->
    admin_route(["nodes"]).

%% @doc Return route for node resource.
-spec handoffs_route() -> list().
handoffs_route() ->
    admin_route(["handoffs"]).

%% @doc Return route for partition resource.
-spec partitions_route() -> list().
partitions_route() ->
    admin_route(["partitions"]).

%% @doc Return route for cluster resource.
-spec cluster_route() -> list().
cluster_route() ->
    admin_route(["cluster"]).
