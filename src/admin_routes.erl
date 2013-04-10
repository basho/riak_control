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

-module(admin_routes).

-export([admin_route/1,
         ring_route/0,
         ring_route/1,
         vnode_route/0,
         vnode_route/1,
         nodes_route/0,
         partitions_route/0,
         cluster_route/0]).

%% helper, ensures all routes begin with /admin
-spec admin_route(list(string())) -> list(string()).
admin_route(Rest) -> ["admin"|Rest].

%% routes that query/act on partitions
-spec ring_route([string()])
                -> [string() ,...].
ring_route(Route) -> admin_route(["ring"|Route]).

-spec ring_route() -> [string() ,...].
ring_route() -> admin_route(["ring"]).

%% routes that query/act on individual v-nodes
-spec vnode_route(list(string())) -> list(string()).
vnode_route(Route) -> admin_route(["vnode",partition|Route]).

-spec vnode_route() -> list(string()).
vnode_route() -> admin_route(["vnode",partition]).

%% new routes.
-spec nodes_route() -> list(string()).
nodes_route() -> admin_route(["nodes"]).
-spec partitions_route() -> list(string()).
partitions_route() -> admin_route(["partitions"]).
-spec cluster_route() -> list(string()).
cluster_route() -> admin_route(["cluster"]).
