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
-compile([export_all]).

%% helper, ensures all routes begin with /admin
admin_route (Rest) -> ["admin"|Rest].

%% routes that query/act on a single node in the cluster
node_route (Route) -> admin_route(["node",node|Route]).
node_route () -> admin_route(["node",node]).

%% routes that query/act on the cluster
cluster_route (Route) -> admin_route(["cluster"|Route]).
cluster_route () -> admin_route(["cluster"]).

%% routes that query/act on partitions
ring_route (Route) -> admin_route(["ring"|Route]).
ring_route () -> admin_route(["ring"]).

%% routes that query/act on individual v-nodes
vnode_route (Route) -> admin_route(["vnode",partition|Route]).
vnode_route () -> admin_route(["vnode",partition]).

%% new restful routes
nodes_route() -> admin_route(["nodes"]).
partitions_route() -> admin_route(["partitions"]).
