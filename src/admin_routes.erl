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
         nodes_route/0,
         partitions_route/0,
         cluster_route/0]).

%% helper, ensures all routes begin with /admin
admin_route(Rest) -> ["admin"|Rest].

%% routes that query/act on partitions
ring_route(Route) -> admin_route(["ring"|Route]).
ring_route() -> admin_route(["ring"]).

%% new routes.
nodes_route() -> admin_route(["nodes"]).
partitions_route() -> admin_route(["partitions"]).
cluster_route() -> admin_route(["cluster"]).
