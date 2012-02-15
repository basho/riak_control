%% -------------------------------------------------------------------
%%
%% Copyright (c) 2011 Basho Technologies, Inc.
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

%% @doc Utilities needed by many pieces of the riak_control app.
-module(riak_control).

-export([
         target_node/1,
         is_app_up/1,
         priv_dir/0
        ]).

%% @doc Find out if any nodes are running a given Riak app.
-spec is_app_up(atom()) -> boolean().
is_app_up(App) ->
    riak_core_node_watcher:nodes(App) /= [].

%% @doc Path for the /priv dir of the riak_control app.
-spec priv_dir() -> string().
priv_dir() ->
    code:priv_dir(?MODULE).

%% @doc Return the target node for a particular action.
target_node (Req) ->
    list_to_existing_atom(dict:fetch(node,wrq:path_info(Req))).
