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
%%
%% @doc Application supervisor.

-module(riak_control_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    RiakControlSession =
        #{id => riak_control_session,
          start => {riak_control_session, start_link, []}
         },

    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 10},
    case app_helper:get_env(riak_control, enabled, false) of
        true ->
            Resources = [riak_control_wm_gui,
                         riak_control_wm_cluster,
                         riak_control_wm_nodes,
                         riak_control_wm_partitions],
            Routes = lists:append([Resource:routes() || Resource <- Resources]),
            _ = [webmachine_router:add_route(R) || R <- Routes],

            {ok, {SupFlags, [RiakControlSession]}};
        _ ->
            {ok, {SupFlags, [] }}
    end.
