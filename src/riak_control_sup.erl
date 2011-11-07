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

-module(riak_control_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    case app_helper:get_env(riak_control,enabled,false) of
        true ->
            Resources = [{admin, admin_gui},
                         {admin, admin_ring},
                         {admin, admin_cluster},
                         {admin, admin_node}
                        ],
            Routes = lists:append([routes(E, M) || {E, M} <- Resources]),
            [webmachine_router:add_route(R) || R <- Routes];
        _ ->
            ok
    end,

    %% modules to be started up
    Riak_control_session={riak_control_session,
                          {riak_control_session, start_link, []},
                          permanent, 
                          5000, 
                          worker, 
                          [riak_control_session]},

    {ok, { {one_for_one, 5, 10}, [Riak_control_session] } }.

routes(Env, Module) ->
    case app_helper:get_env(riak_control, Env, false) of
        true ->
            Module:routes();
        false ->
            [];
        _Other ->
            error_logger:warning_msg(
              "Defaulting riak_control appenv '~p' to 'false'."
              " Found unknown \"~p\"", [Env, _Other])
    end.
