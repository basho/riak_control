%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.
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
%% @todo Write specs.

-module(riak_control_ring).

-export([status/1]).

-include_lib("riak_control/include/riak_control.hrl").

%% @doc Return the ring.
ring() ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    Ring.

%% @doc Return the current default n_val.
n_val() ->
    {ok, Props} = application:get_env(riak_core, default_bucket_props),
    {n_val, NVal} = lists:keyfind(n_val, 1, Props),
    NVal.

%% @doc Return list of nodes, available partition and quorum.
-spec status(list(node())) ->
    list({number(), number(), number()}).
status(Unavailable) ->
    Ring = ring(),
    NVal = n_val(),
    Quorum = ceiling((NVal / 2) + 1),
    status(Ring, NVal, Quorum, Unavailable).

%% @doc Return list of nodes, available partition and quorum.
-spec status(riak_core:ring(), number(), number(), list(node())) ->
    list({number(), number(), number()}).
status(Ring, NVal, Quorum, Unavailable) ->
    Preflists = riak_core_ring:all_preflists(Ring, NVal),
    lists:foldl(fun(Preflist, Acc) ->

                %% Get the first partition in the preflist.
                %%
                [{Index, _}|_] = Preflist,

                %% Determine count of nodes in the preflist that are
                %% down.
                %%
                Available = lists:foldl(fun({_Index, Node}, Acc1) ->
                            case unavailable_node(Node, Unavailable) of
                                false ->
                                    Acc1 + 1;
                                true ->
                                    Acc1
                            end
                    end, 0, Preflist),

                %% Return each index, available primaries, and what the
                %% quorum is.
                Acc ++ [{Index, Available, Quorum}]

        end, [], Preflists).

%% @doc Return true if a node is unavailable.
-spec unavailable_node(node(), list(node())) -> boolean().
unavailable_node(Node, Unavailable) ->
    lists:member(Node, Unavailable).

%% @doc Produce ceiling.
-spec ceiling(number()) -> number().
ceiling(X) when X < 0 ->
    trunc(X);
ceiling(X) ->
    T = trunc(X),
    case (X - T == 0) of
        true ->
            T;
        false ->
            T + 1
    end.
