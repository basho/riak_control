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
%% @doc Helper utilities for dealing with the ring.

-module(riak_control_ring).

-export([ring/0,
         n_val/0,
         status/2,
         status/3]).

-include_lib("riak_control/include/riak_control.hrl").

%% @doc Return the ring.
-spec ring() -> riak_core:ring().
ring() ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    Ring.

%% @doc Return the current default n_val.
-spec n_val() -> number().
n_val() ->
    {ok, Props} = application:get_env(riak_core, default_bucket_props),
    {n_val, NVal} = lists:keyfind(n_val, 1, Props),
    NVal.

%% @doc Return list of nodes, available partition and quorum.
status(Ring, Unavailable) ->
    NVal = n_val(),
    Quorum = ceiling((NVal / 2) + 1),
    status(Ring, NVal, Quorum, Unavailable).

%% @doc Return list of nodes, available partition and quorum.
-spec status(riak_core:ring(), number(), list(node())) ->
    list({number(), number(), number()}).
status(Ring, NVal, Unavailable) ->
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
                Acc ++ [[{n_val, NVal},
                         {index, list_to_binary(integer_to_list(Index))},
                         {quorum, Quorum},
                         {available, Available}]]

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
