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
-spec status(riak_core:ring(), list(node())) ->
    list({number(), number(), number()}).
status(Ring, Unavailable) ->
    NVal = n_val(),
    Quorum = mochinum:int_ceil(NVal / 2),
    status(Ring, NVal, Quorum, Unavailable).

%% @doc Return list of nodes, available partition and quorum.
-spec status(riak_core:ring(), number(), list(node())) ->
    list({number(), number(), number()}).
status(Ring, NVal, Unavailable) ->
    Quorum = mochinum:int_ceil(NVal / 2),
    status(Ring, NVal, Quorum, Unavailable).

%% @doc Return list of nodes, available partition and quorum.
-spec status(riak_core:ring(), number(), number(), list(node())) ->
    list({number(), number(), number()}).
status(Ring, NVal, Quorum, Unavailable) ->
    Preflists = riak_core_ring:all_preflists(Ring, NVal),
    {Status, _, _, _} = lists:foldr(fun fold_preflist_proplist/2,
                                    {[], NVal, Quorum, Unavailable},
                                    Preflists),
    lists:usort(fun sort_preflist_proplist/2, Status).

%% @doc Perform a binary conversion of the index to be returned to th
%%      user.
-spec pretty_index(number()) -> binary().
pretty_index(Index) ->
    list_to_binary(integer_to_list(Index)).

%% @doc Sort the preference-as-proplist data structures by index.
-spec sort_preflist_proplist(list(), list()) -> boolean().
sort_preflist_proplist(A, B) ->
    proplists:get_value(index, A) < proplists:get_value(index, B).

%% @doc Fold over the preference lists and generate a data structure
%%      representing partitions in the cluster along with primary
%%      replicas.
-spec fold_preflist_proplist(list(),
    {list(), integer(), integer(), list(node())}) ->
    {list(), integer(), integer(), list(node())}.
fold_preflist_proplist(Preflist,
                       {Status0, NVal, Quorum, Unavailable}) ->
    [{Index, _}|_] = Preflist,

    All = [Node || {_, Node} <- Preflist],

    {Down, Up} = lists:partition(fun(Node) ->
                    lists:member(Node, Unavailable) end, All),

    UUp = lists:usort(Up),
    UAll = lists:usort(All),
    UDown = lists:usort(Down),

    Status = [[{quorum, Quorum},
               {distinct, length(UAll) =:= length(All)},
               {index, pretty_index(Index)},
               {available, length(Up)},
               {unavailable_nodes, UDown},
               {available_nodes, UUp},
               {all_nodes, UAll}]|Status0],

    {Status, NVal, Quorum, Unavailable}.
