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

-type version()       :: integer().
-type index()         :: integer().
-type status()        :: valid | invalid | down | leaving | incompatible | transitioning.
-type home()          :: primary | fallback | undefined.
-type service()       :: {atom(), home()}.
-type services()      :: [service()].
-type owner()         :: atom().
-type vnode()         :: {{atom(),atom()},atom()}.
-type handoff()       :: {atom(),integer(),atom()}.
-type online()        :: boolean().
-type ring()          :: riak_core_ring:riak_core_ring().
-type handoffs()      :: [handoff()].
-type vnodes()        :: [vnode()].
-type plan()          :: [] | legacy | ring_not_ready | unavailable.
-type transfer()      :: riak_core_ring:pending_change().
-type transfers()     :: [transfer()].
-type single_n_val()  :: pos_integer().
-type n_vals()        :: [pos_integer()].

-type stage_error() :: nodedown
                     | already_leaving
                     | not_member
                     | only_member
                     | is_claimant
                     | invalid_replacement
                     | already_replacement
                     | not_reachable
                     | not_single_node
                     | self_join.

-type action() :: leave
                | remove
                | {replace, node()}
                | {force_replace, node()}.

-type claim_percentage() :: number().

-type change() :: {node(), action()}.

-record(partition_info,
        { index       :: index(),
          partition   :: integer(),
          owner       :: owner(),
          vnodes      :: services(),
          handoffs    :: handoffs() }).

-define(PARTITION_INFO,  #partition_info).
-type partition()     :: ?PARTITION_INFO{}.
-type partitions()    :: [partition()].

%% Riak 1.3
-record(member_info,
        { node        :: atom(),
          status      :: status(),
          reachable   :: boolean(),
          vnodes      :: vnodes(),
          handoffs    :: handoffs(),
          ring_pct    :: float(),
          pending_pct :: float(),
          mem_total   :: integer(),
          mem_used    :: integer(),
          mem_erlang  :: integer() }).

%% Riak 1.4.1+
-record(member_info_v2,
        { node        :: atom(),
          status      :: status(),
          reachable   :: boolean(),
          vnodes      :: vnodes(),
          handoffs    :: handoffs(),
          ring_pct    :: float(),
          pending_pct :: float(),
          mem_total   :: integer(),
          mem_used    :: integer(),
          mem_erlang  :: integer(),
          action      :: action(),
          replacement :: node() }).

-define(MEMBER_INFO,     #member_info_v2).
-type member()        :: ?MEMBER_INFO{}.
-type members()       :: [member()].

%% These two should always match, in terms of webmachine dispatcher
%% logic, and ADMIN_BASE_PATH should always end with a /
-define(ADMIN_BASE_PATH, "/admin/").
-define(ADMIN_BASE_ROUTE, ["admin"]).

%% Value for WWW-Authenticate header
-define(ADMIN_AUTH_HEAD, "Basic realm=riak").

%% Names of HTTP header fields
-define(HEAD_CTYPE, "Content-Type").
