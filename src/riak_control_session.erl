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

-module(riak_control_session).

-behavior(gen_server).

-ifdef (TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("riak_control/include/riak_control.hrl").

%% API
-export([start_link/0,
         get_version/0,
         get_ring/0,
         get_nodes/0,
         get_services/0,
         get_partitions/0,
         get_status/0,
         get_plan/0,
         clear_plan/0,
         stage_change/3,
         commit_plan/0,
         force_update/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% exported for RPC calls.
-export([get_my_info/0]).

-record(state, {vsn         :: version(),
                services    :: services(),
                ring        :: ring(),
                partitions  :: partitions(),
                nodes       :: members(),
                update_tick :: boolean()}).

-type normalized_action() :: leave
                           | remove
                           | replace
                           | force_replace
                           | stop
                           | down.

%% @doc Periodically update the ring with itself
-define(INTERVAL, 3000).

%% @doc Delay used after a ring update
-define(UPDATE_TICK_TIMEOUT, 1000).

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Start service from supervisor.
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).

%% @doc Return version.
-spec get_version() -> version().
get_version() ->
    gen_server:call(?MODULE, get_version, infinity).

%% @doc Get overall cluster status.
-spec get_status() -> {ok, version(), status()}.
get_status() ->
    gen_server:call(?MODULE, get_status, infinity).

%% @doc Return ring.
-spec get_ring() -> {ok, version(), ring()}.
get_ring() ->
    gen_server:call(?MODULE, get_ring, infinity).

%% @doc Return list of cluster members.
-spec get_nodes() -> {ok, version(), members()}.
get_nodes() ->
    gen_server:call(?MODULE, get_nodes, infinity).

%% @doc Return service list.
-spec get_services() -> {ok, version(), services()}.
get_services() ->
    gen_server:call(?MODULE, get_services, infinity).

%% @doc Return partition list.
-spec get_partitions() -> {ok, version(), partitions()}.
get_partitions() ->
    gen_server:call(?MODULE, get_partitions, infinity).

%% @doc Get the staged cluster plan.
-spec get_plan() -> {ok, list(), list()} | {error, atom()}.
get_plan() ->
    gen_server:call(?MODULE, get_plan, infinity).

%% @doc Stage a change to the cluster.
-spec stage_change(node(), normalized_action(), node()) ->
    ok | {error, stage_error()} | {badrpc, nodedown}.
stage_change(Node, Action, Replacement) ->
    gen_server:call(?MODULE,
                    {stage_change, Node, Action, Replacement}, infinity).

%% @doc Commit a staged cluster plan.
-spec commit_plan() -> ok | error.
commit_plan() ->
    gen_server:call(?MODULE, commit_plan, infinity).

%% @doc Clear the staged cluster plan.
-spec clear_plan() -> {ok, ok | error}.
clear_plan() ->
    gen_server:call(?MODULE, clear_plan, infinity).

%% @doc Force ring/membership update.
-spec force_update() -> ok.
force_update() ->
    gen_server:cast(?MODULE, update_ring).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

%% @doc
%%
%% Initialize and retrieve initial state.  Also, register watchers for
%% ring and membership changes.
%%
%% @end
init([]) ->
    %% make sure terminate/2 is called when the process exits
    process_flag(trap_exit,true),

    %% setup a callback that will see the ring changing as it happens
    %% so we can see block and wait for a change instead of just
    %% grabbing it every time
    add_node_watcher(),

    %% get the current ring so we have a baseline to work from
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),

    %% start a timer that will allow us to periodically ping cluster nodes
    erlang:send_after(?INTERVAL, self(), ping_ring_nodes),

    %% the initial state of the session
    State=#state{vsn=1,
                 partitions=[],
                 nodes=[],
                 services=[],
                 update_tick=false},

    %% start the server
    {ok, update_ring(State, Ring)}.

handle_call(commit_plan, _From, State) ->
    {reply, maybe_commit_plan(), State};
handle_call({stage_change, Node, Action, Replacement}, _From, State) ->
    {reply, maybe_stage_change(Node, Action, Replacement), State};
handle_call(clear_plan, _From, State) ->
    {reply, {ok, maybe_clear_plan()}, State};
handle_call(get_plan, _From, State) ->
    {reply, retrieve_plan(), State};
handle_call(get_version, _From, State=#state{vsn=V}) ->
    {reply, {ok, V}, State};
handle_call(get_status, _From, State=#state{vsn=V,nodes=N}) ->
    Status = determine_overall_status(N),
    {reply, {ok, V, Status}, State};
handle_call(get_ring, _From, State=#state{vsn=V,ring=R}) ->
    {reply, {ok, V, R}, State};
handle_call(get_nodes, _From, State=#state{vsn=V,nodes=N}) ->
    {reply, {ok, V, N}, State};
handle_call(get_services, _From, State=#state{vsn=V,services=S}) ->
    {reply, {ok, V, S}, State};
handle_call(get_partitions, _From, State=#state{vsn=V,partitions=P}) ->
    {reply, {ok, V, P}, State}.

%% @doc
%%
%% Whenever there's a change to the ring, we try and update our
%% localized view of "the world". However, ring updates can happen
%% very often, and large rings change *a lot*. For this reason,
%% we only want to update the ring every so often. So once we've
%% updated the ring, a tick flag is set indicating that we can't
%% update the ring any more until it's cleared.
%%
%% @end
handle_cast({update_ring, Ring}, State=#state{update_tick=Tick}) ->
    case Tick of
        false -> {noreply,update_ring(State,Ring)};
        true -> {noreply,State}
    end;
handle_cast(update_ring,State) ->
    {ok, Ring}=riak_core_ring_manager:get_my_ring(),
    {noreply, update_ring(State, Ring)};
handle_cast({update_services, Services}, State) ->
    {noreply, update_services(State, Services)}.

handle_info(ping_ring_nodes, State) ->
    erlang:send_after(?INTERVAL, self(), ping_ring_nodes),
    {ok, Ring}=riak_core_ring_manager:get_my_ring(),
    handle_cast({update_ring, Ring}, State);

handle_info(clear_update_tick, State) ->
    {noreply, State#state{update_tick=false}};
handle_info(_,State) ->
    {noreply, State}.

terminate (_Reason,_State) ->
    ok.

code_change (_Old,State,_Extra) ->
    {ok,State}.

%% ===================================================================
%% Private functions
%% ===================================================================

%% @doc Update state with new vsn to track changes.
-spec rev_state(#state{}) -> #state{}.
rev_state(State=#state{vsn=V}) ->
    State#state{vsn=V+1}.

%% @doc Register a watcher for membership changes.
-spec add_node_watcher() -> ok.
add_node_watcher() ->
    Self = self(),
    Fn = fun (Services) -> gen_server:cast(Self,{update_services,Services}) end,
    riak_core_node_watcher_events:add_sup_callback(Fn).

%% @doc Update list of services and partitions.
-spec update_services(#state{}, services()) -> #state{}.
update_services(State=#state{services=S}, Services) ->
    NewServices = lists:usort(S ++ Services),
    NodeState = update_nodes(State#state{services=NewServices}),
    NewState = update_partitions(NodeState),
    rev_state(NewState).

%% @doc Update ring state and partitions.
-spec update_ring(#state{}, ring()) -> #state{}.
update_ring(State, Ring) ->
    erlang:send_after(?UPDATE_TICK_TIMEOUT, self(), clear_update_tick),
    NodeState = update_nodes(State#state{update_tick=true, ring=Ring}),
    NewState = update_partitions(NodeState),
    rev_state(NewState).

%% @doc Update ring.
-spec update_nodes(#state{}) -> #state{}.
update_nodes(State=#state{ring=Ring}) ->
    Members = riak_core_ring:all_member_status(Ring),
    Nodes = [get_member_info(Member,Ring) || Member <- Members],
    State#state{nodes=Nodes}.

%% @doc Update partitions.
-spec update_partitions(#state{}) -> #state{}.
update_partitions(State=#state{ring=Ring}) ->
    Owners = riak_core_ring:all_owners(Ring),
    Handoffs = get_all_handoffs(State),
    Partitions = [get_partition_details(State, Owner, Handoffs) || Owner <- Owners],
    State#state{partitions=Partitions}.

%% @doc Ping and retrieve vnode workers.
-spec get_member_info({node(), status()}, ring()) -> member().
get_member_info(_Member={Node, Status}, Ring) ->
    RingSize = riak_core_ring:num_partitions(Ring),

    %% calculate how much of the ring this node owns vs. targetted
    Indices = riak_core_ring:indices(Ring,Node),
    FutureIndices = riak_core_ring:future_indices(Ring, Node),
    PctRing = length(Indices) / RingSize,
    PctPending = length(FutureIndices) / RingSize,

    %% try and get a list of all the vnodes running on the node
    case rpc:call(Node, riak_control_session, get_my_info, []) of
        {badrpc,nodedown} ->
            ?MEMBER_INFO{node = Node,
                         status = Status,
                         reachable = false,
                         vnodes = [],
                         handoffs = [],
                         ring_pct = PctRing,
                         pending_pct = PctPending};
        {badrpc,_Reason} ->
            ?MEMBER_INFO{node = Node,
                         status = incompatible,
                         reachable = true,
                         vnodes = [],
                         handoffs = [],
                         ring_pct = PctRing,
                         pending_pct = PctPending};
        MemberInfo = ?MEMBER_INFO{} ->
            MemberInfo?MEMBER_INFO{status = Status,
                                   ring_pct = PctRing,
                                   pending_pct = PctPending};
        MemberInfo0 = #member_info{} ->
            %% Upgrade older member information record.
            MemberInfo = upgrade_member_info(MemberInfo0),
            MemberInfo?MEMBER_INFO{status = Status,
                                   ring_pct = PctRing,
                                   pending_pct = PctPending};
        _ ->
            %% default case where a record incompatibility causes a
            %% failure matching the record format.
            ?MEMBER_INFO{node = Node,
                         status = incompatible,
                         reachable = true,
                         vnodes = [],
                         handoffs = [],
                         ring_pct = PctRing,
                         pending_pct = PctPending}
    end.

%% @doc Return current nodes information.
-spec get_my_info() -> member().
get_my_info() ->
    {Total, Used} = get_my_memory(),
    Handoffs = get_handoff_status(),
    VNodes = riak_core_vnode_manager:all_vnodes(),
    ErlangMemory = proplists:get_value(total,erlang:memory()),
    try
        case riak_core_capability:get({riak_control, member_info_version}) of
            v1 ->
                %% >= 1.4.1, where we have the upgraded cluster record.
                ?MEMBER_INFO{node = node(),
                             reachable = true,
                             mem_total = Total,
                             mem_used = Used,
                             mem_erlang = ErlangMemory,
                             vnodes = VNodes,
                             handoffs = Handoffs};
            v0 ->
                %% pre-1.4.1.
                handle_bad_record(Total, Used, ErlangMemory, VNodes, Handoffs)
        end
    catch
        _:{unknown_capability, _} ->
            %% capabilities are not registered yet.
            erlang:throw({badrpc, unknown_capability})
    end.

%% @doc Return current nodes memory.
-spec get_my_memory() -> {term(), term()}.
get_my_memory() ->
    Mem = memsup:get_system_memory_data(),

    %% get the total memory available to erlang and free memory
    {_, Total} = lists:keyfind(total_memory, 1, Mem),
    {_, Free} = lists:keyfind(free_memory, 1, Mem),

    %% buffered memory is available as well
    Buffered = case lists:keyfind(buffered_memory, 1, Mem) of
                 {_, BufferedMem} -> BufferedMem;
                 false -> 0
               end,

    %% so is cached memory
    Cached = case lists:keyfind(cached_memory, 1, Mem) of
               {_, CachedMem} -> CachedMem;
               false -> 0
             end,

    %% return the total memory an memory used
    {Total, Total - (Free + Cached + Buffered)}.

%% @doc Format a transfer.
-spec format_transfer({'status_v2',[any()]}) -> handoff().
format_transfer({status_v2, Handoff}) ->
    Mod = proplists:get_value(mod, Handoff),
    SrcPartition = proplists:get_value(src_partition, Handoff),
    SrcNode = proplists:get_value(src_node, Handoff),
    {Mod, SrcPartition, SrcNode}.

%% @doc Get handoffs for this node.
-spec get_handoff_status() -> handoffs().
get_handoff_status() ->
    Transfers = riak_core_handoff_manager:status({direction, outbound}),
    [format_transfer(Transfer) || Transfer <- lists:flatten(Transfers)].

%% @doc Get handoffs for every node.
-spec get_all_handoffs(#state{}) -> handoffs().
get_all_handoffs(#state{nodes=Members}) ->
    lists:flatten([HS || ?MEMBER_INFO{handoffs=HS} <- Members]).

%% @doc Get information for a particular index.
-spec get_partition_details(#state{}, {integer(), term()}, handoffs())
    -> #partition_info{}.
get_partition_details(#state{services=Services, ring=Ring}, {Idx, Owner}, HS) ->
    Statuses = [get_vnode_status(Service, Ring, Idx) || Service <- Services],
    Handoffs = [{Mod, Node} || {Mod, I, Node} <- HS, I == Idx],
    #partition_info{index = Idx,
                    partition = partition_index(Ring,Idx),
                    owner = Owner,
                    vnodes = Statuses,
                    handoffs = Handoffs}.

%% @doc Given a partition index, return.
-spec partition_index(ring(), integer()) -> term().
partition_index(Ring, Index) ->
    NumPartitions = riak_core_ring:num_partitions(Ring),
    Index div chash:ring_increment(NumPartitions).

%% @doc Return current vnode status.
-spec get_vnode_status(term(), term(), integer()) -> {service(), home()}.
get_vnode_status(Service, Ring, Index) ->
    UpNodes = riak_core_node_watcher:nodes(Service),
    case riak_core_apl:get_apl_ann(<<(Index-1):160>>, 1, Ring, UpNodes) of
        [{{_, _}, Status}|_] ->
            {Service, Status};
        [] ->
            {Service, undefined}
    end.

%% @doc Attempt to clear the cluster plan.
-spec maybe_clear_plan() -> ok | error.
maybe_clear_plan() ->
    try riak_core_claimant:clear() of
        ok ->
            ok
    catch
        _:_ ->
            error
    end.

%% @doc Return list of nodes, current and future claim.
-spec nodes_and_claim_percentages(ring()) -> list().
nodes_and_claim_percentages(Ring) ->
    Nodes = lists:keysort(2, riak_core_ring:all_member_status(Ring)),
    [{Name, riak_core_console:pending_claim_percentage(Ring, Name)} ||
        {Name, _} <- Nodes].

%% @doc Compute from a series of ring transitions, the final ring.
-spec compute_final_ring_claim(list({ring(), ring()})) ->
    [{node(),{number(),number()}}].
compute_final_ring_claim(Rings) ->
    {_, FinalRing} = lists:last(Rings),
    nodes_and_claim_percentages(FinalRing).

%% @doc Attempt to retrieve the claim plan.
-spec retrieve_plan() -> {ok, list(), list(ring())} | {error, atom()}.
retrieve_plan() ->
    try riak_core_claimant:plan() of
        {error, Error} ->
            {error, Error};
        {ok, Changes, NextRings} ->
            case Changes of
                [] ->
                    {ok, [], []};
                _ ->
                    {ok, Changes, compute_final_ring_claim(NextRings)}
            end
    catch
        _:_ ->
            {error, unknown}
    end.

%% @doc Attempt to commit the plan.
-spec maybe_commit_plan() -> ok | {error, term()}.
maybe_commit_plan() ->
    riak_core_claimant:commit().

%% @doc Stage a change for one particular node.
-spec maybe_stage_change(node(), normalized_action(), node()) ->
    ok | {error, stage_error()} | {badrpc, nodedown}.
maybe_stage_change(Node, Action, Replacement) ->
    case Action of
        join ->
            {ok, Ring} = riak_core_ring_manager:get_my_ring(),
            case riak_core_ring:all_members(Ring) of
                [_Me] ->
                    riak_core:staged_join(Node);
                _ ->
                    rpc:call(Node, riak_core, staged_join, [node()])
            end;
        leave ->
            riak_core_claimant:leave_member(Node);
        remove ->
            riak_core_claimant:remove_member(Node);
        replace ->
            riak_core_claimant:replace(Node, Replacement);
        force_replace ->
            riak_core_claimant:force_replace(Node, Replacement);
        down ->
            riak_core:down(Node);
        stop ->
            rpc:call(Node, riak_core, stop, [])
    end.

%% @doc Conditionally upgrade member info records once they cross node
%%      boundaries.
-spec upgrade_member_info(member() | #member_info{}) -> member().
upgrade_member_info(MemberInfo = ?MEMBER_INFO{}) ->
    MemberInfo;
upgrade_member_info(MemberInfo = #member_info{}) ->
    ?MEMBER_INFO{
        node = MemberInfo#member_info.node,
        status = MemberInfo#member_info.status,
        reachable = MemberInfo#member_info.reachable,
        vnodes = MemberInfo#member_info.vnodes,
        handoffs = MemberInfo#member_info.handoffs,
        ring_pct = MemberInfo#member_info.ring_pct,
        pending_pct = MemberInfo#member_info.pending_pct,
        mem_total = MemberInfo#member_info.mem_total,
        mem_used = MemberInfo#member_info.mem_used,
        mem_erlang = MemberInfo#member_info.mem_erlang}.

%% @doc Handle incompatible record for the 1.4.0 release.
handle_bad_record(Total, Used, ErlangMemory, VNodes, Handoffs) ->
    Counters = riak_core_capability:get({riak_kv, crdt}),
    case lists:member(pncounter, Counters) of
        true ->
            %% 1.4.0, where we have a bad record.
            {member_info,
             node(), incompatible, true, VNodes, Handoffs, undefined,
             undefined, Total, Used, ErlangMemory, undefined,
             undefined};
        false ->
            %% < 1.4.0, where we have the old style record.
            #member_info{node = node(),
                         reachable = true,
                         mem_total = Total,
                         mem_used = Used,
                         mem_erlang = ErlangMemory,
                         vnodes = VNodes,
                         handoffs = Handoffs}
    end.

%% @doc Determine overall cluster status.
%%      If the cluster is of one status, return it; default to valid.
%%      If one or more nodes is incompatible, return incompatible, else 
%%      introduce a new state called transitioning.
-spec determine_overall_status(members()) -> status().
determine_overall_status(Nodes) ->
    Statuses = lists:usort([Node?MEMBER_INFO.status || Node <- Nodes]),
    case length(Statuses) of
        0 ->
            valid;
        1 ->
            lists:nth(1, Statuses);
        _ ->
            case lists:member(incompatible, Statuses) of
                true ->
                    incompatible;
                false ->
                    transitioning
            end
    end.
