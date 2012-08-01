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

%% API
-export([start_link/0,
         get_version/0,
         get_ring/0,
         get_nodes/0,
         get_services/0,
         get_partitions/0,
         force_update/0
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%% private method only used in rpc calls
-export([get_my_info/0]).

%% record definitions
-include_lib("riak_control/include/riak_control.hrl").

-record(state,
        { vsn         :: version(),
          services    :: [service()],
          ring        :: riak_core_ring:riak_core_ring(),
          partitions  :: [#partition_info{}],
          nodes       :: [#member_info{}],
          update_tick :: boolean()
        }).

%% hack: periodically update the ring with itself
-define(INTERVAL, 3000).

%% delay used after a ring update
-define(UPDATE_TICK_TIMEOUT, 1000).

%% ===================================================================
%% Public API
%% ===================================================================

start_link () ->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).

get_version () ->
    gen_server:call(?MODULE,get_version,infinity).

get_ring () ->
    gen_server:call(?MODULE,get_ring,infinity).

get_nodes () ->
    gen_server:call(?MODULE,get_nodes,infinity).

get_services () ->
    gen_server:call(?MODULE,get_services,infinity).

get_partitions () ->
    gen_server:call(?MODULE,get_partitions,infinity).

force_update () ->
    gen_server:cast(?MODULE,update_ring,infinity).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init ([]) ->
    %% make sure terminate/2 is called when the process exits
    process_flag(trap_exit,true),

    %% setup a callback that will see the ring changing as it happens
    %% so we can see block and wait for a change instead of just
    %% grabbing it every time
    add_ring_watcher(),
    add_node_watcher(),

    %% get the current ring so we have a baseline to work from
    {ok,Ring}=riak_core_ring_manager:get_my_ring(),

    %% start a timer that will allow us

    %% start a timer that will allow us to periodically ping cluster nodes
    erlang:send_after(?INTERVAL,self(),ping_ring_nodes),

    %% the initial state of the session
    State=#state{ vsn=1,
                  partitions=[],
                  nodes=[],
                  services=[],
                  update_tick=false
                },

    %% start the server
    {ok,update_ring(State,Ring)}.

%% calls that block until a reply is sent
handle_call (get_version,_From,State=#state{vsn=V}) ->
    {reply,{ok,V},State};

handle_call (get_ring,_From,State=#state{vsn=V,ring=R}) ->
    {reply,{ok,V,R},State};

handle_call (get_nodes,_From,State=#state{vsn=V,nodes=N}) ->
    {reply,{ok,V,N},State};

handle_call (get_services,_From,State=#state{vsn=V,services=S}) ->
    {reply,{ok,V,S},State};

handle_call (get_partitions,_From,State=#state{vsn=V,partitions=P}) ->
    {reply,{ok,V,P},State}.

%% calls that don't block and don't reply
handle_cast ({update_ring,Ring},State=#state{update_tick=Tick}) ->
    %% Whenever there's a change to the ring, we try and update our
    %% localized view of "the world". However, ring updates can happen
    %% very often, and large rings change *a lot*. For this reason,
    %% we only want to update the ring every so often. So once we've
    %% updated the ring, a tick flag is set indicating that we can't
    %% update the ring any more until it's cleared.
    case Tick of
        false -> {noreply,update_ring(State,Ring)};
        true -> {noreply,State}
    end;

handle_cast (update_ring,State) ->
    {ok,Ring}=riak_core_ring_manager:get_my_ring(),
    {noreply,update_ring(State,Ring)};

handle_cast ({update_services,Services},State) ->
    {noreply,update_services(State,Services)}.

%% misc. messages
handle_info (ping_ring_nodes,State) ->
    erlang:send_after(?INTERVAL,self(),ping_ring_nodes),
    {ok,Ring}=riak_core_ring_manager:get_my_ring(),
    handle_cast({update_ring,Ring},State);

handle_info (clear_update_tick,State) ->
    {noreply,State#state{update_tick=false}};

handle_info (_,State) ->
    {noreply,State}.

terminate (_Reason,_State) ->
    ok.

code_change (_Old,State,_Extra) ->
    {ok,State}.

%% ===================================================================
%% Private functions
%% ===================================================================

rev_state (State=#state{vsn=V}) ->
    State#state{vsn=V+1}.

add_ring_watcher () ->
    Self=self(),
    Fn=fun (Ring) ->
               gen_server:cast(Self,{update_ring,Ring})
       end,
    riak_core_ring_events:add_sup_callback(Fn).

add_node_watcher () ->
    Self=self(),
    Fn=fun (Services) ->
               gen_server:cast(Self,{update_services,Services})
       end,
    riak_core_node_watcher_events:add_sup_callback(Fn).

%% update the services list, also update the partitions
update_services (State=#state{services=S},Services) ->
    NewServices=lists:usort(S ++ Services),
    NodeState=update_nodes(State#state{services=NewServices}),
    NewState=update_partitions(NodeState),
    rev_state(NewState).

%% update the ring state, also update nodes and partitions
update_ring (State,Ring) ->
    erlang:send_after(?UPDATE_TICK_TIMEOUT,self(),clear_update_tick),
    NodeState=update_nodes(State#state{update_tick=true,ring=Ring}),
    NewState=update_partitions(NodeState),
    rev_state(NewState).

%% update the state cache with node status information
update_nodes (State=#state{ring=Ring}) ->
    Members=riak_core_ring:all_member_status(Ring),
    Nodes=[get_member_info(Member,Ring) || Member <- Members],
    State#state{nodes=Nodes}.

%% update the state information with partitions
update_partitions (State=#state{ring=Ring}) ->
    Owners=riak_core_ring:all_owners(Ring),
    Handoffs=get_all_handoffs(State),
    Partitions=[get_partition_details(State,Owner,Handoffs) || Owner <- Owners],
    State#state{partitions=Partitions}.

%% ping a node and get all its vnode workers at the same time
get_member_info (_Member={Node,Status},Ring) ->
    RingSize=riak_core_ring:num_partitions(Ring),

    %% calculate how much of the ring this node owns vs. targetted
    Indices=riak_core_ring:indices(Ring,Node),
    FutureIndices=riak_core_ring:future_indices(Ring,Node),
    PctRing=length(Indices) / RingSize,
    PctPending=length(FutureIndices) / RingSize,

    %% try and get a list of all the vnodes running on the node
    case rpc:call(Node,riak_control_session,get_my_info,[]) of
        {badrpc,nodedown} ->
            #member_info{ node=Node,
                          status=Status,
                          reachable=false,
                          vnodes=[],
                          handoffs=[],
                          ring_pct=PctRing,
                          pending_pct=PctPending
                        };
        {badrpc,_Reason} ->
            #member_info{ node=Node,
                          status=incompatible,
                          reachable=true,
                          vnodes=[],
                          handoffs=[],
                          ring_pct=PctRing,
                          pending_pct=PctPending
                        };
        MemberInfo = #member_info{} ->
            %% there is a race condition here, when a node is stopped
            %% gracefully (e.g. `riak stop`) the event will reach us
            %% before the node is actually down and the rpc call will
            %% succeed, but since it's shutting down it won't have any
            %% vnode workers running...
            MemberInfo#member_info{ status=Status,
                                    ring_pct=PctRing,
                                    pending_pct=PctPending
                                  }
    end.

%% run locally per-node, collects information about this node for the session
get_my_info () ->
    {Total,Used}=get_my_memory(),

    %% construct the member information for this node
    #member_info{ node=node(),
                  reachable=true,
                  mem_total=Total,
                  mem_used=Used,
                  mem_erlang=proplists:get_value(total,erlang:memory()),
                  vnodes=riak_core_vnode_manager:all_vnodes(),
                  handoffs=get_handoff_status()
                  }.

%% format transfer to unifed display.
format_transfer(Transfer) ->
    case Transfer of
        {{Mod, Partition}, Node, outbound, _, _} ->
            {Mod, Partition, Node};
        {status_v2, Status} ->
            Mod = proplists:get_value(mod, Status),
            SrcPartition = proplists:get_value(src_partition, Status),
            SrcNode = proplists:get_value(src_node, Status),
            {Mod, SrcPartition, SrcNode}
    end.

%% get handoffs.
get_handoff_status() ->
    {Transfers, _Down} = riak_core_status:all_active_transfers([{direction, outbound}]),
    [format_transfer(Transfer) || Transfer <- lists:flatten(Transfers)].

%% get memory information for this machine
get_my_memory () ->
    Mem=memsup:get_system_memory_data(),

    %% get the total memory available to erlang and free memory
    {_,Total}=lists:keyfind(total_memory,1,Mem),
    {_,Free}=lists:keyfind(free_memory,1,Mem),

    %% buffered memory is available as well
    Buffered=case lists:keyfind(buffered_memory,1,Mem) of
                 {_,BufferedMem} -> BufferedMem;
                 false -> 0
             end,

    %% so is cached memory
    Cached=case lists:keyfind(cached_memory,1,Mem) of
               {_,CachedMem} -> CachedMem;
               false -> 0
           end,

    %% return the total memory an memory used
    {Total,Total - (Free + Cached + Buffered)}.


%% each node knows about its set of handoffs, collect them all together
get_all_handoffs(#state{nodes=Members}) ->
    lists:flatten([HS || #member_info{handoffs=HS} <- Members]).

%% return a proplist of details for a given index
get_partition_details (#state{services=Services,ring=Ring},{Idx,Owner},HS) ->
    Statuses=[get_vnode_status(Service,Ring,Idx) || Service <- Services],
    Handoffs=[{Mod,Node} || {Mod,I,Node} <- HS, I==Idx],
    #partition_info{ index=Idx,
                     partition=partition_index(Ring,Idx),
                     owner=Owner,
                     vnodes=Statuses,
                     handoffs=Handoffs
                   }.

%% get the partition number of a given index
partition_index (Ring,Index) ->
    NumPartitions=riak_core_ring:num_partitions(Ring),
    Index div chash:ring_increment(NumPartitions).

%% get the current status of a vnode for a given partition
get_vnode_status (Service,Ring,Index) ->
    UpNodes=riak_core_node_watcher:nodes(Service),
    case riak_core_apl:get_apl_ann(<<(Index-1):160>>,1,Ring,UpNodes) of
        [{{_,_Node},Status}|_] -> {Service,Status};
        [] -> {Service,undefined}
    end.


%% ---------------------------------------------------------------------------
%% EUNIT tests

-ifdef(EQC).

%% we want to see quickcheck output
-define(QC_OUT(P),
        eqc:on_output(fun(Str, Args) ->
                              io:format(user, Str, Args)
                      end,
                      P)).

quickcheck_test_ () ->
    {timeout, 120,
     fun() ->
             ?assert(eqc:quickcheck(?QC_OUT(eqc_routes:prop_routes())))
     end}.

-endif.
