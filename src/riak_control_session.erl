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

%% API
-export([start_link/0,
         get_version/0,
         get_ring/0,
         get_nodes/0,
         get_services/0,
         get_partitions/0
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
          partitions  :: [tuple()],
          nodes       :: [tuple()],
          update      :: reference()
        }).

%% hack: periodically update the ring with itself
-define(INTERVAL, 3000).

%% delay used after a ring update
-define(RING_UPDATE_DELAY, 500).

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
                  update=make_ref()
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
handle_cast ({update_ring,Ring},State=#state{update=TimerRef}) ->
    %% Whenever we receive a ring update, what we actually do is start
    %% a timer that will actually process the new ring after a certain
    %% period of time has elapsed. We do this because ring updates come
    %% in spurts and we don't want to process all of those changes, we
    %% only want the last change.

    %% cancel the current delay timer, and wait a bit longer
    erlang:cancel_timer(TimerRef),

    %% setup the new timer to actually update with the new ring
    NewRef=erlang:send_after(?RING_UPDATE_DELAY,
                             self(),
                             {actually_update_ring,Ring}),
    {noreply,State#state{update=NewRef}};

handle_cast ({update_services,Services},State) ->
    {noreply,update_services(State,Services)}.

%% misc. messages
handle_info (ping_ring_nodes,State=#state{ring=Ring}) ->
    erlang:send_after(?INTERVAL,self(),ping_ring_nodes),
    handle_cast({update_ring,Ring},State);

handle_info ({actually_update_ring,Ring},State) ->
    {noreply,update_ring(State,Ring)};

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
    NodeState=update_nodes(State#state{ring=Ring}),
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
    Partitions=[get_partition_details(State,Owner) || Owner <- Owners],
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
        {badrpc,_Reason} ->
            #member_info{ node=Node,
                          status=Status,
                          reachable=false,
                          vnodes=[],
                          handoffs=[],
                          ring_pct=PctRing,
                          pending_pct=PctPending
                        };
        Member_info ->
            %% there is a race condition here, when a node is stopped
            %% gracefully (e.g. `riak stop`) the event will reach us
            %% before the node is actually down and the rpc call will
            %% succeed, but since it's shutting down it won't have any
            %% vnode workers running...
            Member_info#member_info{ status=Status,
                                     ring_pct=PctRing,
                                     pending_pct=PctPending
                                   }
    end.

%% run locally per-node, collects information about this node for the session
get_my_info () ->
    {Total,Used,_}=memsup:get_memory_data(),
    {ok,Active,Pending}=riak_core_handoff_manager:all_handoffs(),

    %% construct the member information for this node
    #member_info{ node=node(),
                  reachable=true,
                  mem_total=Total,
                  mem_used=Used,
                  mem_erlang=proplists:get_value(total,erlang:memory()),
                  vnodes=riak_core_vnode_manager:all_vnodes(),
                  handoffs=Active ++ Pending
                }.

%% return a proplist of details for a given index
get_partition_details (#state{services=S,nodes=Nodes,ring=R},{Index,Node}) ->
    case lists:keysearch(Node,2,Nodes) of
        {value,#member_info{vnodes=Vnodes,handoffs=HS}} ->
            Statuses=[get_vnode_status(Service,R,Index,Vnodes) || Service <- S],
            Handoffs=[{M,N} || {{M,I},N,_} <- HS, I==Index],
            case Handoffs of
                [] -> ok;
                _  -> io:format(">>>>> ~w~n", [Handoffs])
            end,
            #partition_info{ index=Index,
                             partition=partition_index(R,Index),
                             owner=Node,
                             vnodes=Statuses,
                             handoffs=Handoffs
                           };
        false ->
            #partition_info{ index=Index,
                             partition=partition_index(R,Index),
                             owner=Node,
                             vnodes=[],
                             handoffs=[]
                           }
    end.

%% get the partition number of a given index
partition_index (Ring,Index) ->
    NumPartitions=riak_core_ring:num_partitions(Ring),
    Index div chash:ring_increment(NumPartitions).

%% get the current status of a vnode for a given partition
get_vnode_status (Service,Ring,Index,_Vnodes) ->
    UpNodes=riak_core_node_watcher:nodes(Service),
    case riak_core_apl:get_apl_ann(<<(Index-1):160>>,1,Ring,UpNodes) of
        [{{_,_Node},Status}|_] -> {Service,Status};
        [] -> {Service,undefined}
    end.

