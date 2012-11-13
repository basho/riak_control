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

-module(riak_control_formatting).

-export([action_result/3,
         node_ring_details/2]).

-include("riak_control.hrl").

%% all actions return the same format
action_result(Error={badrpc,_},Req,C) ->
    Body = mochijson2:encode({struct,[Error]}),
    {false, wrq:set_resp_body(Body, Req), C};
action_result(Error={error,_},Req,C) ->
    Body = mochijson2:encode({struct,[Error]}),
    {false, wrq:set_resp_body(Body, Req), C};
action_result(_,Req,C) ->
    Body = mochijson2:encode({struct,[{result,ok}]}),
    {true, wrq:set_resp_body(Body, Req), C}.

%% return a proplist of details for a given index
node_ring_details (P=#partition_info{index=Index,vnodes=Vnodes},Nodes) ->
    case lists:keyfind(P#partition_info.owner,2,Nodes) of
        #member_info{node=Node,status=Status,reachable=Reachable} ->
            Handoffs = P#partition_info.handoffs,
            VnodeStatuses = [{atom_to_list(VnodeName) ++
                              "_vnode_status", vnode_status(VnodeName, VnodeStatus, Handoffs)}
                             || {VnodeName, VnodeStatus} <- Vnodes],
            NodeDetails = [{index,list_to_binary(integer_to_list(Index))},
                       {i,P#partition_info.partition},
                       {node,Node},
                       {status,Status},
                       {reachable,Reachable}
            ],
            NodeDetails ++ VnodeStatuses;
        false -> []
    end.

vnode_status(Service, Status, Handoffs) ->
    Vnodes = riak_core:vnode_modules(),
    Worker = proplists:get_value(Service, Vnodes),
    case proplists:get_value(Worker, Handoffs) of
        undefined ->
            Status;
        _ ->
            handoff
    end.
