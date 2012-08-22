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

-module(admin_ring).
-export([routes/0,
         init/1,
         content_types_provided/2,
         to_json/2,
         is_authorized/2,
         service_available/2,
         forbidden/2,
         node_ring_details/2
        ]).

%% riak_control and webmachine dependencies
-include_lib("riak_control/include/riak_control.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%% mappings to the various content types supported for this resource
-define(CONTENT_TYPES,[{"application/json",to_json}]).

%% the different vnode types we care about
-define(VNODE_TYPES,[riak_kv,riak_pipe,riak_search]).

%% defines the webmachine routes this module handles
routes () ->
    [{admin_routes:ring_route(["partitions"]),?MODULE,[]}].

%% entry-point for the resource from webmachine
init ([]) ->
    {ok,_V,Partitions}=riak_control_session:get_partitions(),
    {ok,Partitions}.

%% validate origin
forbidden(RD, C) ->
    {riak_control_security:is_null_origin(RD), RD, C}.

%% redirect to SSL port if using HTTP
service_available (RD,C) ->
    riak_control_security:scheme_is_available(RD,C).

%% validate username and password
is_authorized (RD,C) ->
    riak_control_security:enforce_auth(RD,C).

%% return the list of available content types for webmachine
content_types_provided (Req,C) ->
    {?CONTENT_TYPES,Req,C}.

%% valid | invalid | joining | leaving | exiting
to_json (Req,C=Partitions) ->
    {ok,_V,Nodes}=riak_control_session:get_nodes(),
    Details=[{struct,node_ring_details(P,Nodes)} || P <- Partitions],
    {mochijson2:encode({struct,[{partitions,Details}]}), Req,C}.

%% return a proplist of details for a given index
node_ring_details (P=#partition_info{index=Index,vnodes=Vnodes},Nodes) ->
    case lists:keyfind(P#partition_info.owner,2,Nodes) of
        #member_info{node=Node,status=Status,reachable=Reachable} ->
            [{index,list_to_binary(integer_to_list(Index))},
             {i,P#partition_info.partition},
             {node,Node},
             {status,Status},
             {reachable,Reachable},
             {vnodes,Vnodes},
             {handoffs,{struct,vnode_handoffs(P#partition_info.handoffs)}}
            ];
        false -> []
    end.

%% determine the status for each vnode worker and if there's a handoff
vnode_handoffs (Hoffs) ->
    lists:foldl(fun ({Service,Worker},Acc) ->
                        case proplists:get_value(Worker,Hoffs) of
                            undefined -> Acc;
                            Target -> [{Service,Target}|Acc]
                        end
                end,
                [],
                riak_core:vnode_modules()).
