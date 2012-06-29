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
    Filter=wrq:get_qs_value("filter",Req),
    PS=filter_partitions(Req,Partitions,Filter),
    {Page,N,XS}=paginate_results(Req,PS),
    Details=[{struct,node_ring_details(P,Nodes)} || P <- XS],
    {mochijson2:encode({struct,[{pages,N},
                                {page,Page},
                                {contents,Details}
                               ]}),
     Req,C}.

%% filter a ring based on a given filter name
filter_partitions (Req,PS,"node") ->
    Node=try
             list_to_existing_atom(wrq:get_qs_value("q","undefined",Req))
         catch
             _:_ -> undefined
         end,
    [P || P=#partition_info{owner=N} <- PS, N==Node];
filter_partitions (_Req,PS,"fallback") ->
    [P || P=#partition_info{vnodes=V} <- PS,
          lists:keyfind(fallback,2,V) =/= false];
filter_partitions (_Req,PS,"handoff") ->
    [P || P=#partition_info{handoffs=H} <- PS, H =/= []];
filter_partitions (_Req,PS,_) ->
    PS.

%% the url can optionally specify paging for the partitions
paginate_results (Req,XS) ->
    Len=length(XS),
    N=max(16,list_to_integer(wrq:get_qs_value("n","64",Req))),

    case N > Len of
        true ->
            {1,1,XS};
        false ->
            D=trunc(Len / N),

            %% there might be a left over page with < N items on it
            Pages=case D * N of
                      Len -> D;
                      _ -> D+1
                  end,

            %% cap the page # we can be within
            P=min(Pages,max(1,list_to_integer(wrq:get_qs_value("p","1",Req)))),

            %% if this is the last page, just grab whatever's left over
            Contents=case P of
                         Pages -> {_,C}=lists:split((P-1)*N,XS), C;
                         _ -> {_,Rest}=lists:split((P-1)*N,XS),
                              {C,_}=lists:split(N,Rest),
                              C
                     end,

            %% page #, total page count, contents of this page
            {P,Pages,Contents}
    end.

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
