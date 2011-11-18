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
         service_available/2
        ]).

%% webmachine dependencies
-include_lib("webmachine/include/webmachine.hrl").

%% mappings to the various content types supported for this resource
-define(CONTENT_TYPES,[{"application/json",to_json}]).

%% the different vnode types we care about
-define(VNODE_TYPES,[riak_kv,riak_pipe,riak_search]).

%% defines the webmachine routes this module handles
routes () ->
    [{admin_routes:ring_route(["partitions"]),?MODULE,all},
     {admin_routes:ring_route(["partitions","filter","none"]),?MODULE,none},
     {admin_routes:ring_route(["partitions","filter","node",node]),?MODULE,node}
    ].

%% entry-point for the resource from webmachine
init (Filter) ->
    {ok,_V,Partitions}=riak_control_session:get_partitions(),
    {ok,{Partitions,Filter}}.

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
to_json (Req,C={Partitions,Filter}) ->
    PS=filter_partitions(Req,Partitions,Filter),
    Details=[{struct,node_ring_details(P)} || P <- PS],
    {mochijson2:encode(Details),Req,C}.

%% filter a ring based on a given filter name
filter_partitions (Req,PS,node) ->
    Node=list_to_existing_atom(dict:fetch(node,wrq:path_info(Req))),
    [P || P={_,N,_} <- PS, N==Node];
filter_partitions (_Req,PS,all) ->
    PS;
filter_partitions (_Req,_PS,_) ->
    [].

%% return a proplist of details for a given index
node_ring_details (_P={Index,I,OwnerNode,Vnodes}) ->
    [{index,Index},
     {i,I},
     {node,OwnerNode},
     {vnodes,Vnodes}
    ].
