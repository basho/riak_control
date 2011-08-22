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

%% @doc Exposes services information for a node
%%
%%      The resource exposes itself at `/admin/nodes/nodename'.
-module(admin_services_resource).

-export([
         routes/0,
         init/1,
         service_available/2,
         is_authorized/2,
         allowed_methods/2,
         content_types_provided/2,
         resource_exists/2,
         produce_json/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include("riak_control.hrl").

-record(ctx, {
          nodename
         }).

-type context() :: #ctx{}.
-type method()  :: 'PUT' | 'POST' | 'GET' | 'HEAD' | 'DELETE'.

%%% riak_control_sup API

%% @doc Get the webmachine dispatcher config for this resource.
-spec routes() -> [webmachine_dispatcher:matchterm()].
routes() ->
    [{?ADMIN_BASE_ROUTE++["nodes", 'nodename', "services"],
      ?MODULE,
      []}].

%%% Webmachine API

-spec init(list()) -> {ok, context()}.
init(_Props) ->
    {ok, #ctx{}}.

-spec allowed_methods(wrq:reqdata(), context()) ->
                             {[method()], wrq:request(), context()}.
allowed_methods(RD, Ctx) ->
    {['GET'], RD, Ctx}.

-spec service_available(wrq:reqdata(), context()) ->
                               {boolean() | {halt, non_neg_integer()}, wrq:reqdata(), context()}.
service_available(RD, Ctx) ->
    riak_control_security:scheme_is_available(RD, Ctx).

-spec is_authorized(wrq:reqdata(), context()) ->
                           {true | string(), wrq:reqdata(), context()}.
is_authorized(RD, Ctx) ->
    riak_control_security:enforce_auth(RD, Ctx).

-spec content_types_provided(wrq:reqdata(), context()) ->
         {[{ContentType::string(), HandlerFunction::atom()}],
          wrq:reqdata(), context()}.
content_types_provided(RD, Ctx) ->
    {[{"application/json", produce_json}], RD, Ctx}.

-spec resource_exists(wrq:reqdata(), context()) ->
                             {boolean(), wrq:reqdata(), context()}.
resource_exists(RD, Ctx) ->
    Node = list_to_atom(wrq:path_info(nodename, RD)),
    NodeCtx = Ctx#ctx{nodename=Node},
    {is_node_in_cluster(Node), RD, NodeCtx}.

-spec produce_json(wrq:reqdata(), context()) ->
         {binary(), wrq:reqdata(), context()}.
produce_json(RD, #ctx{nodename=Node}=Ctx) ->
    Services = service_initials(riak_core_node_watcher:services(Node), []),
    JSON = mochijson2:encode([{services, Services}]),
    {JSON, RD, Ctx}.

%% ===================================================================
%% Internal functions
%% ===================================================================
is_node_in_cluster(Node) when is_atom(Node) ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    Members = riak_core_ring:all_members(Ring),
    lists:member(Node, Members).

service_initials([], Acc) ->
    lists:reverse(Acc);
service_initials([H|T], Acc) ->
    service_initials(T, [{struct,[{name, H}, {initial, service_initial(H)}]}|Acc]).

%% Get the display initial for the service
%% TODO maybe something more sophisticated
%% Like capitalise first letter after riak_
service_initial(riak_kv) ->
    'K';
service_initial(riak_pipe) ->
    'P';
service_initial(riak_search) ->
    'S';
service_initial(Service) ->
    Service.
