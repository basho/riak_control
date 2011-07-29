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

%% @doc Add/remove/examine node
%%
%%      The resource exposes itself at `/admin/nodes/nodename'.
-module(admin_node_resource).

-export([
         routes/0,
         init/1,
         service_available/2,
         is_authorized/2,
         allowed_methods/2,
         content_types_accepted/2,
         resource_exists/2,
         accept_json/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include("riak_control.hrl").

-record(ctx, {
          base_url,
          ring_members,
          nodename
         }).

-type context() :: #ctx{}.
-type method()  :: 'PUT' | 'POST' | 'GET' | 'HEAD' | 'DELETE'.

%%% riak_control_sup API

%% @doc Get the webmachine dispatcher config for this resource.
-spec routes() -> [webmachine_dispatcher:matchterm()].
routes() ->
    [{?ADMIN_BASE_ROUTE++["nodes", 'nodename'],
      ?MODULE,
      [{base_url, ?ADMIN_BASE_PATH++"nodes/"}]}].

%%% Webmachine API

-spec init(list()) -> {ok, context()}.
init(Props) ->
    {base_url, Url} = lists:keyfind(base_url, 1, Props),
    {ok, #ctx{base_url=Url}}.

-spec allowed_methods(wrq:reqdata(), context()) ->
                             {[method()], wrq:request(), context()}.
allowed_methods(RD, Ctx) ->
    {['PUT'], RD, Ctx}.

-spec service_available(wrq:reqdata(), context()) ->
         {boolean() | {halt, non_neg_integer()}, wrq:reqdata(), context()}.
service_available(RD, Ctx) ->
    riak_control_security:scheme_is_available(RD, Ctx).

-spec is_authorized(wrq:reqdata(), context()) ->
         {true | string(), wrq:reqdata(), context()}.
is_authorized(RD, Ctx) ->
    riak_control_security:enforce_auth(RD, Ctx).

-spec content_types_accepted(wrq:reqdata(), context()) ->
         {[{ContentType::string(), HandlerFunction::atom()}],
          wrq:reqdata(), context()}.
content_types_accepted(RD, Ctx) ->
    {[{"application/json", accept_json}], RD, Ctx}.

-spec resource_exists(wrq:reqdata(), context()) ->
         {boolean(), wrq:reqdata(), context()}.
resource_exists(RD, Ctx) ->
    NewNode = list_to_atom(wrq:path_info(nodename, RD)),
    case riak_core_ring_manager:get_my_ring() of
        {ok, Ring} ->
            Members = riak_core_ring:all_members(Ring),
            RingCtx = Ctx#ctx{ring_members=Members, nodename=NewNode},
            {lists:member(NewNode, Members), RD, RingCtx};
        _Error ->
            {false, RD, Ctx}
    end.

-spec accept_json(wrq:reqdata(), context()) ->
         {binary(), wrq:reqdata(), context()}.
accept_json(RD, #ctx{nodename=NewNode}=Ctx) ->
    ok = riak_core_gossip:send_ring(node(), NewNode),
    {true, RD, Ctx}.
