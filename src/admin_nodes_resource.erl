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

%% @doc Basic cluster membership inspection.
%%
%%      The resource exposes itself at `/admin/nodes'.
-module(admin_nodes_resource).

-export([
         routes/0,
         init/1,
         service_available/2,
         is_authorized/2,
         content_types_provided/2,
         resource_exists/2,
         produce_json/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include("riak_control.hrl").

-record(ctx, {
          base_url,
          ring
         }).

-type context() :: #ctx{}.

%%% riak_control_sup API

%% @doc Get the webmachine dispatcher config for this resource.
-spec routes() -> [webmachine_dispatcher:matchterm()].
routes() ->
    [{?ADMIN_BASE_ROUTE++["nodes"],
      ?MODULE,
      [{base_url, ?ADMIN_BASE_PATH++"nodes/"}]}].

%%% Webmachine API

-spec init(list()) -> {ok, context()}.
init(Props) ->
    {base_url, Url} = lists:keyfind(base_url, 1, Props),
    {ok, #ctx{base_url=Url}}.

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
    case riak_core_ring_manager:get_my_ring() of
        {ok, Ring} ->
            RingCtx = Ctx#ctx{ring=Ring},
            {true, add_node_links(RD, RingCtx), RingCtx};
        _Error ->
            {false, RD, Ctx}
    end.

-spec produce_json(wrq:reqdata(), context()) ->
         {binary(), wrq:reqdata(), context()}.
produce_json(RD, #ctx{ring=Ring}=Ctx) ->
    %% TODO: this is not really the list of nodes we'll want (nodes
    %% running, and connected, but not yet claiming partitions are
    %% interesting to the UI as well), but it's a good example of
    %% serving data over this interface
    Members = riak_core_ring:all_members(Ring),
    Partitions = [{Idx, [{owner, Owner}]} || {Idx, Owner} <- riak_core_ring:all_owners(Ring)],
    VNodes = all_vnodes(Members, dict:from_list(Partitions)),
    JSON = mochijson2:encode([{nodes, Members}, {partitions, lists:sort(dict:to_list(VNodes))}]),
    {JSON, RD, Ctx}.

%%% Internal

add_node_links(RD, #ctx{base_url=BaseUrl, ring=Ring}) ->
    Headers = [{"Link", node_link_header(BaseUrl, Node)}
               || Node <- riak_core_ring:all_members(Ring)],
    wrq:merge_resp_headers(Headers, RD).

node_link_header(BaseUrl, Node) ->
    io_lib:format("<~s~s>; rel=\"node\"",
                  [BaseUrl, mochiweb_util:quote_plus(Node)]).

all_vnodes([], VNodes) ->
    VNodes;
all_vnodes([Node|Rest], VNodes) ->
    VNodes2 = vnodes_for_node(Node, VNodes),
    all_vnodes(Rest, VNodes2).

%% gets the vnodes that are *actually* running on Node
vnodes_for_node(Node, VNodes) when is_atom(Node) ->
    Services = riak_core_node_watcher:services(Node),
    vnodes_for_service(Services, Node, VNodes).

vnodes_for_service([], _Node, VNodes) ->
    VNodes;
vnodes_for_service([Service | Rest ], Node, VNodes) ->
    Idxs = [{Pid, riak_core_vnode:get_mod_index(Pid)} || Pid <- service_pids(Service, Node)],
    NewDict = insert_into_dict(Idxs, Node, Service, VNodes),
    vnodes_for_service(Rest, Node, NewDict).

service_pids(Service, Node) when Node =:= node() ->
    riak_core_vnode_master:all_nodes(service_vnode_mod(Service));
service_pids(Service, Node) ->
    rpc:call(Node, riak_core_vnode_master, all_nodes, [service_vnode_mod(Service)], infinity).

%% Guess the name of the vnode from the service
service_vnode_mod(Service) when is_atom(Service) ->
    list_to_atom(atom_to_list(Service) ++ "_vnode").

insert_into_dict([], _, _, Dict) ->
    Dict;
insert_into_dict([{Pid, {_, Idx}} | Rest], Node, Service, Dict) ->
    Entry = dict:fetch(Idx, Dict),
    Owner = proplists:get_value(owner, Entry),
    VNodes = proplists:get_value(vnodes, Entry, []),
    AtHome = Node =:= Owner,
    HandingOff = riak_core_vnode:is_handing_off(Pid),
    NewVNodes = [ {Service, [{service, Service}, {location, Node}, {home, AtHome}]} | VNodes],
    insert_into_dict(Rest, Node, Service, dict:store(Idx, [{owner, Owner}, {vnodes, NewVNodes}], Dict)).

