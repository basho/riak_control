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
    Nodes = lists:filter(fun(X) ->
                                 case rpc:call(X, erlang, whereis, [riak_core_sup]) of
                                     undefined ->
                                         false;
                                     _ ->
                                         true
                                 end
                         end,
                         [node() | nodes()]),
    HandOffs = handoffs_by_partition(Nodes),
    VNodeMods = lists:sort(riak_core:vnode_modules()),
    VNodes = all_vnodes(riak_core_ring:all_owners(Ring), Nodes, VNodeMods, HandOffs, []),
    JSON = mochijson2:encode([{nodes, Nodes}, {partitions, lists:sort(VNodes)}]),
    {JSON, RD, Ctx}.

%%% Internal

%%% HANDOFF
handoffs_by_partition(Nodes) ->
    HandOffs = get_all_handoffs(Nodes),
    handoffs_by_partition(HandOffs, dict:new()).

%% merge the handoffs from all nodes into a single dict
%% of {Mod, Idx, Location} -> handingoff | receiving_handoff
handoffs_by_partition([], HandOffs) ->
    HandOffs;
handoffs_by_partition([{Host, Hoffs} | Rest], HandOffs) ->
    HandOffs2 = lists:foldl(fun({{VNodeMod, Idx}, Targets}, Dict) ->
                                    append_handoff(Idx, VNodeMod, Host, Targets, Dict);
                               ([], Dict) -> Dict end, HandOffs, Hoffs),
    handoffs_by_partition(Rest, HandOffs2).

%% Add the handoff data to the dict of handoffs
append_handoff(Idx, VNodeMod, Host, [Target], HandOffs) ->
    D1 = dict:append({Idx, VNodeMod, Host}, handingoff, HandOffs),
    dict:append({Idx, VNodeMod, Target}, receiving_handoff, D1).

%% interogate handoff manager on each Nodes
get_all_handoffs(Nodes) ->
    multicall(Nodes, riak_core_handoff_manager, all_handoffs, []).

%%% VNODES
%% Evil inefficient, find a better way
all_vnodes([], _Nodes, _VnodeMods, _HandOffs,  Partitions) ->
    lists:reverse(Partitions);
all_vnodes([{Idx, Owner}|Rest], Nodes, VNodeMods, HandOffs, Partitions) ->
    VNodes = vnode_data(Idx, Owner, Nodes, VNodeMods, HandOffs, []),
    all_vnodes(Rest, Nodes, VNodeMods, HandOffs, [{Idx, [{owner, Owner}, {vnodes, VNodes}]} | Partitions]).

vnode_data(_Idx, _Owner, _Nodes, [], _HandOffs, Acc) ->
    lists:flatten(lists:reverse(Acc));
vnode_data(Idx, Owner, Nodes, [{_Service, VNodeMod}|Rest], HandOffs, Acc) ->
    Res = multicall(Nodes, riak_core_vnode_master, is_vnode_pid, [Idx, VNodeMod]),
    ServiceData = lists:foldl(fun(Elem, L) ->
                                      is_home_active(Idx, Owner, Elem, VNodeMod, HandOffs, L) end, [], Res),
    vnode_data(Idx, Owner, Nodes, Rest, HandOffs, [ServiceData | Acc]).

%% Given the index, owner, location, vnodemod and dict of active handoffs
%% is the vnode running at home or away? is it giving or receiving handoff?
is_home_active(Idx, Owner, {Node, {true, _Pid}}, VNodeMod, HandOffs, Acc) ->
    case dict:find({Idx, VNodeMod, Node}, HandOffs) of
        error ->
            [{struct, [{mod, VNodeMod}, {location, Node}, {status, home_or_away(Node, Owner)}]} | Acc];
        {ok, Status} ->
            [{struct, [{mod, VNodeMod}, {location, Node}, {status, Status}]} | Acc]
    end;
is_home_active(_, _, _, _, _, Acc) ->
    Acc.

%% Partition owned by 1, running on 2, is it home or away?
home_or_away(_Node, _Node) ->
    home;
home_or_away(_, _) ->
    away.

%% call MFA on Nodes, only return up responses annotated with responding node
multicall(Nodes, Mod, Fun, Args) ->
    {Results, Down} = rpc:multicall(Nodes, Mod, Fun, Args, infinity),
    Up = Nodes -- Down,
    lists:zip(Up, Results).

add_node_links(RD, #ctx{base_url=BaseUrl, ring=Ring}) ->
    Headers = [{"Link", node_link_header(BaseUrl, Node)}
               || Node <- riak_core_ring:all_members(Ring)],
    wrq:merge_resp_headers(Headers, RD).

node_link_header(BaseUrl, Node) ->
    io_lib:format("<~s~s>; rel=\"node\"",
                  [BaseUrl, mochiweb_util:quote_plus(Node)]).

