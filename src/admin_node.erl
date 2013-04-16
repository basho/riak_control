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

-module(admin_node).

-author('Christopher Meiklejohn <cmeiklejohn@basho.com>').

-export([routes/0,
         init/1,
         forbidden/2,
         process_post/2,
         is_authorized/2,
         allowed_methods/2,
         service_available/2]).

-include_lib("riak_control/include/riak_control.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%% @doc Return routes this resource should respond to.
-spec routes() -> [webmachine_dispatcher:matchterm()].
routes() ->
    [{admin_routes:nodes_route(), ?MODULE, []}].

%% @doc Initialize resource.
-spec init([]) -> {ok, undefined}.
init([]) ->
    {ok, undefined}.

%% @doc Allowed methods.
-spec allowed_methods(wrq:reqdata(), undefined) ->
    {list(atom()), wrq:reqdata(), undefined}.
allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

%% @doc Prevent requests coming from an invalid origin.
-spec forbidden(wrq:reqdata(), undefined) ->
    {boolean(), wrq:reqdata(), undefined}.
forbidden(ReqData, Context) ->
    {riak_control_security:is_protected(ReqData, Context), ReqData, Context}.

%% @doc Handle SSL requests.
-spec service_available(wrq:reqdata(), undefined) ->
    {boolean(), wrq:reqdata(), undefined}.
service_available(ReqData, Context) ->
    riak_control_security:scheme_is_available(ReqData, Context).

%% @doc Ensure user has access.
-spec is_authorized(wrq:reqdata(), undefined) ->
    {boolean(), wrq:reqdata(), undefined}.
is_authorized(ReqData, Context) ->
    riak_control_security:enforce_auth(ReqData, Context).

%% @doc Handle node modifications.
-spec process_post(wrq:reqdata(), undefined) ->
    {boolean(), wrq:reqdata(), undefined}.
process_post(ReqData, Context) ->
    {true, ReqData, Context}.

%% @doc Mark a node as down in the cluster.
-spec mark_node_as_down(node()) ->
    ok | {error, not_member} | {error, only_member}.
mark_node_as_down(Node) when is_atom(Node) ->
    riak_core:down(Node).

%% @doc Stop a node in the cluster.
-spec stop_node(node()) -> ok | error.
stop_node(Node) when is_atom(Node) ->
    case rpc:call(Node, riak_core, stop, []) of
        {badrpc, Error} ->
            {error, Error};
        Result ->
            Result
    end.
