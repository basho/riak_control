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
%%
%% @doc Returns a list of all partitions, how many primary replicas
%%      are available, what the current n_val and quorum
%%      configuration is, as well as the unavailable nodes for
%%      each partition.

-module(riak_control_wm_partitions).

-export([routes/0,
         init/1,
         content_types_provided/2,
         to_json/2,
         is_authorized/2,
         service_available/2,
         forbidden/2]).

-include_lib("riak_control/include/riak_control.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-define(CONTENT_TYPES, [{"application/json",to_json}]).

-define(VNODE_TYPES, [riak_kv,riak_pipe,riak_search]).

-record(context, {partitions, n_vals}).
-type context() :: #context{}.

%% @doc Route handling.
-spec routes() -> [webmachine_dispatcher:matchterm()].
routes() ->
    [{riak_control_routes:partitions_route(), ?MODULE, []}].

%% @doc Get partition list at the start of the request.
-spec init(list()) -> {ok, context()}.
init([]) ->
    {ok, _, Partitions} = riak_control_session:get_partitions(),
    {ok, _, NVals} = riak_control_session:get_n_vals(),
    {ok, #context{partitions=Partitions, n_vals=NVals}}.

%% @doc Validate origin.
-spec forbidden(wrq:reqdata(), context()) ->
    {boolean(), wrq:reqdata(), context()}.
forbidden(ReqData, Context) ->
    {riak_control_security:is_null_origin(ReqData), ReqData, Context}.

%% @doc Determine if it's available.
-spec service_available(wrq:reqdata(), context()) ->
    {boolean() | {halt, non_neg_integer()}, wrq:reqdata(), context()}.
service_available(ReqData, Context) ->
    riak_control_security:scheme_is_available(ReqData, Context).

%% @doc Handle authorization.
-spec is_authorized(wrq:reqdata(), context()) ->
    {true | string(), wrq:reqdata(), context()}.
is_authorized(ReqData, Context) ->
    riak_control_security:enforce_auth(ReqData, Context).

%% @doc Return available content types.
-spec content_types_provided(wrq:reqdata(), context()) ->
    {[{ContentType::string(), HandlerFunction::atom()}], wrq:reqdata(), context()}.
content_types_provided(ReqData, Context) ->
    {?CONTENT_TYPES, ReqData, Context}.

%% @doc Return a list of partitions.
-spec to_json(wrq:reqdata(),context()) ->
    {iolist(), wrq:reqdata(), context()}.
to_json(ReqData, Context=#context{partitions=Partitions, n_vals=NVals}) ->
    Encoded = mochijson2:encode({struct,[{partitions, Partitions}, {n_vals, NVals}]}),
    {Encoded, ReqData, Context}.
