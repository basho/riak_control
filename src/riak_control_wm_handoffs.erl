%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
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
%% @doc
%%
%% Returns all handoffs.
%%
%% GET /handoffs
%%
%% @end

-module(riak_control_wm_handoffs).

-export([routes/0,
         init/1,
         to_json/2,
         forbidden/2,
         is_authorized/2,
         allowed_methods/2,
         service_available/2,
         content_types_provided/2]).

-export([retrieve_transfers/0]).

-include_lib("riak_control/include/riak_control.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%% @doc Return routes this resource should respond to.
-spec routes() -> [webmachine_dispatcher:matchterm()].
routes() ->
    [{riak_control_routes:handoffs_route(), ?MODULE, []}].

%% @doc Initialize resource.
-spec init([]) -> {ok, undefined}.
init([]) ->
    {ok, undefined}.

%% @doc Allowed methods.
-spec allowed_methods(wrq:reqdata(), undefined) ->
    {list(atom()), wrq:reqdata(), undefined}.
allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

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

%% @doc Return content-types which are provided.
-spec content_types_provided(wrq:reqdata(), undefined) ->
    {list(), wrq:reqdata(), undefined}.
content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

%% @doc Return the list of handoffs.
-spec to_json(wrq:reqdata(), undefined) -> {binary(), wrq:reqdata(), undefined}.
to_json(ReqData, Context) ->
    Handoffs = case retrieve_transfers() of
        [] ->
            [];
        Transfers ->
            orddict:fold(fun format_transfers/3, [], Transfers)
    end,
    Encoded = mochijson2:encode({struct, [{handoffs, Handoffs}]}),
    {Encoded, ReqData, Context}.

%% @doc Format transfers.
-spec format_transfers({term(), term()}, list(), list()) -> list().
format_transfers({Owner, NextOwner}, Transfers, Handoffs) ->
    Handoffs ++ lists:flatmap(fun({Index, Waiting, _Complete, Status}) ->
                case Status of
                    complete ->
                        [];
                    awaiting ->
                        [{struct,[{"index", Index},
                                 {"owner", Owner},
                                 {"next_owner", NextOwner},
                                  {"waiting_for", Waiting}]}]
                end
        end, Transfers).

%% @doc Retrieve transfers from the ring.
-spec retrieve_transfers() -> [{term(), term()}].
retrieve_transfers() ->
    case riak_core_gossip:legacy_gossip() of
        true ->
            [];
        false ->
            {_Claimant, _RingReady, _Down, _MarkedDown, Changes} =
                riak_core_status:ring_status(),
            Changes
    end.
