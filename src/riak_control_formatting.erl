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

%% @doc JSON formatting of result
-module(riak_control_formatting).

-export([cluster_action_result/3, node_action_result/3]).

-include("riak_control.hrl").

%% TODO: combine these two.

%% all actions return the same format
cluster_action_result(Error={error,_},Req,C) ->
    {{error,mochijson2:encode({struct,[Error]})},Req,C};
cluster_action_result(Error={badrpc,_},Req,C) ->
    {{error,mochijson2:encode({struct,[Error]})},Req,C};
cluster_action_result(_,Req,C) ->
    {mochijson2:encode({struct,[{result,ok}]}),Req,C}.

%% all actions return the same format
node_action_result({error,Reason},Req,C) ->
    {{error,Reason},Req,C};
node_action_result(_,Req,C) ->
    {mochijson2:encode({struct,[{result,ok}]}),Req,C}.
