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

-export([action_result/3]).

-include("riak_control.hrl").

%% all actions return the same format
action_result(Error={badrpc,_},Req,C) ->
    Body = mochijson2:encode({struct,[Error]}),
    {false, wrq:set_resp_body(Body, Req), C};
action_result(Error={error,_},Req,C) ->
    Body = mochijson2:encode({struct,[Error]}),
    {false, wrq:set_resp_body(Body, Req), C};
action_result(_,Req,C) ->
    Body = mochijson2:encode({struct,[{result,ok}]}),
    {true, wrq:set_resp_body(Body, Req), C}.
