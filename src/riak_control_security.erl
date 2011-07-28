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

%% @doc SSL and Authorization enforcement for administration URLs.
-module(riak_control_security).

-export([
         scheme_is_available/2,
         enforce_auth/2
        ]).

-include("riak_control.hrl").

%% @doc Intended to be called from a webmachine resource's
%% service_available function.  The return value is a valid resource
%% return value (`{Result, ReqData, Context}').
%%
%% If the appenv `riak_control:admin_https_only' is `true', this
%% function checks whether the request came in via HTTPS.  If it did
%% not, it generates a response to redirect to the same path via HTTPS
%% (via Location header and code 301).  If the request came in via
%% HTTPS, or the appenv is set to `false', the request is allowed to
%% proceed.
-spec scheme_is_available(wrq:reqdata(), term()) ->
         {true | {halt, 301}, wrq:reqdata(), Ctx::term()}.
scheme_is_available(RD, Ctx) ->
    case app_helper:get_env(riak_control, admin_https_only, true) of
        false ->
            %% make http-is-okay very specific, but https-is-required
            %% catch-all, for a small bit of config safety
            {true, RD, Ctx};
        _True ->
            case wrq:scheme(RD) of
                https ->
                    {true, RD, Ctx};
                _ ->
                    Path = wrq:raw_path(RD),
                    Loc = case app_helper:get_env(riak_core, https) of
                               undefined ->
                                   Host = string:join(lists:reverse(wrq:host_tokens(RD)), "."),
                                   ["https://", Host, Path];
                               [{Host, Port}|_] ->
                                   ["https://", Host, ":", integer_to_list(Port), wrq:raw_path(RD)]
                           end,
                    {{halt, 301},
                     wrq:set_resp_header("Location", Loc, RD),
                     Ctx}
            end
    end.

%% @doc Intended to be called from a webmachine resource's
%% is_authorized function.  The return value is a valid resource
%% return value (`{Result, ReqData, Context}').
%%
%% This function checks for valid authentication in the request.  If
%% the authentication is valid, `true' is returned.  If it is invalid,
%% the value for the response WWW-Authenticate header is returned.
%%
%% The correct credentials are controled by the appenv
%% `riak_control:admin_auth'.  Valid values include:
%%
%%    - `erlang' :: the username should be the same as the nodename
%%                  (e.g. `riak@127.0.0.1'), and the password should
%%                  be the same as the node's cookie (e.g. `riak')
enforce_auth(RD, Ctx) ->
    case wrq:get_req_header("authorization", RD) of
        "Basic "++Base64 ->
            enforce_basic_auth(RD, Ctx, Base64);
        _ ->
            {?ADMIN_AUTH_HEAD, RD, Ctx}
    end.

enforce_basic_auth(RD, Ctx, Base64) ->
    Str = base64:mime_decode_to_string(Base64),
    case string:tokens(Str, ":") of
        [User, Pass] ->
            enforce_user_pass(RD, Ctx, User, Pass);
        _ ->
            {?ADMIN_AUTH_HEAD, RD, Ctx}
    end.

enforce_user_pass(RD, Ctx, User, Pass) ->
    case valid_userpass(User, Pass) of
        true ->
            {true, RD, Ctx};
        false ->
            {?ADMIN_AUTH_HEAD, RD, Ctx}
    end.

valid_userpass(User, Pass) ->
    case app_helper:get_env(riak_control, admin_auth) of
        erlang ->
            User == atom_to_list(node()) andalso
                Pass == atom_to_list(erlang:get_cookie());
        _Unknown ->
            error_logger:warning_msg(
              "Unknown admin_auth type \"~p\"", [_Unknown]),
            false
    end.
