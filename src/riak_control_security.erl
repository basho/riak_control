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
%%
%% @doc SSL and Authorization enforcement for administration URLs.

-module(riak_control_security).

-export([scheme_is_available/2,
         enforce_auth/2,
         csrf_token/2,
         is_valid_csrf_token/2,
         is_null_origin/1,
         is_protected/2]).

-include("riak_control.hrl").

-type context() :: term() | undefined.
-type csrf_token() :: list() | undefined.

%% @doc Enforce use of HTTPS only when a valid auth scheme is enabled.
-spec scheme_is_available(wrq:reqdata(), context()) ->
    {boolean(), wrq:reqdata(), context()}.
scheme_is_available(RD, Ctx) ->
    case app_helper:get_env(riak_control, force_ssl, undefined) of
        undefined ->
            %% Handle upgrade, where we want to preserve existing
            %% behavior.
            case app_helper:get_env(riak_control, auth, none) of
                none ->
                    {true, RD, Ctx};
                _ ->
                    redirect_if_not_ssl(RD, Ctx)
            end;
        true ->
            redirect_if_not_ssl(RD, Ctx);
        _ ->
            {true, RD, Ctx}
    end.

%% @doc Redirect if request is not SSL.
-spec redirect_if_not_ssl(wrq:reqdata(), context()) ->
    {boolean(), wrq:reqdata(), context()}.
redirect_if_not_ssl(ReqData, Context) ->
    case wrq:scheme(ReqData) of
        https ->
            {true, ReqData, Context};
        _ ->
            https_redirect(ReqData, Context)
    end.

%% @doc Perform http redirect to ssl.
-spec https_redirect(wrq:reqdata(), context()) ->
    {{halt, 303}, wrq:reqdata(), context()}.
https_redirect(RD,Ctx) ->
    Path = wrq:raw_path(RD),
    Host = wrq:sock(RD),
    Bindings = app_helper:get_env(riak_core, https, []),
    Location = case lists:keyfind(Host, 1, Bindings) of
        {_, Port} ->
            ["https://", Host, ":", integer_to_list(Port), Path];
        _ ->
            ["https://", Host, Path]
    end,
    {{halt,303}, wrq:set_resp_header("Location", Location, RD), Ctx}.

%% @doc Intended to be called from a webmachine resource's
%%      is_authorized function.  The return value is a valid resource
%%      return value (`{Result, ReqData, Context}').
%%
%% This function checks for valid authentication in the request.  If
%% the authentication is valid, `true' is returned.  If it is invalid,
%% the value for the response WWW-Authenticate header is returned.
%%
%% The correct credentials are controled by the appenv
%% `riak_control:auth'.  Valid values include:
%%
%%    - `userlist' :: `riak_control:userlist' will contain a list of
%%                    {"user","pass"} pairs that are used.
%%
%%    - `none'     :: No authentication.
%%
-spec enforce_auth(wrq:reqdata(), context()) ->
    {boolean(), wrq:reqdata(), context()}.
enforce_auth(RD, Ctx) ->
    case app_helper:get_env(riak_control,auth,none) of
        none ->
            {true, RD, Ctx};
        Auth ->
            case wrq:get_req_header("authorization", RD) of
                "Basic "++Base64 ->
                    enforce_basic_auth(RD, Ctx, Base64, Auth);
                _ ->
                    {?ADMIN_AUTH_HEAD, RD, Ctx}
            end
    end.

%% @doc Enforce basic auth.
-spec enforce_basic_auth(wrq:reqdata(), context(), term(), atom()) ->
    {boolean(), wrq:reqdata(), context()}.
enforce_basic_auth(RD, Ctx, Base64, Auth) ->
    Str = base64:mime_decode_to_string(Base64),
    case string:tokens(Str, ":") of
        [User, Pass] ->
            enforce_user_pass(RD, Ctx, User, Pass, Auth);
        _ ->
            {?ADMIN_AUTH_HEAD, RD, Ctx}
    end.

%% @doc Enforce user and password match.
-spec enforce_user_pass(wrq:reqdata(), context(), nonempty_string(),
                        nonempty_string(), atom()) ->
    {boolean(), wrq:reqdata(), context()}.
enforce_user_pass(RD, Ctx, User, Pass, Auth) ->
    case valid_userpass(User, Pass, Auth) of
        true ->
            {true, RD, Ctx};
        _ ->
            {?ADMIN_AUTH_HEAD, RD, Ctx}
    end.

%% @doc Validate username and password given a particular auth style.
-spec valid_userpass(nonempty_string(), nonempty_string(),
                     atom()) -> boolean().
valid_userpass(_User, _Pass, none) ->
    true;
valid_userpass(User, Pass, userlist) ->
    Users = app_helper:get_env(riak_control, userlist, []),
    proplists:get_value(User, Users) == Pass;
valid_userpass(_User, _Pass, _Auth) ->
    error_logger:warning_msg("Unknown auth type '~p'", [_Auth]),
    false.

%% @doc Generate a new CSRF token.
-spec csrf_token(wrq:reqdata(), context()) -> csrf_token().
csrf_token(ReqData, Context) ->
    case get_csrf_token(ReqData, Context) of
        undefined ->
            binary_to_list(riak_core_base64url:encode(crypto:rand_bytes(256)));
        Token ->
            Token
    end.

%% @doc Get the CSRF token from the cookie.
-spec get_csrf_token(wrq:reqdata(), context()) -> csrf_token().
get_csrf_token(ReqData, _Context) ->
    wrq:get_cookie_value("csrf_token", ReqData).

%% @doc Ensure this request contains a valid csrf protection token.
-spec is_valid_csrf_token(wrq:reqdata(), context()) -> boolean().
is_valid_csrf_token(ReqData, Context) ->
    HeaderToken = wrq:get_req_header("X-CSRF-Token", ReqData),
    CookieToken = get_csrf_token(ReqData, Context),
    HeaderToken /= undefined andalso HeaderToken == CookieToken.

%% @doc Is this a protected method?
-spec is_protected_method(wrq:reqdata()) -> boolean().
is_protected_method(ReqData) ->
    Method = wrq:method(ReqData),
    Method == 'POST' orelse Method == 'PUT'.

%% @doc Is this a protected?
-spec is_protected(wrq:reqdata(), context()) -> boolean().
is_protected(ReqData, Context) ->
    (is_null_origin(ReqData) or
     not is_valid_csrf_token(ReqData, Context)) and
    is_protected_method(ReqData).

%% @doc Check if the Origin header is "null". This is useful to look for
%%      attempts at CSRF, but is not a complete answer to the problem.
-spec is_null_origin(wrq:reqdata()) -> boolean().
is_null_origin(ReqData) ->
    case wrq:get_req_header("Origin", ReqData) of
        "null" ->
            true;
        _ ->
            false
    end.
