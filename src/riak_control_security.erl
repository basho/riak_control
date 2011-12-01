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

-export([scheme_is_available/2,
         enforce_auth/2,
         https_redirect_loc/1
        ]).

-include("riak_control.hrl").

%% if riak_control has an auth scheme selected, then we enforce
%% use of HTTPS and will redirect the user to the HTTPS version
%% of the page requested
scheme_is_available(RD, Ctx) ->
    case app_helper:get_env(riak_control, auth, none) of
        none ->
            {true, RD, Ctx};
        _ ->
            case wrq:scheme(RD) of
                https ->
                    {true, RD, Ctx};
                _ ->
                    https_redirect(RD,Ctx)
            end
    end.

%% get the https location to redirect to (callable w/o a request)
https_redirect_loc (Path) ->
    case app_helper:get_env(riak_control, enabled, false) of
        true ->
            case app_helper:get_env(riak_core, https) of
                [{Host,Port}|_] ->
                    {ok,["https://",Host,":",integer_to_list(Port),Path]};
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

%% set the redirect header and where to go with it
https_redirect (RD,Ctx) ->
    Path=wrq:raw_path(RD),
    Loc=case https_redirect_loc(Path) of
            {ok,Dest} ->
                Dest;
            _ ->
                Host=string:join(lists:reverse(wrq:host_tokens(RD)),"."),
                ["https://",Host,Path]
        end,
    {{halt,303},wrq:set_resp_header("Location",Loc,RD),Ctx}.

%% @doc Intended to be called from a webmachine resource's
%% is_authorized function.  The return value is a valid resource
%% return value (`{Result, ReqData, Context}').
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

%% validate the username and password
valid_userpass(User, Pass) ->
    Auth=app_helper:get_env(riak_control, auth),
    valid_userpass(User, Pass, Auth).

%% validate the username and password with the given auth style
valid_userpass(_User, _Pass, none) ->
    true;
valid_userpass(User, Pass, userlist) ->
    Users=app_helper:get_env(riak_control, userlist, []),
    proplists:get_value(User, Users) == Pass;
valid_userpass(_User, _Pass, _Auth) ->
    error_logger:warning_msg("Unknown auth type '~p'", [_Auth]),
    false.

