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

%% @doc Primarily a file server to throw the static bits of the Admin
%% ui at a browser.
%%
%%  The resource exposes itself at `/admin/ui'.
-module(admin_ui_resource).

-export([
         routes/0,
         init/1,
         service_available/2,
         is_authorized/2,
         resource_exists/2,
         content_types_provided/2,
         produce_content/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include("riak_control.hrl").

-record(ctx, {
          base_url,
          base_path
         }).

-type context() :: #ctx{}.

%%% riak_control_sup API

%% @doc Get the webmachine dispatcher config for this resource.
-spec routes() -> [webmachine_dispatcher:matchterm()].
routes() ->
    [{?ADMIN_BASE_ROUTE ++ ["ui", '*'],
      ?MODULE,
      [{base_url, ?ADMIN_BASE_PATH ++ "ui/"},
       {base_path, "admin_ui"}]}].

%%% Webmachine API

-spec init(list()) -> {ok, context()}.
init(Props) ->
    {base_url, Url} = lists:keyfind(base_url, 1, Props),
    {base_path, Path} = lists:keyfind(base_path, 1, Props),
    {ok, #ctx{base_url=Url,
              base_path=Path}}.

-spec service_available(wrq:reqdata(), context()) ->
         {boolean() | {halt, 301}, wrq:reqdata(), context()}.
service_available(RD, #ctx{base_url=Base}=Ctx) ->
    case is_toplevel(RD) of
        almost ->
            %% force trailing slash
            {{halt, 301},
             wrq:set_resp_header("Location", Base, RD),
             Ctx};
        _ ->
            riak_control_security:scheme_is_available(RD, Ctx)
    end.

-spec is_authorized(wrq:reqdata(), context()) ->
         {true | string(), wrq:reqdata(), context()}.
is_authorized(RD, Ctx) ->
    riak_control_security:enforce_auth(RD, Ctx).

-spec resource_exists(wrq:reqdata(), context()) ->
         {boolean(), wrq:reqdata(), context()}.
resource_exists(RD, Ctx) ->
    {filelib:is_regular(file_path(RD, Ctx)), RD, Ctx}.

-spec content_types_provided(wrq:reqdata(), context()) ->
         {[{ContentType::string(), HandlerFunction::atom()}],
          wrq:reqdata(), context()}.
content_types_provided(RD, Ctx) ->
    {[{webmachine_util:guess_mime(file_path(RD, Ctx)), produce_content}],
     RD, Ctx}.

-spec produce_content(wrq:reqdata(), context()) ->
         {binary(), wrq:reqdata(), context()}.
produce_content(RD, Ctx) ->
    {ok, Content} = file:read_file(file_path(RD, Ctx)),
    {Content, RD, Ctx}.

%%% Internal

-spec is_toplevel(wrq:reqdata()) -> boolean() | almost.
is_toplevel(RD) ->
    case wrq:disp_path(RD) of
        [] ->
            %% have to force the '/' after 'rekon' in the url,
            %% or relative paths to other resources will be wrong
            case lists:reverse(wrq:raw_path(RD)) of
                [$/|_] -> true;
                _      -> almost
            end;
        "index.html" -> true;
        _            -> false
    end.

%% @doc Get the absolute path to the file indicated by the requests's URL.
-spec file_path(wrq:reqdata(), context()) -> string().
file_path(RD, #ctx{base_path=BasePath}) ->
    SubPath = case is_toplevel(RD) of
                  almost -> ["index.html"];
                  true -> ["index.html"];
                  false ->
                      %% strip .. to prevent unauthorized access
                      [ P || P <- filename:split(wrq:disp_path(RD)),
                             P /= ".." ]
              end,
    Priv = riak_control:priv_dir(),
    filename:join([Priv, BasePath, SubPath]).
