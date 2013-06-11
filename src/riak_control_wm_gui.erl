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
%% @doc Provides a resource for serving up the GUI skeleton.

-module(riak_control_wm_gui).

-export([routes/0,
         init/1,
         content_types_provided/2,
         resource_exists/2,
         previously_existed/2,
         moved_permanently/2,
         to_resource/2,
         is_authorized/2,
         service_available/2
        ]).

%% riak_control and webmachine dependencies
-include_lib("riak_control/include/riak_control.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, {
          base_url,
          base_path
         }).
-type context() :: #ctx{}.

%% mappings to the various content types supported for this resource
-define(CONTENT_TYPES,[{"text/css", to_resource},
                       {"text/html",to_resource},
                       {"text/plain",to_resource},
                       {"text/javascript",to_resource}
                      ]).

%% defines the webmachine routes this module handles
-spec routes() -> [webmachine_dispatcher:matchterm()].
routes() ->
    [{riak_control_routes:admin_route([]),?MODULE,index},
     {riak_control_routes:admin_route(["ui",'*']),?MODULE,undefined},
     {riak_control_routes:admin_route(["ui","index.html"]),?MODULE,oldindex}
    ].

%% entry-point for the resource from webmachine
-spec init(any()) -> {ok, any()}.
init(Resource) -> {ok,Resource}.

%% redirect to the index page if no file given
-spec moved_permanently(wrq:reqdata(), oldindex|context()) ->
                               {false|{true, string()}, wrq:reqdata(), index|context()}.
moved_permanently(Req,oldindex) -> {{true,"/admin"},Req,index};
moved_permanently(Req,Ctx) -> {false,Req,Ctx}.

%% the index file isn't here
-spec previously_existed(wrq:reqdata(), context()|oldindex) ->
                             {boolean(), wrq:reqdata(), context()|oldindex}.
previously_existed(Req,oldindex) -> {true,Req,oldindex};
previously_existed(Req,Ctx) -> {false,Req,Ctx}.

%% a resource other than the index is here
-spec resource_exists(wrq:reqdata(), context()|oldindex) ->
                             {boolean(), wrq:reqdata(), context()|oldindex}.
resource_exists (Req,oldindex) -> {false,Req,oldindex};
resource_exists (Req,Ctx) -> {true,Req,Ctx}.

%% redirect to SSL port if using HTTP
-spec service_available(wrq:reqdata(), context()) ->
                               {boolean() | {halt, non_neg_integer()}, wrq:reqdata(), context()}.
service_available(RD,C) ->
    riak_control_security:scheme_is_available(RD,C).

%% validate username and password
-spec is_authorized(wrq:reqdata(), context()) ->
                           {true | string(), wrq:reqdata(), context()}.
is_authorized(RD,C) ->
    riak_control_security:enforce_auth(RD,C).

%% return the list of available content types for webmachine
-spec content_types_provided(wrq:reqdata(), context()) ->
         {[{ContentType::string(), HandlerFunction::atom()}],
          wrq:reqdata(), context()}.
content_types_provided(Req,Ctx=index) ->
    {[{"text/html", to_resource}],Req, Ctx};
content_types_provided(Req,Ctx) ->
    Index = file_path(Req),
    MimeType = webmachine_util:guess_mime(Index),
    {[{MimeType, to_resource}],Req, Ctx}.

%% return file path
-spec file_path(wrq:reqdata()) -> string().
file_path(Req) ->
    Path=wrq:path_tokens(Req),
    filename:join([riak_control:priv_dir(),"admin"] ++ Path).

%% loads a resource file from disk and returns it
-spec get_file(wrq:reqdata()) ->
                      binary().
get_file(Req) ->
    Index = file_path(Req),
    {ok,Source}=file:read_file(Index),
    Source.

%% respond to an index request
-spec to_resource(wrq:reqdata(), context()) ->
                         {binary(), wrq:reqdata(), context()}.
to_resource(Req,Ctx=index) ->
    Token = riak_control_security:csrf_token(Req, Ctx),
    {ok, Content} = index_dtl:render([{csrf_token, Token}]),
    {Content, wrq:set_resp_header("Set-Cookie", "csrf_token="++Token++"; httponly", Req), Ctx};

%% respond to a file request
to_resource(Req,Ctx) ->
    {get_file(Req),Req,Ctx}.
