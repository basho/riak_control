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

-module(admin_gui).
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

%% mappings to the various content types supported for this resource
-define(CONTENT_TYPES,[{"text/css", to_resource},
                       {"text/html",to_resource},
                       {"text/plain",to_resource},
                       {"text/javascript",to_resource}
                      ]).

%% defines the webmachine routes this module handles
routes() ->
    [{admin_routes:admin_route([]),?MODULE,index},
     {admin_routes:admin_route(["fallbacks"]),?MODULE,fallback},
     {admin_routes:admin_route(["ui",'*']),?MODULE,undefined},
     {admin_routes:admin_route(["ui","index.html"]),?MODULE,oldindex}
    ].

%% entry-point for the resource from webmachine
init(Resource) -> {ok,Resource}.

%% redirect to the index page if no file given
moved_permanently(Req,oldindex) -> {{true,"/admin"},Req,index};
moved_permanently(Req,Ctx) -> {false,Req,Ctx}.

%% the index file isn't here
previously_existed(Req,oldindex) -> {true,Req,oldindex};
previously_existed(Req,Ctx) -> {false,Req,Ctx}.

%% a resource other than the index is here
resource_exists (Req,oldindex) -> {false,Req,oldindex};
resource_exists (Req,Ctx) -> {true,Req,Ctx}.

%% redirect to SSL port if using HTTP
service_available(RD,C) ->
    riak_control_security:scheme_is_available(RD,C).

%% validate username and password
is_authorized(RD,C) ->
    riak_control_security:enforce_auth(RD,C).

%% return the list of available content types for webmachine
content_types_provided(Req,Ctx=index) ->
    {[{"text/html", to_resource}],Req, Ctx};
content_types_provided(Req,Ctx) ->
    Index = file_path(Req),
    MimeType = webmachine_util:guess_mime(Index),
    {[{MimeType, to_resource}],Req, Ctx}.

%% return file path
file_path(Req) ->
    Path=wrq:path_tokens(Req),
    filename:join([riak_control:priv_dir(),"admin"] ++ Path).

%% loads a resource file from disk and returns it
get_file(Req) ->
    Index = file_path(Req),
    {ok,Source}=file:read_file(Index),
    Source.

%% true if the node is running riak_control
redirect_loc(Node) ->
    rpc:call(Node,riak_control_security,https_redirect_loc,[[]]).

%% don't use this node as a fallback, must be valid and reachable
find_fallbacks(Nodes) ->
    lists:foldl(fun (#member_info{node=Node,status=valid,reachable=true},Acc) ->
                        case Node == node() of
                            true -> Acc;
                            false ->
                                case redirect_loc(Node) of
                                    {ok,Loc} ->
                                        URI=lists:flatten(Loc),
                                        [list_to_binary(URI)|Acc];
                                    _ -> Acc
                                end
                        end;
                    (_,Acc) -> Acc
                end,
                [],
                Nodes).

%% find another node in the cluster that is running the GUI
to_resource(Req,Ctx=fallback) ->
    {ok,_V,Nodes}=riak_control_session:get_nodes(),
    NodeURIs=find_fallbacks(Nodes),
    {mochijson2:encode(NodeURIs),Req,Ctx};

%% respond to an index request
to_resource(Req,Ctx=index) ->
    Token = riak_control_security:csrf_token(Req, Ctx),
    {ok, Content} = index_dtl:render([{csrf_token, Token}]),
    {Content, wrq:set_resp_header("Set-Cookie", "csrf_token="++Token++"; secure; httponly", Req), Ctx};

%% respond to a file request
to_resource(Req,Ctx=oldindex) ->
    {"a",Req,Ctx};

%% respond to a file request
to_resource(Req,Ctx) ->
    {get_file(Req),Req,Ctx}.
