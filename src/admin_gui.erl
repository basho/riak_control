-module(admin_gui).
-export([routes/0,
         init/1,
         content_types_provided/2,
         moved_permanently/2,
         previously_existed/2,
         resource_exists/2,
         get_file/2,
         is_authorized/2,
         service_available/2
        ]).

%% webmachine dependencies
-include_lib("webmachine/include/webmachine.hrl").

%% mappings to the various content types supported for this resource
-define(CONTENT_TYPES,[{"text/html",get_file},
                       {"text/plain",get_file},
                       {"text/javascript",get_file}
                      ]).

%% defines the webmachine routes this module handles
routes () ->
    [{admin_routes:admin_route([]),?MODULE,index},
     {admin_routes:admin_route(["ui",file]),?MODULE,undefined}].

%% entry-point for the resource from webmachine
init (Resource) -> 
    {ok,Resource}.

%% redirect to the index page if no file given
moved_permanently (Req,index) ->
    {{true,"/admin/ui/index.html"},Req,undefined};
moved_permanently (Req,Ctx) ->
    {false,Req,Ctx}.

%% the index file isn't here
previously_existed (Req,index) -> {true,Req,index};
previously_existed (Req,Ctx) -> {false,Req,Ctx}.

%% a resource other than the index is here
resource_exists (Req,index) -> {false,Req,index};
resource_exists (Req,Ctx) -> {true,Req,Ctx}.

%% redirect to SSL port if using HTTP
service_available (RD,C) ->
    riak_control_security:scheme_is_available(RD,C).

%% validate username and password
is_authorized (RD,C) ->
    riak_control_security:enforce_auth(RD,C).

%% return the list of available content types for webmachine
content_types_provided (Req,Ctx) ->
    {?CONTENT_TYPES,Req,Ctx}.

%% valid | invalid | joining | leaving | exiting
get_file (Req,Ctx) ->
    File=dict:fetch(file,wrq:path_info(Req)),
    Index=filename:join([riak_control:priv_dir(),"admin",File]),
    {ok,Source}=file:read_file(Index),
    {Source,Req,Ctx}.
