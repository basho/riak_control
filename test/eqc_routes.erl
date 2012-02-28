-module(eqc_routes).
-compile(export_all).

-ifdef(EQC).

%% EQC headers
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

%% command generators
-define(CLUSTER_LIST,{call,?MODULE,cluster_list,[]}).
-define(PARTITION_LIST,{call,?MODULE,partition_list,[]}).
-define(HTTP_CLUSTER_LIST(U),{call,?MODULE,http_cluster_list,[U]}).
-define(HTTP_PARTITION_LIST(U,F),{call,?MODULE,http_partition_list,[U,F]}).


%% ---------------------------------------------------------------------------
%% statem state

-record(state,{auth,ring_size}).


%% ---------------------------------------------------------------------------
%% properties

prop_routes () ->
    ?FORALL(Auth,g_auth(),
            collect(Auth,
            begin
                setup(Auth),

                %% start application dependencies
                riak_core_util:start_app_deps(riak_core),
                riak_core_util:start_app_deps(riak_control),

                %% can now start riak control
                application:start(riak_control),

                %% ensure riak_control is up and running
                riak_core:wait_for_application(riak_control),
                {ok,Size}=application:get_env(riak_core,ring_creation_size),

                %% now run the test with these auth settings
                ?ALWAYS(10,
                        ?FORALL(Cmds,commands(?MODULE,#state{auth=Auth,
                                                             ring_size=Size
                                                            }),
                                begin
                                    {H,S,Res}=run_commands(?MODULE,Cmds),

                                    %% results
                                    ?WHENFAIL(
                                       io:format("H: ~p\nS: ~p\nR: ~p\n",
                                                 [H,S,Res]),
                                       Res==ok)
                                end))
            end)).


%% ---------------------------------------------------------------------------
%% statem behavior

initial_state () ->
    #state{}.

command (_State) ->
    oneof([?CLUSTER_LIST,
           ?PARTITION_LIST,
           ?LET(User,g_userpass(),
                oneof([?HTTP_CLUSTER_LIST(User),
                       ?LET(Filter,g_qs(),
                            ?HTTP_PARTITION_LIST(User,Filter))
                      ]))
          ]).

precondition (_State,_Gen) ->
    true.

postcondition (State,{call,_,http_cluster_list,[User]},Res) ->
    http_validate(State,Res,User) andalso cluster_validate(State,Res);
postcondition (State,{call,_,http_partition_list,[User,Filter]},Res) ->
    http_validate(State,Res,User) andalso filter_validate(State,Res,Filter);
postcondition (_State,_Gen,_Res) ->
    true.

next_state (State,_V,_Gen) ->
    State.


%% ---------------------------------------------------------------------------
%% negative testing common post-conditions

http_validate (#state{auth=userlist},{Code,_,_},{user,pass}) ->
    Code < 300;
http_validate (#state{auth=userlist},{Code,_,_},_) ->
    Code >= 300;
http_validate (#state{auth=none},{Code,_,_},_) ->
    Code < 300.


cluster_validate (_State,{Code,"",_Url}) ->
    not (Code < 300);
cluster_validate (_State,{_Code,Body,_Url}) ->
    [{struct,Node}]=mochijson2:decode(Body),
    {_,Name}=lists:keyfind(<<"name">>,1,Node),
    {_,Me}=lists:keyfind(<<"me">>,1,Node),
    {_,Reachable}=lists:keyfind(<<"reachable">>,1,Node),
    binary_to_atom(Name,utf8) == node() andalso Me andalso Reachable.


filter_validate (_State,{Code,"",_Url},_Filter) ->
    not (Code < 300);
filter_validate (#state{ring_size=Size},{_Code,Body,_Url},Filter) ->
    {struct,Json}=mochijson2:decode(Body),
    {_,Pages}=lists:keyfind(<<"pages">>,1,Json),
    {_,Page}=lists:keyfind(<<"page">>,1,Json),
    {_,Contents}=lists:keyfind(<<"contents">>,1,Json),
    filter_validate_n(Size,Pages,Page,Contents,Filter) andalso
        %% TODO: more validation here
        true.


filter_validate_n (Size,Pages,_Page,Contents,Filter) ->
    ContentSize=erlang:length(Contents),
    N=case lists:keyfind(n,1,Filter) of
          false -> Size div Pages;
          {_,X} -> X
      end,
    %% less than is okay for now, we might have returned the last page
    ContentSize =< max(N,16).


%% ---------------------------------------------------------------------------
%% eqc generators

%% valid authorization modes for app.config
g_auth () ->
    oneof([userlist,none]).

%% username and password pairs (none = don't pass user/pass)
g_userpass () ->
    oneof([{user,pass},
           {user,bad_password},
           {bad_user,pass},
           {bad_user,bad_password},
           none % not passing auth data
          ]).

%% query string generator
g_qs () ->
    ?LET(Filter,g_filter(),
         ?LET(Page,g_page(),
              Filter ++ Page)).

%% valid and invalid query string filters for ring page
g_filter () ->
    oneof([[{filter,node}],
           [{filter,node},{q,g_node()}],
           [{q,g_node()}],
           [{filter,handoffs}],
           [{filter,fallbacks}],
           []
          ]).

%% pagination data for ring page
g_page () ->
    oneof([[{n,int()},{p,int()}],
           [{n,int()}],
           [{p,int()}],
           []
          ]).

%% this node or a dummy node
g_node () ->
    oneof([node(),'dummy@nohost']).


%% ---------------------------------------------------------------------------
%% private

f (Fmt,Args) ->
    lists:flatten(io_lib:format(Fmt,Args)).

set (K,V) ->
    application:set_env(riak_core,K,V).

setup (Auth) ->
    application:load(riak_core),
    application:load(riak_control),

    %% hide sasl output
    application:set_env(sasl,sasl_error_logger,false),

    %% local the pem files in the test folder
    PrivDir=code:priv_dir(riak_control),
    TestDir=filename:join([PrivDir,"..","test"]),
    CertFile=filename:join([TestDir,"cert.pem"]),
    KeyFile=filename:join([TestDir,"key.pem"]),

    %% set env values for riak core
    set(http, [{"127.0.0.1",18098}]),
    set(https, [{"127.0.0.1",18069}]),
    set(ssl, [{certfile, CertFile},
              {keyfile, KeyFile}
             ]),

    %% finally, enable riak control
    application:set_env(riak_control,userlist,[{"user","pass"}]),
    application:set_env(riak_control,enabled,true),
    application:set_env(riak_control,admin,true),
    application:set_env(riak_control,auth,Auth).

cluster_list () ->
    {ok,_V,Nodes}=riak_control_session:get_nodes(),
    Nodes.

partition_list () ->
    {ok,_V,Ring}=riak_control_session:get_ring(),
    Ring.

http_cluster_list (User) ->
    http_request(User,"/admin/cluster/list",[]).

http_partition_list (User,Filter) ->
    http_request(User,"/admin/ring/partitions",Filter).

http_request (User,Route,Query) ->
    Url=request_url(User,Route,Query),
    case httpc:request(get,{Url,[]},[],[]) of
        {ok,{{_HTTP,Code,_OK},_Headers,Body}} -> {Code,Body,Url};
        {ok,{{_HTTP,Code,_OK},Body}} -> {Code,Body,Url}
    end.

request_url (User,Route,Query) ->
    f("https://~slocalhost:18069~s?~s",[auth_user(User),Route,qs(Query)]).

auth_user ({User,Pass}) -> f("~s:~s@",[User,Pass]);
auth_user (_) -> "".

qs ([{K,V}|QS]) -> f("~s=~w&~s", [K,V,qs(QS)]);
qs ([]) -> "".

-endif. % EQC
