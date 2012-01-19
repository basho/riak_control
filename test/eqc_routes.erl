-module(eqc_routes).
-compile(export_all).

-ifdef(EQC).

%% EQC headers
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

%% command generators
-define(CLUSTER_LIST,{call,?MODULE,cluster_list,[]}).
-define(PARTITION_LIST,{call,?MODULE,partition_list,[]}).
-define(HTTP_CLUSTER_LIST(U),{call,?MODULE,http_cluster_list,[http,U]}).
-define(HTTP_PARTITION_LIST(U,F),{call,?MODULE,http_partition_list,[http,U,F]}).


%% ---------------------------------------------------------------------------
%% statem state

-record(state,{auth}).


%% ---------------------------------------------------------------------------
%% properties

prop_routes () ->
    ?FORALL(Auth,g_auth(),
            collect(Auth,
            begin
                setup(Auth),

                %% start applications
                [application:start(App) || App <- [sasl,
                                                   crypto,
                                                   inets,
                                                   riak_sysmon,
                                                   webmachine,
                                                   os_mon,
                                                   riak_core,
                                                   riak_control]],

                %% ensure riak_control is up and running
                riak_core:wait_for_application(riak_control),

                %% now run the test with these auth settings
                ?ALWAYS(10,
                        ?FORALL(Cmds,commands(?MODULE,#state{auth=Auth}),
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
                       ?LET(Filter,g_filter(),
                            ?HTTP_PARTITION_LIST(User,Filter))
                      ]))
          ]).

precondition (_State,_Gen) ->
    true.

postcondition (State,{call,_,_,[http,User|_]},Res) ->
    http_validate(State,Res,User);
postcondition (_State,_Gen,_Res) ->
    true.

next_state (State,_V,_Gen) ->
    State.


%% ---------------------------------------------------------------------------
%% negative testing common post-conditions

http_validate (#state{auth=userlist},{C,_,_},{user,pass}) ->
    C < 300;
http_validate (#state{auth=userlist},{C,_,_},_) ->
    C >= 300;
http_validate (#state{auth=none},{C,_,_},_) ->
    C < 300.


%% ---------------------------------------------------------------------------
%% eqc generators

g_auth () ->
    oneof([userlist,none]).

g_userpass () ->
    oneof([{user,pass},
           {user,bad_password},
           {bad_user,pass},
           {bad_user,bad_password},
           none % not passing auth data
          ]).

g_filter () ->
    oneof([{node,[{q,node()}]},
           {handoffs,[]},
           {fallbacks,[]},
           none]).


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

    %% set env values for riak core
    set(http, [{"127.0.0.1",18098}]),
    set(https, [{"127.0.0.1",18069}]),
    set(ssl, [{certfile, "/Users/jeff/Projects/b/riak/rel/files/cert.pem"},
              {keyfile, "/Users/jeff/Projects/b/riak/rel/files/key.pem"}
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

http_cluster_list (_,User) ->
    http_request(User,"/admin/cluster/list",[]).

http_partition_list (_,User,_Filter) ->
    http_request(User,"/admin/ring/partitions",[]).

http_request (User,Route,Query) ->
    Url=request_url(User,Route,Query),
    case httpc:request(get,{Url,[]},[],[]) of
        {ok,{{_HTTP,Code,_OK},_Headers,Body}} -> {Code,Body,Url};
        {ok,{{_HTTP,Code,_OK},Body}} -> {Code,Body,Url}
    end.

request_url (User,Route,_Query) ->
    f("https://~slocalhost:18069~s",[auth_user(User),Route]).

auth_user ({User,Pass}) -> f("~s:~s@",[User,Pass]);
auth_user (_) -> "".

-endif. % EQC
