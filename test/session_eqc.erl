-module(session_eqc).

-ifdef(EQC).

%% EQC headers
-include_lib("eqc/include/eqc_statem.hrl").
-include_lib("eqc/include/eqc.hrl").

%% public api
-compile(export_all).

-define(SESSION(F), {call,riak_control_session,F,[]}).

%% ====================================================================
%% Tests
%% ====================================================================


initial_state () ->
    not_running.


next_state (not_running,{ok,Pid},?SESSION(start_link)) ->
    test_nodes;
next_state (test_nodes,_,?SESSION(get_nodes)) ->
    shutdown.


command (not_running) -> ?SESSION(start_link);
command ({test_nodes,Pid}) -> ?SESSION(get_nodes).


precondition (_,_) -> true.
postcondition (_,_,_) -> true.


%% -? EQC
-endif.
