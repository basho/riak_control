-module(riak_control_schema_test).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

basic_schema_test() ->
    Config = cuttlefish_unit:generate_config("../priv/riak_control.schema", []),

    cuttlefish_unit:assert_config(Config, "riak_control.enabled", false),
    cuttlefish_unit:assert_config(Config, "riak_control.auth", userlist),
    cuttlefish_unit:assert_config(Config, "riak_control.userlist", [{"user","pass"}]),
    ok.

userlist_schema_test() ->
    CuttlefishConf = [
        {["riak_control", "user", "dev", "password"], "1234"},
        {["riak_control", "user", "admin", "password"], "5678"}
    ],

    Config = cuttlefish_unit:generate_config("../priv/riak_control.schema", CuttlefishConf),

    cuttlefish_unit:assert_config(Config, "riak_control.userlist.dev", "1234"),
    cuttlefish_unit:assert_config(Config, "riak_control.userlist.admin", "5678"),

    %% Other than userlist, which we overrode in CuttlefishConf, the others should still be set to defaults:
    cuttlefish_unit:assert_config(Config, "riak_control.enabled", false),
    cuttlefish_unit:assert_config(Config, "riak_control.auth", userlist),
    ok.
