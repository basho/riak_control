-module(riak_control_schema_test).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

basic_schema_test() ->
    lager:start(),
    Config = cuttlefish_unit:generate_config("../priv/riak_control.schema", []),

    cuttlefish_unit:assert_config(Config, "riak_control.enabled", false),
    cuttlefish_unit:assert_config(Config, "riak_control.auth", userlist),
    cuttlefish_unit:assert_not_configured(Config, "riak_control.userlist"),
    ok.

override_test() ->
    Conf = [
        {["riak_control"], on},
        {["riak_control", "auth", "mode"], off}
    ],
    Config = cuttlefish_unit:generate_config("../priv/riak_control.schema", Conf),
    cuttlefish_unit:assert_config(Config, "riak_control.enabled", true),
    cuttlefish_unit:assert_config(Config, "riak_control.auth", none),
    cuttlefish_unit:assert_not_configured(Config, "riak_control.userlist"),
    ok.

userlist_schema_test() ->
    CuttlefishConf = [
        {["riak_control", "auth", "user", "dev", "password"], "1234"},
        {["riak_control", "auth", "user", "admin", "password"], "5678"}
    ],

    Config = cuttlefish_unit:generate_config("../priv/riak_control.schema", CuttlefishConf),

    cuttlefish_unit:assert_config(Config, "riak_control.userlist.dev", "1234"),
    cuttlefish_unit:assert_config(Config, "riak_control.userlist.admin", "5678"),

    %% Other than userlist, which we overrode in CuttlefishConf, the others should still be set to defaults:
    cuttlefish_unit:assert_config(Config, "riak_control.enabled", false),
    cuttlefish_unit:assert_config(Config, "riak_control.auth", userlist),
    ok.
