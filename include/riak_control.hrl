%% These two should always match, in terms of webmachine dispatcher
%% logic, and ADMIN_BASE_PATH should always end with a /
-define(ADMIN_BASE_PATH, "/admin/").
-define(ADMIN_BASE_ROUTE, ["admin"]).

%% Value for WWW-Authenticate header
-define(ADMIN_AUTH_HEAD, "Basic realm=riak").
