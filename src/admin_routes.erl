-module(admin_routes).
-compile([export_all]).

%% helper, ensures all routes begin with /admin
admin_route (Rest) -> ["admin"|Rest].

%% routes that query/act on a single node in the cluster
node_route (Route) -> admin_route(["node"|Route]).
node_route () -> admin_route(["node"]).

%% routes that query/act on the cluster
cluster_route (Route) -> admin_route(["cluster"|Route]).
cluster_route () -> admin_route(["cluster"]).

%% routes that query/act on partitions
ring_route (Route) -> admin_route(["ring"|Route]).
ring_route () -> admin_route(["ring"]).

%% routes that query/act on individual v-nodes
vnode_route (Route) -> admin_route(["vnode",partition|Route]).
vnode_route () -> admin_route(["vnode",partition]).

