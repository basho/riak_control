-module(admin_routes).
-compile([export_all]).

admin_route (Rest) -> ["admin"|Rest].

node_route (Route) -> admin_route(["node"|Route]).
node_route () -> admin_route(["node"]).

cluster_route (Route) -> admin_route(["cluster"|Route]).
cluster_route () -> admin_route(["cluster"]).

ring_route (Route) -> admin_route(["ring"|Route]).
ring_route () -> admin_route(["ring"]).

vnode_route (Route) -> admin_route(["vnode",partition|Route]).
vnode_route () -> admin_route(["vnode",partition]).

