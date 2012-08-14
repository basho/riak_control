minispade.register('core', function() {
  RiakControl.Node = DS.Model.extend({
    primaryKey: 'name',

    name: DS.attr("string"),
    status: DS.attr("string"),
    reachable: DS.attr("boolean"),

    ring_pct: DS.attr("number"),
    pending_pct: DS.attr("number"),

    mem_total: DS.attr("number"),
    mem_used: DS.attr("number"),
    mem_erlang: DS.attr("number"),
    low_mem: DS.attr("boolean"),

    me: DS.attr("boolean")
  });

  RiakControl.Partition = DS.Model.extend({
    primaryKey: 'index',

    i: DS.attr("number"),
    index: DS.attr("string"),
    node: DS.attr("string"),
    status: DS.attr("string"),
    reachable: DS.attr("boolean"),

    riak_kv_vnode_status: DS.attr("string"),
    riak_pipe_vnode_status: DS.attr("string"),
    riak_search_vnode_status: DS.attr("string"),

    vnodeStatus: function(vnode) {
      var partitionStatus = this.get('status');
      var vnodeReachable = this.get('reachable');
      var vnodeStatus = this.get(vnode + '_vnode_status');

      if(partitionStatus === "incompatible") { return "unknown"; }
      if(vnodeStatus === "handoff") { return "handoff"; }
      if(vnodeStatus === "primary" && vnodeReachable === true) { return "active"; }
      if(vnodeStatus === "fallback") { return "fallback"; }

      return "disabled";
    },

    kvStatus: function() {
      return this.vnodeStatus('riak_kv');
    }.property("riak_kv_vnode_status"),

    pipeStatus: function() {
      return this.vnodeStatus('riak_pipe');
    }.property("riak_pipe_vnode_status"),

    searchStatus: function() {
      return this.vnodeStatus('riak_search');
    }.property("riak_search_vnode_status")
  });
});
