minispade.register('core', function() {

  /**
   * @class
   *
   * Node models a Riak Node participating in a cluster.
   */
  RiakControl.Node = DS.Model.extend(
    /** @scope RiakControl.Node.prototype */ {

    /**
     * Use the node name and ip address as the
     * unique identifier for the node.
     */
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

    /**
     * This boolean attribute determines if the node
     * responsible for the API requests is running
     * Riak Control.
     */
    me: DS.attr("boolean")
  });

  /**
   * @class
   *
   * Partition represents one of the partitions in the
   * consistent hashing ring owned by the cluster.
   */
  RiakControl.Partition = DS.Model.extend(
    /** @scope RiakControl.Partition.prototype */ {

    /**
     * Use the index into the ring as the primary key and
     * unique identifier for this particular partition.
     */
    primaryKey: 'index',

    i: DS.attr("number"),
    index: DS.attr("string"),
    node: DS.attr("string"),
    status: DS.attr("string"),
    reachable: DS.attr("boolean"),

    riak_kv_vnode_status: DS.attr("string"),
    riak_pipe_vnode_status: DS.attr("string"),
    riak_search_vnode_status: DS.attr("string"),

    /**
     * Return status of whether the node is incompatible or not.
     *
     * @returns {Boolean}
     */
    incompatible: function() {
      return this.get('status') === 'incompatible';
    }.property('status'),

    /**
     * Consider an available node one which is compatible, and
     * reachable.
     *
     * @returns {Boolean}
     */
    available: function() {
      return !this.get('incompatible') && this.get('reachable');
    }.property('status', 'reachable'),

    /**
     * Coerce vnode status into representations that are useful
     * for the user interface.
     *
     * @param {String} vnode
     *  vnode name, such as riak_kv.
     *
     * @returns {String} status for use in the user interface.
     */
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

    /**
     * Return status of the riak_kv vnode.
     *
     * @returns {String}
     */
    kvStatus: function() {
      return this.vnodeStatus('riak_kv');
    }.property("riak_kv_vnode_status"),

    /**
     * Return status of the riak_pipe vnode.
     *
     * @returns {String}
     */
    pipeStatus: function() {
      return this.vnodeStatus('riak_pipe');
    }.property("riak_pipe_vnode_status"),

    /**
     * Return status of the riak_search vnode.
     *
     * @returns {String}
     */
    searchStatus: function() {
      return this.vnodeStatus('riak_search');
    }.property("riak_search_vnode_status")
  });

});
