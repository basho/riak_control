minispade.register('core', function() {

  /** Handle an array type. */
  DS.attr.transforms.array = {
    from: function(serialized) {
      return Em.none(serialized) ? [] : serialized;
    },

    to: function(deserialized) {
      return Em.none(deserialized) ? [] : deserialized;
    }
  };

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
    me: DS.attr("boolean"),

    /**
     * Whether this node is currently the claimant or not.
     */
    claimant: DS.attr("boolean")
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

    /* Partition index. */
    index: DS.attr("string"),

    /* Cluster n_val. */
    n_val: DS.attr("number"),

    /* Number of reachable/available primaries. */
    available: DS.attr("number"),

    /* Cluster quorum value. */
    quorum: DS.attr("number"),

    /* Whether all primaries are on distinct nodes. */
    distinct: DS.attr("boolean"),

    /* The list of unavailable primaries. */
    unavailable_nodes: DS.attr("array"),

    /* Whether unavailable nodes are present. */
    unavailable: function() {
      return this.get('unavailable_nodes').length > 0;
    }.property('unavailable_nodes'),

    /* The list of available primaries. */
    available_nodes: DS.attr("array"),

    /* The list of primaries. */
    all_nodes: DS.attr("array"),

    /* Whether or not all primaries are down or not. */
    allPrimariesDown: function() {
      return this.get('available') === 0;
    }.property('available'),

    /* Whether or not a quorum of primaries are down. */
    quorumUnavailable: function() {
      return this.get('available') < this.get('quorum');
    }.property('quorum', 'available')
  });

});
