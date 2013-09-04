minispade.register('core', function() {

  /** Handle an array type. */
  var none = Ember.isNone;

  DS.ArrayTransform = DS.Transform.extend({
    deserialize: function(serialized) {
      return none(serialized) ? [] : serialized;
    },

    serialize: function(deserialized) {
      return none(deserialized) ? [] : deserialized;
    },
  });

  RiakControl.register('transform:array', DS.ArrayTransform);

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

    /** The id attribute contains the name after deserializing */
    name: Ember.computed.alias('id'),

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

  /** Register serializer that respects custom primary key */
  RiakControl.register('serializer:node', DS.RESTSerializer.extend({
    primaryKey: 'name'
  }));
});
