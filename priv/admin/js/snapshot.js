minispade.register('snapshot', function() {

  /**
   * @class
   *
   * SnapshotController is responsible for rendering of the snapshot page and
   * deriving overall cluster state from a selection of nodes.
   */
  RiakControl.SnapshotController = Ember.ObjectController.extend(
    /** @scope RiakControl.SnapshotController.prototype */ {

    /**
     * Reloads the record array associated with this controller.
     *
     * @returns {void}
     */
    reload: function() {
      this.get('content').reload();
    },

    /**
     * Called by the router, to start polling when this controller/view is navigated to.
     *
     * @returns {void}
     */
    startInterval: function() {
      this._intervalId = setInterval($.proxy(this.reload, this), RiakControl.refreshInterval);
    },

    /**
     * Called by the router, to stop polling when this controller/view is navigated away from.
     *
     * @returns {void}
     */
    cancelInterval: function() {
      if(this._intervalId) {
        clearInterval(this._intervalId);
      }
    },

    /**
     * Filter nodes and return just nodes classified as unreachable.
     *
     * @returns {Array} filtered list of nodes
     */
    unreachableNodes: function() {
      return this.get('content').filterProperty('reachable', false);
    }.property('content.@each.reachable'),

    /**
     * Determine if there are any unreachable nodes.
     *
     * @returns {Boolean}
     */
    areUnreachableNodes: function() {
      return this.get('unreachableNodes.length') > 0;
    }.property('content.@each.reachable'),

    /**
     * Filter nodes and return just nodes classified as incompatible.
     *
     * @returns {Array} filtered list of nodes
     */
    incompatibleNodes: function() {
      return this.get('content').filterProperty('status', 'incompatible');
    }.property('content.@each.status'),

    /**
     * Determine if there are any incompatible nodes.
     *
     * @returns {Boolean}
     */
    areIncompatibleNodes: function() {
      return this.get('incompatibleNodes.length') > 0;
    }.property('content.@each.status'),

    /**
     * Filter nodes and return just nodes classified as down.
     *
     * @returns {Array} filtered list of nodes
     */
    downNodes: function() {
      return this.get('content').filterProperty('status', 'down');
    }.property('content.@each.status'),

    /**
     * Determine if there are any down nodes.
     *
     * @returns {Boolean}
     */
    areDownNodes: function() {
      return this.get('downNodes.length') > 0;
    }.property('content.@each.status'),

    /**
     * Filter nodes and return just nodes classified as low-memory nodes.
     *
     * @returns {Array} filtered list of nodes
     */
    lowMemNodes: function() {
      return this.get('content').filterProperty('low_mem', true);
    }.property('content.@each.low_mem'),

    /**
     * Determine if there are any low-memory nodes.
     *
     * @returns {Boolean}
     */
    areLowMemNodes: function() {
      return this.get('lowMemNodes.length') > 0;
    }.property('content.@each.low_mem'),

    /**
     * Derive overall state of the cluster based on node status.
     *
     * @return {Boolean} whether the cluster is happy or not
     */
    healthyCluster: function() {
      var areUnreachableNodes = this.get('areUnreachableNodes');
      var areIncompatibleNodes = this.get('areIncompatibleNodes');
      var areDownNodes = this.get('areDownNodes');
      var areLowMemNodes = this.get('areLowMemNodes');

      return !(areUnreachableNodes || areIncompatibleNodes || areDownNodes || areLowMemNodes);
    }.property('areUnreachableNodes', 'areIncompatibleNodes', 'areDownNodes', 'areLowMemNodes')
  });

});
