minispade.register('shared', function () {

  /**
   * ClusterAndNodeControls contains properties shared by:
   * - RiakControl.ClusterController
   * - RiakControl.NodesController
   */
  RiakControl.ClusterAndNodeControls = Ember.Mixin.create({
    
    /**
     * If content is loading, return true.
     *
     * @returns {boolean}
     */
    isLoading: false,

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
    }
  });

  /**
   * RiakControl.NodeProperties contains properties shared by:
   * - RiakControl.CurrentClusterItemView
   * - RiakControl.StagedClusterItemView
   * - RiakControl.CurrentNodesItemView
   */
  RiakControl.NodeProperties = Ember.Mixin.create({

    /**
     * Color the lights appropriately based on the node status.
     *
     * @returns {string}
     */
    indicatorLights: function() {
      var status = this.get('status');
      var reachable = this.get('reachable');
      var color;

      if(reachable === false) {
        color = "red";
      } else if(status === 'leaving' || status === 'joining') {
        color = "orange";
      } else if (status === 'valid') {
        color = "green";
      } else {
        color = "grey";
      }

      return "gui-light status-light inline-block " + color;
    }.property('reachable', 'status'),

    /**
     * Color the arrows in the partitions column appropriately based
     * on how ring_pct and pending_pct compare.
     *
     * @returns {String}
     */
    coloredArrows: function() {
      var current = this.get('ring_pct'),
          pending = this.get('pending_pct'),
          common  = 'left pct-arrows ';

      if (pending > current) {
        return common + 'pct-gaining';
      } else if (pending < current) {
        return common + 'pct-losing';
      } else {
        return common + 'pct-static';
      }
    }.property('ring_pct', 'pending_pct'),

    /**
     * Normalizer.
     *
     * @returns {number}
     */
    memDivider: function() {
      return this.get('mem_total') / 100;
    }.property('mem_total'),

    /**
     * Compute memory ceiling.
     *
     * @returns {number}
     */
    memErlangCeil: function () {
      return Math.ceil(this.get('mem_erlang') / this.get('memDivider'));
    }.property('mem_erlang', 'memDivider'),

    /**
     * Compute free memory from total and used.
     *
     * @returns {number}
     */
    memNonErlang: function () {
      return Math.round(
          (this.get('mem_used') / this.get('memDivider')) - this.get('memErlangCeil'));
    }.property('mem_used', 'memDivider', 'memErlangCeil'),

    /**
     * Compute free memory from total and used.
     *
     * @returns {number}
     */
    memFree: function () {
      return this.get('mem_total') - this.get('mem_used');
    }.property('mem_total', 'mem_used'),

    /**
     * Format free memory to be a readbale version.
     *
     * @returns {number}
     */
    memFreeReadable: function () {
      return Math.round(this.get('memFree') / this.get('memDivider'));
    }.property('memFree', 'memDivider'),

    /**
     * Format used memory to be a readbale version.
     *
     * @returns {number}
     */
    memUsedReadable: function () {
      return Math.round((this.get('mem_total') - this.get('memFree')) /
          this.get('memDivider'));
    }.property('mem_total', 'memFree', 'memDivider'),

    /**
     * Return CSS style for rendering memory used by Erlang.
     *
     * @returns {number}
     */
    memErlangStyle: function () {
      return 'width: ' + this.get('memErlangCeil') + '%';
    }.property('memErlangCeil'),

    /**
     * Return CSS style for rendering occupied non-erlang memory.
     *
     * @returns {string}
     */
    memNonErlangStyle: function () {
      return 'width: ' + this.get('memNonErlang') + '%';
    }.property('memNonErlang'),

    /**
     * Return CSS style for rendering free memory.
     *
     * @returns {string}
     */
    memFreeStyle: function () {
      return 'width: ' + this.get('memFreeReadable') + '%';
    }.property('memFreeReadable'),

    /**
     * Formatted ring percentage.
     *
     * @returns {string}
     */
    ringPctReadable: function () {
      return Math.round(this.get('ring_pct') * 100);
    }.property('ring_pct')
  });

});
