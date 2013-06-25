minispade.register('stats', function() {

  /**
   * @class
   *
   * SnapshotController is responsible for rendering of the snapshot page and
   * deriving overall cluster state from a selection of nodes.
   */
  RiakControl.StatsController = Ember.ObjectController.extend(
    /** @scope RiakControl.StatsController.prototype */ {

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
    }
  });

  /**
   * @class
   *
   * View showing the overall cluster status.
   */
  RiakControl.StatsView = Ember.View.extend(
    /** @scope RiakControl.StatsView.prototype */ {
    templateName: 'stats'
  });

});
