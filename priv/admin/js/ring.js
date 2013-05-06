minispade.register('ring', function() {

  /**
   * @class
   *
   * Controls filtering, pagination and loading/reloading of the partition list
   * for the cluster.
   */
  RiakControl.RingController = Ember.ObjectController.extend({

    /**
     * Determines the ringReady status text.
     *
     * @returns {String}
     */
    ringReady: function(){
      var ready = this.get('content.ringReady').toString();
      return ready[0].toUpperCase() + ready.slice(1);
    }.property('content.ringReady'),

    /**
     * Determines class names for the claimant node's indicator light.
     * CURRENTLY RETURNS EXAMPLE CODE.
     *
     * @returns {String} - Class names specifying light color.
     */
    claimantIndicatorLight: function () {
      return "gui-light status-light inline-block green";
    }.property(),

    /**
     * Determines class names for the ringReady field's indicator light.
     * CURRENTLY RETURNS EXAMPLE CODE.
     *
     * @returns {String} - Class names specifying light color.
     */
    ringReadyIndicatorLight: function () {
      return "ring-light inline-block green";
    }.property(),

    /**
     * Determines class names for the currentHandoff field's indicator light.
     *
     * @returns {String} - Class names specifying light color.
     */
    currentHandoffsIndicatorLight: function () {
      if (this.get('content.currentHandoffs') > 0) {
        return "gui-light status-light inline-block orange";
      } else {
        return "gui-light status-light inline-block gray";
      }
    }.property('content.currentHandoffs')
  });

  RiakControl.RingDetailsController = Ember.Controller.extend();

  RiakControl.UnreachableNodesController = Ember.ArrayController.extend(
    /** @scope RiakControl.UnreachableNodesController.prototype */ {

    /**
     * Filter out the reachable nodes.
     * */
    filteredContent: function() {
      return this.get('content').filterProperty('reachable', false);
    }.property('content', 'content.@each'),

    /**
     * Reload the recordarray from the server.
     *
     * @returns {void}
     */
    reload: function() {
      this.get('content').reload();
    },

    /**
     * Called by the router to start the polling interval when the page is selected.
     *
     * @returns {void}
     */
     startInterval: function() {
       this._intervalId = setInterval($.proxy(this.reload, this),
        RiakControl.refreshInterval);
     },

    /**
     * Called by the router to stop the polling interval when the page is navigated
     * away from.
     *
     * @returns {void}
     */
    cancelInterval: function() {
      if(this._intervalId) {
        clearInterval(this._intervalId);
      }
    }
  });

  RiakControl.RingDetailsView = Ember.View.extend({
    templateName: 'ring_details'
  });

  RiakControl.UnreachableNodesView = Ember.View.extend({
    templateName: 'unreachable_nodes'
  });

  RiakControl.UnreachableNodeView = Ember.View.extend(
    RiakControl.NodeProperties,
    {
      templateName: 'bad_node'
    }
  );

  RiakControl.UnreachableNodesCollectionView = Ember.CollectionView.extend({
    tagName: 'div',
    itemViewClass: RiakControl.UnreachableNodeView
  });

  RiakControl.HandoffsView = Ember.View.extend({
    templateName: 'handoffs'
  });

  RiakControl.HandoffsController = Ember.ArrayController.extend(
    /** @scope RiakControl.HandoffsController.prototype */ {

    /**
     * Reload the recordarray from the server.
     *
     * @returns {void}
     */
    reload: function() {
      this.get('content').reload();
    },

    /**
     * Called by the router to start the polling interval when the page is selected.
     *
     * @returns {void}
     */
     startInterval: function() {
       this._intervalId = setInterval($.proxy(this.reload, this),
        RiakControl.refreshInterval);
     },

    /**
     * Called by the router to stop the polling interval when the page is navigated
     * away from.
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
   * Container view for the ring page.
   */
  RiakControl.RingView = Ember.View.extend(
    /** @scope RiakControl.RingView.prototype */ {
    templateName: 'ring'
  });

});
