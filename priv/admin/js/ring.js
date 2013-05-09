minispade.register('ring', function() {

  /**
   * @class
   *
   * Controls filtering, pagination and loading/reloading of the partition list
   * for the cluster.
   */
  RiakControl.RingController = Ember.ObjectController.extend(
    /** @scope RiakControl.RingController.prototype */ {

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

  RiakControl.RingDetailsController = Ember.Controller.extend(
    /** @scope RiakControl.RingDetailsController.prototype */ {
    });

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

  /**
   * @class
   *
   * Container for the ring details.
   */
  RiakControl.RingDetailsView = Ember.View.extend(
    /** @scope RiakControl.RingDetailsView.prototype */ {
    templateName: 'ring_details'
  });

  /**
   * @class
   *
   * Unreachable node list.
   */
  RiakControl.UnreachableNodesView = Ember.View.extend(
    /** @scope RiakControl.UnreachableNodesView.prototype */ {
    templateName: 'unreachable_nodes'
  });

  /**
   * @class
   *
   * Specifies which template to use for listing out bad nodes.
   */
  RiakControl.UnreachableNodeView = Ember.View.extend(
    RiakControl.NodeProperties,
    /** @scope RiakControl.UnreachableNodeView.prototype */
    {
      templateName:       'bad_node',
      nameBinding:        'content.name',
      reachableBinding:   'content.reachable',
      statusBinding:      'content.status',
      ring_pctBinding:    'content.ring_pct',
      pending_pctBinding: 'content.pending_pct',
      mem_totalBinding:   'content.mem_total',
      mem_usedBinding:    'content.mem_used',
      mem_erlangBinding:  'content.mem_erlang',
      meBinding:          'content.me'
    }
  );

  /**
   * @class
   *
   * Specifies which template to use for listing out bad nodes.
   */
  RiakControl.HandoffView = Ember.View.extend(
    RiakControl.NodeProperties,
    /** @scope RiakControl.HandoffView.prototype */
    {
      templateName:       'handoff',
      indexBinding:       'content.index',
      ownerBinding:       'content.owner',
      next_ownerBinding:  'content.next_owner',
      waiting_forBinding: 'content.waiting_for'
    }
  );

  /**
   * @class
   *
   * Creates a collection of bad nodes.
   */
  RiakControl.UnreachableNodesCollectionView = Ember.CollectionView.extend(
    /** @scope RiakControl.UnreachableNodesCollectionView.prototype */ {
    tagName: 'div',
    itemViewClass: RiakControl.UnreachableNodeView
  });

  /**
   * @class
   *
   * Creates a collection of handoffs.
   */
  RiakControl.HandoffsCollectionView = Ember.CollectionView.extend(
    /** @scope RiakControl.HandoffsCollectionView.prototype */ {
    tagName: 'div',
    itemViewClass: RiakControl.HandoffView
  });

  /**
   * @class
   *
   * Specified which template to use for listing out handoffs.
   */
  RiakControl.HandoffsView = Ember.View.extend(
    /** @scope RiakControl.HandoffsView.prototype */ {
    templateName: 'handoffs'
  });

  /**
   * @class
   *
   * Responsible for loading all of the handoffs.
   */
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
