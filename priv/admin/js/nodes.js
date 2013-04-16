minispade.register('nodes', function() {

  /**
   * @class
   *
   * Node_managementView is responsible for allowing you to stop
   * or down a node.
   */
  RiakControl.NodesView = Ember.View.extend(
    /** @scope RiakControl.NodesView.prototype */ {
    templateName: 'nodes'
  });

  /**
   * @class
   *
   * NodesController is responsible for displaying the list of nodes
   * in the cluster.
   */
  RiakControl.NodesController = Ember.Controller.extend(
    /** @scope RiakControl.NodesController.prototype */ {

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
     * Removes all checks from radio buttons.
     *
     * @returns {void}
     */
    clearChecked: function () {
      $('#node-list input[type=radio]').each(function (index, item) {
        item.checked = false;
        $(item).parent().css('background-position', 'left top');
      });
    },

    /**
     * Submits requests to stop and/or down nodes to the app.
     */
    applyChanges: function () {
      /*
       * Submit changes to the backend.
       */
    },

    /**
     * If content is loading, return true.
     *
     * @returns {boolean}
     */
    isLoading: false

  });

  /**
   * @class
   *
   * One item in the collection of current cluster views.
   */
  RiakControl.CurrentNodesItemView = Ember.View.extend(
    /** @scope RiakControl.CurrentNodesItemView.prototype */ {

    /* Bindings from the model */

    templateName:       'current_nodes_item',
    nameBinding:        'content.name',
    reachableBinding:   'content.reachable',
    statusBinding:      'content.status',
    ring_pctBinding:    'content.ring_pct',
    pending_pctBinding: 'content.pending_pct',
    mem_totalBinding:   'content.mem_total',
    mem_usedBinding:    'content.mem_used',
    mem_erlangBinding:  'content.mem_erlang',
    meBinding:          'content.me',

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
     * In order for labels to be clickable, they need to be bound to checks/radios
     * by ID.  However, since these nodes are cloned by Ember, we need a way to make
     * sure all of those elements get id's that don't override each other. This
     * function gives us an ID string we can use as a prefix for id's on these other
     * elements.
     *
     * @returns {String}
     */
    nodeID: function() {
      return Ember.guidFor(this);
    }.property(),

    /**
     * An ID value for the leave normally radio button and corresponding label.
     */
    stopRadio: function () {
      return this.get('nodeID') + '_stop_node';
    }.property('nodeID'),

    /**
     * An ID value for the force leave radio button and corresponding label.
     */
    downRadio: function () {
      return this.get('nodeID') + '_down_node';
    }.property('nodeID'),

    /**
     * A node can not be stopped when:
     * - It is unreachable.
     * - It is down.
     */
    stopRadioClasses: function () {
      var status    = this.get('status'),
          reachable = this.get('reachable'),
          classes   = 'gui-radio-wrapper';
      if (!reachable || status === 'down') {
        classes += ' semi-transparent';
      } 
      return classes;
    }.property('status', 'reachable'),

    /**
     * A node can not be marked as down when:
     * - It is alive and well
     * - It is already down.
     */
    downRadioClasses: function () {
      var status    = this.get('status'),
          reachable = this.get('reachable'),
          classes   = 'gui-radio-wrapper';
      if ((reachable && status === 'valid') || status === 'down') {
        classes += ' semi-transparent';
      } 
      return classes;
    }.property('status', 'reachable'),

    /**
     * When a node can't be stopped, disable the user
     * from clicking the stop radio button.
     */
    stopDisablerClasses: function () {
      return 'disabler' + (/\ssemi\-transparent$/.test(this.get('stopRadioClasses')) ? ' show' : '');
    }.property('stopRadioClasses'),

    /**
     * When a node can't be downed, disable the user from
     * clicking the down radio button.
     */
    downDisablerClasses: function () {
      return 'disabler' + (/\ssemi\-transparent$/.test(this.get('downRadioClasses')) ? ' show' : '');
    }.property('downRadioClasses'),

    /**
     * Normalizer.
     *
     * @returns {number}
     */
    mem_divider: function() {
      return this.get('mem_total') / 100;
    }.property('mem_total'),

    /**
     * Compute memory ceiling.
     *
     * @returns {number}
     */
    mem_erlang_ceil: function () {
      return Math.ceil(this.get('mem_erlang') / this.get('mem_divider'));
    }.property('mem_erlang', 'mem_divider'),

    /**
     * Compute free memory from total and used.
     *
     * @returns {number}
     */
    mem_non_erlang: function () {
      return Math.round(
          (this.get('mem_used') / this.get('mem_divider')) - this.get('mem_erlang_ceil'));
    }.property('mem_used', 'mem_divider', 'mem_erlang_ceil'),

    /**
     * Compute free memory from total and used.
     *
     * @returns {number}
     */
    mem_free: function () {
      return this.get('mem_total') - this.get('mem_used');
    }.property('mem_total', 'mem_used'),

    /**
     * Format free memory to be a readbale version.
     *
     * @returns {number}
     */
    mem_free_readable: function () {
      return Math.round(this.get('mem_free') / this.get('mem_divider'));
    }.property('mem_free', 'mem_divider'),

    /**
     * Format used memory to be a readbale version.
     *
     * @returns {number}
     */
    mem_used_readable: function () {
      return Math.round((this.get('mem_total') - this.get('mem_free')) /
          this.get('mem_divider'));
    }.property('mem_total', 'mem_free', 'mem_divider'),

    /**
     * Return CSS style for rendering memory used by Erlang.
     *
     * @returns {number}
     */
    mem_erlang_style: function () {
      return 'width: ' + this.get('mem_erlang_ceil') + '%';
    }.property('mem_erlang_ceil'),

    /**
     * Return CSS style for rendering occupied non-erlang memory.
     *
     * @returns {string}
     */
    mem_non_erlang_style: function () {
      return 'width: ' + this.get('mem_non_erlang') + '%';
    }.property('mem_non_erlang'),

    /**
     * Return CSS style for rendering free memory.
     *
     * @returns {string}
     */
    mem_free_style: function () {
      return 'width: ' + this.get('mem_free_readable') + '%';
    }.property('mem_free_readable'),

    /**
     * Formatted ring percentage.
     *
     * @returns {string}
     */
    ring_pct_readable: function () {
      return Math.round(this.get('ring_pct') * 100);
    }.property('ring_pct')
  });

  /**
   * @class
   *
   * Collection view for showing the current cluster.
   */
  RiakControl.CurrentNodesView = Ember.CollectionView.extend(
    /** @scope RiakControl.CurrentClusterView.prototype */ {
    itemViewClass: RiakControl.CurrentNodesItemView
  });

});
