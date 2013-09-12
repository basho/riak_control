minispade.register('nodes', function() {

  /**
   * @class
   *
   * NodesController is responsible for displaying the list of nodes
   * in the cluster.
   */
  RiakControl.NodesController = Ember.Controller.extend(
    /**
     * Shares properties with RiakControl.ClusterController
     */
    RiakControl.ClusterAndNodeControls,
    /** @scope RiakControl.NodesController.prototype */ {

    /**
     * Reloads the record array associated with this controller.
     *
     * @returns {void}
     */
    reload: function() {
      this.get('content').reload();
    },

    actions: {
      /**
       * Removes all checks from radio buttons.
       *
       * @returns {void}
       */
      clearChecked: function() {
        $('#node-list input[type=radio]').each(function(index, item) {
          item.checked = false;
          $(item).parent().css('background-position', 'left top');
        });
      },

      /**
       * Submits requests to stop and/or down nodes to the app.
       */
      applyChanges: function() {
        var self = this;

        $("#node-list input[type='radio']:checked").each(function(index, item) {
          var name = item.name,
              action = item.value,
              replacement;

          // Empty string instead of undefined for null.
          if(replacement === undefined) {
            replacement = '';
          }

          self.send('stageChange', name, action, replacement);
        });

        self.send('clearChecked');
      }
    }

  });

  /**
   * @class
   *
   * One item in the collection of current cluster views.
   */
  RiakControl.CurrentNodesItemView = Ember.View.extend(
    /**
     * Shares properties with other views that display lists of nodes.
     */
    RiakControl.NodeProperties,
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
    stopRadio: function() {
      return this.get('nodeID') + '_stop_node';
    }.property('nodeID'),

    /**
     * An ID value for the force leave radio button and corresponding label.
     */
    downRadio: function() {
      return this.get('nodeID') + '_down_node';
    }.property('nodeID'),

    /**
     * A node can not be stopped when:
     * - It is unreachable.
     * - It is down.
     */
    stopRadioClasses: function() {
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
    downRadioClasses: function() {
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
    stopDisablerClasses: function() {
      return 'disabler' + (/\ssemi\-transparent$/.test(this.get('stopRadioClasses')) ? ' show' : '');
    }.property('stopRadioClasses'),

    /**
     * When a node can't be downed, disable the user from
     * clicking the down radio button.
     */
    downDisablerClasses: function() {
      return 'disabler' + (/\ssemi\-transparent$/.test(this.get('downRadioClasses')) ? ' show' : '');
    }.property('downRadioClasses')
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
