minispade.register('cluster', function() {

  /**
   * @class
   *
   * Responsible for modeling one specific cluster node.
   */
  RiakControl.CurrentClusterNode = Ember.Object.extend(
    /** @scope RiakControl.CurrentClusterNode.prototype */ {});

  /**
   * @class
   *
   * Responsible for modeling one specific cluster node.
   */
  RiakControl.StagedClusterNode = Ember.Object.extend(
    /** @scope RiakControl.StagedClusterNode.prototype */ {

    /**
     * Does the node have a replacement?
     *
     * @returns {boolean}
     */
    isReplaced: function() {
      return this.get('replacement') !== "undefined" ? true : false;
    }.property('replacement'),

    /**
     * Is the node taking an action?
     *
     * @returns {boolean}
     */
    isAction: function() {
        return this.get('action') !== "undefined" ? true : false;
    }.property('action')

  });

  /**
   * @class
   *
   * Responsible for modeling the current and staged cluster.
   */
  RiakControl.CurrentAndPlannedCluster = Ember.Object.extend(
    /** @scope RiakControl.CurrentAndPlannedCluster.prototype */ {});

  /**
   * @class
   *
   * ClusterController is responsible for display the list of nodes
   * in the cluster.  This controller is basically a placeholder and
   * wrapper around the legacy cluster page until we rewrite it.
   */
  RiakControl.ClusterController = Ember.ObjectController.extend(
    /**
     * Shares properties with RiakControl.NodesController
     */
    RiakControl.ClusterAndNodeControls,
    /** @scope RiakControl.ClusterController.prototype */ {

    /**
     * Refresh a particular cluster, giving a cluster returned as JSON,
     * and a cluster modeled in Ember.
     *
     * Not computationally efficient at all, but explicit for debugging
     * the Ember bindings and propagation.
     *
     * @returns {void}
     */
    refresh: function(newCluster, existingCluster, nodeFactory) {
      newCluster.forEach(function(node) {
        var exists = existingCluster.findProperty('name', node.name);

        // If it doesn't exist yet, add it.  If it does, update it.
        if(exists !== undefined) {
          exists.setProperties(node);
        } else {
          existingCluster.pushObject(nodeFactory.create(node));
        }
      });

      // Iterate the cluster removing nodes that shouldn't
      // be there.
      var changesOccurred    = false;
      var replacementCluster = [];

      existingCluster.forEach(function(node, i) {
        var exists = newCluster.findProperty('name', node.name);

        if(exists === undefined) {
          node.destroy();
          changesOccurred = true;
        } else {
          replacementCluster.pushObject(node);
        }
      });

      if(changesOccurred) {
        existingCluster.set('[]', replacementCluster.get('[]'));
      }
    },

    /**
     * Load data from server.
     *
     * @returns {void}
     */
    load: function() {
      var self = this;

      $.ajax({
        type:     'GET',
        url:      '/admin/cluster',
        dataType: 'json',

        success: function(d) {
          var updatedCurrentCluster = d.cluster.current;
          var currentCurrentCluster = self.get('content.currentCluster');

          self.refresh(updatedCurrentCluster,
            currentCurrentCluster, RiakControl.CurrentClusterNode);

          var updatedStagedCluster = d.cluster.staged;

          if(updatedStagedCluster === 'ring_not_ready') {
            self.set('ringNotReady', true);
          } else {
            self.set('ringNotReady', false);
          }

          if(updatedStagedCluster === 'legacy') {
            self.set('legacyRing', true);
          } else {
            self.set('legacyRing', false);
          }

          if($.isArray(updatedStagedCluster)) {
            var currentStagedCluster = self.get('content.stagedCluster');

            self.refresh(updatedStagedCluster,
              currentStagedCluster, RiakControl.StagedClusterNode);
          }
        },

        error: function (jqXHR, textStatus, errorThrown) {
          if(jqXHR.status === 404 || jqXHR.status === 0) {
            self.get('displayError').call(self, undefined, undefined, "The node hosting Riak Control is unavailable.");
          } else {
            self.get('displayError').call(self, jqXHR, textStatus, errorThrown);
          }
        }
      });
    },

    /**
     * Reload data from server.
     *
     * @returns {void}
     */
    reload: function() {
      this.load();
    },

    /**
     * Holds a boolean tracking if the ring is legacy.
     */
    legacyRing: false,

    /**
     * Holds a boolean tracking if the ring is not yet ready.
     */
    ringNotReady: false,

    /**
     * Returns whether this is a standalone node or not.
     */
    standalone: function() {
      return this.get('content.currentCluster.length') <= 1;
    }.property('content.currentCluster', 'content.currentCluster.@each'),

    /**
     * Return nodes from the current cluster which have not been deleted.
     *
     * @returns {Ember.Array}
     */
    activeCurrentCluster: function() {
      return this.get('content.currentCluster').filterProperty('isDestroyed', false);
    }.property('content.currentCluster', 'content.currentCluster.@each'),

    /**
     * Return list of nodes joining the cluster.
     *
     * @returns {Ember.Array}
     */
    joiningNodes: function() {
      return this.get('activeCurrentCluster').filterProperty('status', 'joining');
    }.property('activeCurrentCluster', 'activeCurrentCluster.@each'),

    /**
     * Determines whether or not we have any joining nodes.
     *
     * @returns {Boolean}
     */
    joiningNodesExist: function () {
      return this.get('joiningNodes').length ? true : false;
    }.property('joiningNodes'),

    /**
     * Holds a boolean tracking whether or not there are any stages in our plan.
     */
    emptyPlan: function() {
      return !this.get('activeStagedCluster.length') > 0;
    }.property('activeStagedCluster', 'activeStagedCluster.length'),

    /**
     * Return nodes from the staged cluster which have not been deleted.
     *
     * @returns {Ember.Array}
     */
    activeStagedCluster: function() {
      return this.get('content.stagedCluster').filterProperty('isDestroyed', false);
    }.property('content.stagedCluster', 'content.stagedCluster.@each'),

    commitPlan: function(ev) {
      ev.preventDefault();

      var self = this;
      var confirmed = $(document).find("[name='confirmed']:checked").val();

      if(confirmed) {
        $.ajax({
          type:     'POST',
          url:      '/admin/cluster',
          dataType: 'json',
          success:  function(d) { self.reload(); },
          error:    function (jqXHR, textStatus, errorThrown) {
            self.get('displayError').call(self, jqXHR, textStatus, errorThrown);
          }
        });
      } else {
        self.get('displayError')(undefined, undefined, "Please confirm the plan.");
      }
    },

    /**
     * Clear the currently staged cluster plan.
     *
     * @returns {void}
     */
    clearPlan: function(ev) {
      ev.preventDefault();

      var self = this;

      $.ajax({
        type:     'DELETE',
        url:      '/admin/cluster',
        dataType: 'json',
        success:  function(d) { self.reload(); },
        error:    function (jqXHR, textStatus, errorThrown) {
          self.get('displayError').call(self, jqXHR, textStatus, errorThrown);
        }
      });
    },

    /**
     * Join a node.
     *
     * @returns {void}
     */
    joinNode: function(ev) {
      ev.preventDefault();

      var self    = this;
      var node    = this.get('joinNodeField');
      var success = function() {
        self.set('joinNodeField', undefined);
      };

      this.stageChange(node, "join", "", success, undefined);
    },

    /**
     * There are various reasons we wouldn't want to display
     * the planned cluster.  If none of those reasons are present,
     * go ahead and show the whole planned cluster view.
     *
     * @returns {boolean}
     */
    displayPlan: function () {
      return !this.get('isLoading') && !this.get('ringNotReady') &&
             !this.get('emptyPlan') && !this.get('legacyRing');
    }.property('isLoading', 'ringNotReady', 'emptyPlan', 'legacyRing')

  });

  /**
   * @class
   *
   * Join Node text field.
   *
   */
  RiakControl.JoinNodeView = Ember.TextField.extend(
    /** @scope RiakControl.JoinNodeView.prototype */ {
    valueBinding: 'controller.joinNodeField',
    classNames: ['gui-input', 'gui-text'],

    /**
     * When the user presses the enter/return key in the
     * add nodes field, calls the 'joinNode' method of
     * the controller.
     *
     * @param {Object} ev - The keyup event.
     *
     * @returns {void}
     */
    keyUp: function (ev) {
      var controller = this.get('controller');
      if(ev.keyCode === 13){
        controller.get('joinNode').call(controller, ev);
      }
    }
  });

  /**
   * @class
   *
   * ClusterView is responsible for display the list of nodes
   * in the cluster.  This controller is basically a placeholder and
   * wrapper around the legacy cluster page until we rewrite it.
   */
  RiakControl.ClusterView = Ember.View.extend(
    /** @scope RiakControl.ClusterView.prototype */ {
    templateName: 'cluster'
  });

  /**
   * @class
   *
   * Toggle button for a current cluster node to expand actions.
   */
  RiakControl.CurrentClusterToggleView = Ember.View.extend(
    /** @scope RiakControl.CurrentClusterToggleView.prototype */ {

    /**
     * Handle click event on the action toggle.
     *
     * @returns {void}
     */
    click: function () {
      var prop = this.get('parentView.expanded');

      if (prop) {
        this.get('parentView').set('expanded', false);
      } else {
        this.get('parentView').set('expanded', true);
      }
    }
  });

  /**
   * @class
   *
   * One item in the collection of current cluster views.
   */
  RiakControl.CurrentClusterItemView = Ember.View.extend(
    /**
     * Shares properties with other views that display lists of nodes.
     */
    RiakControl.NodeProperties,
    /** @scope RiakControl.CurrentClusterItemView.prototype */ {

    /* Bindings from the model */

    templateName:       'current_cluster_item',
    nameBinding:        'content.name',
    reachableBinding:   'content.reachable',
    statusBinding:      'content.status',
    ring_pctBinding:    'content.ring_pct',
    pending_pctBinding: 'content.pending_pct',
    mem_totalBinding:   'content.mem_total',
    mem_usedBinding:    'content.mem_used',
    mem_erlangBinding:  'content.mem_erlang',
    meBinding:          'content.me',

    classNameBindings:  ['expanded:open'],

    /**
     * Stage a change for a given node.
     *
     * @returns {void}
     */
    stageChange: function(ev) {
      ev.preventDefault();

      var self = this;
      var controller = this.get('controller');

      var name = this.get('name');

      var action = this.$().
        find("input[type='radio']:checked").val();
      var forced = this.$().
        find("input[type='checkbox']:checked").val();
      var replacement = this.$().
        find("input[type='select']:selected").val();

      // Make sure we handle the force replace correctly.
      //
      if(action === 'replace' && forced === 'true') {
        action = 'force_replace';
      }

      // Empty string instead of undefined for null.
      if(replacement === undefined) {
        replacement = '';
      }

      controller.stageChange(name, action, replacement);
    },

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
    normalLeaveRadio: function () {
      return this.get('nodeID') + '_normal_leave';
    }.property('nodeID'),

    /**
     * An ID value for the force leave radio button and corresponding label.
     */
    forceLeaveRadio: function () {
      return this.get('nodeID') + '_force_leave';
    }.property('nodeID'),

    /**
     * An ID value for the replace node radio button and corresponding label.
     */
    replaceRadio: function () {
      return this.get('nodeID') + '_replace';
    }.property('nodeID'),

    /**
     * An ID value for the force replace check box and corresponding label.
     */
    forceReplaceCheck: function () {
      return this.get('nodeID') + '_force_replace';
    }.property('nodeID'),

    /**
     * When there are no joining nodes, the radio button for selecting
     * a node should be grayed out.  This will put the proper classes
     * on that radio button to gray it out when there are no joining nodes.
     */
    replaceRadioClasses: function () {
      return 'gui-radio-wrapper' + (this.get('controller.joiningNodesExist') ? '' : ' semi-transparent');
    }.property('controller.joiningNodesExist')

  });

  /**
   * @class
   *
   * Collection view for showing the current cluster.
   */
  RiakControl.CurrentClusterView = Ember.CollectionView.extend(
    /** @scope RiakControl.CurrentClusterView.prototype */ {
    itemViewClass: RiakControl.CurrentClusterItemView
  });

  /**
   * @class
   *
   * One item in the collection of current cluster views.
   */
  RiakControl.StagedClusterItemView = Ember.View.extend(
    /**
     * Shares properties with other views that display lists of nodes.
     */
    RiakControl.NodeProperties,
    /** @scope RiakControl.StagedClusterItemView.prototype */ {

    /* Bindings from the model */

    templateName:       'staged_cluster_item',
    nameBinding:        'content.name',
    statusBinding:      'content.status',
    ring_pctBinding:    'content.ring_pct',
    isReplacedBinding:  'content.isReplaced',
    isActionBinding:    'content.isAction',

    /* Necessary rename to avoid collision */
    node_actionBinding: 'content.action'

  });

  /**
   * @class
   *
   * Collection view for showing the staged cluster.
   */
  RiakControl.StagedClusterView = Ember.CollectionView.extend(
    /** @scope RiakControl.StagedClusterView.prototype */ {
    itemViewClass: RiakControl.StagedClusterItemView
  });

  /**
   * @class
   *
   * View for select menus in cluster item actions boxes.
   */
  RiakControl.ClusterItemSelectView = Ember.Select.extend({

    /*
     * When the select menu changes, update its display.
     */
    change: function () {
      var item = this.$(),
          val  = item.val();
      Ember.run.next(function() {
        item.parent().find('.gui-dropdown-bg').text(val);
      });
    }
  });

});
