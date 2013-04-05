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

      self.setProperties({ ringNotReady: false, legacyRing: false });

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
          } else if (updatedStagedCluster === 'legacy') {
            self.set('legacyRing', true);
          } else {
            var currentStagedCluster = self.get('content.stagedCluster');

            self.refresh(updatedStagedCluster,
              currentStagedCluster, RiakControl.StagedClusterNode);
          }
        },

        error: self.get('displayError')
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
     * Called by the router to start the polling interval when the page
     * is selected.
     *
     * @returns {void}
     */
    startInterval: function() {
      this._intervalId = setInterval($.proxy(this.reload, this),
              RiakControl.refreshInterval);
    },

    /**
     * Called by the router to stop the polling interval when the page
     * is navigated away from.
     *
     * @returns {void}
     */
    cancelInterval: function() {
      if(this._intervalId) {
        clearInterval(this._intervalId);
      }
    },

    /**
     * If content is loading, return true.
     *
     * @returns {boolean}
     */
    isLoading: false,

    /**
     * Holds a boolean tracking if the ring is legacy.
     */
    legacyRing: false,

    /**
     * Holds a boolean tracking if the ring is not yet ready.
     */
    ringNotReady: false,

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
          error:    self.get('displayError')
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
        error:    self.get('displayError')
      });
    },

    /**
     * Add a new node.
     *
     * @returns {void}
     */
    addNode: function(ev) {
      ev.preventDefault();

      var self = this;
      var node = this.get('addNodeField');

      this.stageChange(node, "join", "");
    },

    /**
     * Stage a change.
     *
     * @returns {void}
     */
    stageChange: function(node, action, replacement) {
      var self = this;

      $.ajax({
        type:     'PUT',
        url:      '/admin/cluster',
        dataType: 'json',

        data:     { changes:
                    [{
                      node:        node,
                      action:      action,
                      replacement: replacement
                    }]
                  },

        success: function(d) { self.reload(); },

        error:   self.get('displayError')
      });
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
    }.property('isLoading', 'ringNotReady', 'emptyPlan', 'legacyRing'),

    /**
     * Whenever an ajax call returns an error, we display
     * the error for the user.
     *
     * @param {Object} jqXHR       The xhr request object generated by jQuery.
     * @param {String} textStatus  The status of the response.
     * @param {Error}  errorThrown The error object produced by the ajax request.
     *
     * @returns {void}
     */
    displayError: function (jqXHR, textStatus, errorThrown) {
      var parsed, errors;

      if(jqXHR) {
        parsed = JSON.parse(jqXHR.responseText);
        errors = parsed.errors.join(', ');
      } else {
        errors = errorThrown;
      }

      $('.error-message').removeClass('hide').find('.error-text').
          html('Request failed: ' + errors);
    }
  });

  /**
   * @class
   *
   * Add Node text field.
   *
   */
  RiakControl.AddNodeView = Ember.TextField.extend(
    /** @scope RiakControl.AddNodeView.prototype */ {
    valueBinding: 'controller.addNodeField',
    classNames: ['gui-input', 'gui-text']
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
    node_id: function() {
      return Ember.generateGuid();
    }.property(),

    /**
     * An ID value for the leave normally radio button and corresponding label.
     */
    normal_leave_radio: function () {
      return this.get('node_id') + '_normal_leave';
    }.property('node_id'),

    /**
     * An ID value for the force leave radio button and corresponding label.
     */
    force_leave_radio: function () {
      return this.get('node_id') + '_force_leave';
    }.property('node_id'),

    /**
     * An ID value for the replace node radio button and corresponding label.
     */
    replace_radio: function () {
      return this.get('node_id') + '_replace';
    }.property('node_id'),

    /**
     * An ID value for the force replace check box and corresponding label.
     */
    force_replace_check: function () {
      return this.get('node_id') + '_force_replace';
    }.property('node_id'),

    /**
     * When there are no joining nodes, the radio button for selecting
     * a node should be grayed out.  This will put the proper classes
     * on that radio button to gray it out when there are no joining nodes.
     */
    replace_radio_classes: function () {
      return 'gui-radio-wrapper' + (this.get('controller.joiningNodesExist') ? '' : ' semi-transparent');
    }.property('controller.joiningNodesExist'),

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
    /** @scope RiakControl.StagedClusterItemView.prototype */ {

    /* Bindings from the model */

    templateName:       'staged_cluster_item',
    nameBinding:        'content.name',
    statusBinding:      'content.status',
    ring_pctBinding:    'content.ring_pct',
    isReplacedBinding:  'content.isReplaced',
    isActionBinding:    'content.isAction',

    /* Necessary rename to avoid collision */
    node_actionBinding: 'content.action',

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
   * Collection view for showing the staged cluster.
   */
  RiakControl.StagedClusterView = Ember.CollectionView.extend(
    /** @scope RiakControl.StagedClusterView.prototype */ {
    itemViewClass: RiakControl.StagedClusterItemView
  });

});
