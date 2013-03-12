minispade.register('cluster', function() {

  var SAMPLE_DATA =
      {"cluster":[
        {"name":"dev1@127.0.0.1","status":"valid","reachable":true,"ring_pct":0.5,"mem_total":7370916000,"mem_used":7036208000,"mem_erlang":21245544,"low_mem":true,"me":true},
        {"name":"dev2@127.0.0.1","status":"valid","reachable":false,"ring_pct":0.5,"mem_total":"undefined","mem_used":"undefined","mem_erlang":"undefined","low_mem":false,"me":false},
        {"name":"dev3@127.0.0.1","status":"joining","reachable":false,"ring_pct":0.0,"mem_total":"undefined","mem_used":"undefined","mem_erlang":"undefined","low_mem":false,"me":false}
      ]};


  RiakControl.CurrentClusterNode = Ember.Object.extend({});

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
  RiakControl.ClusterController = Ember.ObjectController.extend({
    init: function() {
      var currentClusterNodes = SAMPLE_DATA.cluster.map(function(i) {
        return RiakControl.CurrentClusterNode.create(i);
      });

      this.set('content', { cluster: currentClusterNodes, plan: undefined });
    },

    isLoading: function () {
      return false;
    }.property()
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

  RiakControl.CurrentClusterToggleView = Ember.View.extend({
    click: function () {
      var prop = this.get('parentView.expanded');
      if (prop) {
        this.get('parentView').set('expanded', false);
      } else {
        this.get('parentView').set('expanded', true);
      }
    }
  });

  RiakControl.CurrentClusterItemView = Ember.View.extend({
    templateName: 'current_cluster_item',

    nameBinding: 'content.name',

    reachableBinding: 'content.reachable',

    statusBinding: 'content.status',

    ring_pctBinding: 'content.ring_pct',

    mem_totalBinding: 'content.mem_total',

    mem_usedBinding: 'content.mem_used',

    mem_erlangBinding: 'content.mem_erlang',

    classNameBindings: ['expanded:open'],

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

    mem_divider: function() {
      return this.get('mem_total') / 100;
    }.property('mem_total'),

    mem_erlang_ceil: function () {
      return Math.ceil(this.get('mem_erlang') / this.get('mem_divider'));
    }.property('mem_erlang', 'mem_divider'),

    mem_non_erlang: function () {
      return Math.round((this.get('mem_used') / this.get('mem_divider')) - this.get('mem_erlang_ceil'));
    }.property('mem_used', 'mem_divider', 'mem_erlang_ceil'),

    mem_free: function () {
      return this.get('mem_total') - this.get('mem_used');
    }.property('mem_total', 'mem_used'),

    mem_free_readable: function () {
      return Math.round(this.get('mem_free') / this.get('mem_divider'));
    }.property('mem_free', 'mem_divider'),

    mem_used_readable: function () {
      return Math.round((this.get('mem_total') - this.get('mem_free')) / this.get('mem_divider'));
    }.property('mem_total', 'mem_free', 'mem_divider'),

    mem_erlang_style: function () {
      return 'width: ' + this.get('mem_erlang_ceil') + '%';
    }.property('mem_erlang_ceil'),

    mem_non_erlang_style: function () {
      return 'width: ' + this.get('mem_non_erlang') + '%';
    }.property('mem_non_erlang'),

    mem_free_style: function () {
      return 'width: ' + this.get('mem_free_readable') + '%';
    }.property('mem_free_readable'),

    ring_pct_readable: function () {
      return this.get('ring_pct') * 100;
    }.property('ring_pct')

  });


  RiakControl.CurrentClusterView = Ember.CollectionView.extend({
    itemViewClass: RiakControl.CurrentClusterItemView
  });

});
