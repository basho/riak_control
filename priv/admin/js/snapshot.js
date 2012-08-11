minispade.register('snapshot', function() {
  RiakControl.SnapshotController = Ember.ObjectController.extend({
    init: function() {
      this.load();
    },

    load: function() {
      $.ajax({
        url: '/admin/overview',
        dataType: 'json',
        context: this,
        success: function (data) {
          this.set('content', data);
        }
      });
    },

    startInterval: function() {
      this._intervalId = setInterval($.proxy(this.load, this), 1000);
    },

    cancelInterval: function() {
      if(this._intervalId) {
        clearInterval(this._intervalId);
      }
    },

    unreachableNodes: function() {
      return this.get('content.unreachable_nodes');
    }.property('content'),

    areUnreachableNodes: function() {
      return this.get('unreachableNodes') && this.get('unreachableNodes').length > 0;
    }.property('content'),

    incompatibleNodes: function() {
      return this.get('content.incompatible_nodes');
    }.property('content'),

    areIncompatibleNodes: function() {
      return this.get('incompatibleNodes') && this.get('incompatibleNodes').length > 0;
    }.property('content'),

    downNodes: function() {
      return this.get('content.down_nodes');
    }.property('content'),

    areDownNodes: function() {
      return this.get('downNodes') && this.get('downNodes').length > 0;
    }.property('content'),

    lowMemNodes: function() {
      return this.get('content.low_mem_nodes');
    }.property('content'),

    areLowMemNodes: function() {
      return this.get('lowMemNodes') && this.get('lowMemNodes').length > 0;
    }.property('content'),

    healthyCluster: function() {
      var unreachableNodes = this.get('unreachableNodes'),
          incompatibleNodes = this.get('incompatibleNodes'),
          downNodes = this.get('downNodes'),
          lowMemNodes = this.get('lowMemNodes');

      return unreachableNodes && unreachableNodes.length === 0 &&
        incompatibleNodes && incompatibleNodes.length === 0 &&
        downNodes && downNodes.length === 0 &&
        lowMemNodes && lowMemNodes.length === 0;
    }.property('content')
  });

  RiakControl.SnapshotView = Ember.View.extend({
    templateName: 'snapshot'
  });
});
