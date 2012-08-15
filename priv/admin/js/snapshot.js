minispade.register('snapshot', function() {
  RiakControl.SnapshotController = Ember.ObjectController.extend({
    load: function() {
      this.get('content').reload();
    },

    startInterval: function() {
      this._intervalId = setInterval($.proxy(this.load, this), 500);
    },

    cancelInterval: function() {
      if(this._intervalId) {
        clearInterval(this._intervalId);
      }
    },

    unreachableNodes: function() {
      return this.get('content').filterProperty('reachable', false);
    }.property('content.@each.reachable'),

    areUnreachableNodes: function() {
      return this.get('unreachableNodes.length') > 0;
    }.property('content.@each.reachable'),

    incompatibleNodes: function() {
      return this.get('content').filterProperty('status', 'incompatible');
    }.property('content.@each.status'),

    areIncompatibleNodes: function() {
      return this.get('incompatibleNodes.length') > 0;
    }.property('content.@each.status'),

    downNodes: function() {
      return this.get('content').filterProperty('status', 'down');
    }.property('content.@each.status'),

    areDownNodes: function() {
      return this.get('downNodes.length') > 0;
    }.property('content.@each.status'),

    lowMemNodes: function() {
      return this.get('content').filterProperty('low_mem', true);
    }.property('content.@each.low_mem'),

    areLowMemNodes: function() {
      return this.get('lowMemNodes.length') > 0;
    }.property('content.@each.low_mem'),

    healthyCluster: function() {
      var areUnreachableNodes = this.get('areUnreachableNodes');
      var areIncompatibleNodes = this.get('areIncompatibleNodes');
      var areDownNodes = this.get('areDownNodes');
      var areLowMemNodes = this.get('areLowMemNodes');

      return !(areUnreachableNodes || areIncompatibleNodes || areDownNodes || areLowMemNodes);
    }.property('content.@each')
  });

  RiakControl.SnapshotView = Ember.View.extend({
    templateName: 'snapshot'
  });
});
