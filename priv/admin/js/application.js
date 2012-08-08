var RiakControl = Ember.Application.create({
  ready: Ember.alias('initialize')
});

RiakControl.ApplicationController = Ember.Controller.extend();

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
    this._intervalId = setInterval($.proxy(this.load, this), 500);
  },

  cancelInterval: function() {
    if(this._intervalId) {
      clearTimeout(this._intervalId);
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

RiakControl.ClusterController     = Ember.ArrayController.extend();
RiakControl.RingController        = Ember.ArrayController.extend();
RiakControl.MapReduceController   = Ember.ObjectController.extend();

RiakControl.ApplicationView = Ember.View.extend({
  templateName: 'application'
});

RiakControl.SnapshotView = Ember.View.extend({
  templateName: 'snapshot'
});

RiakControl.ClusterView = Ember.View.extend({
  templateName: 'cluster'
});

RiakControl.RingView = Ember.View.extend({
  templateName: 'ring'
});

RiakControl.MapReduceView = Ember.View.extend({
  templateName: 'mapreduce'
});

RiakControl.Router = Ember.Router.extend({
  root: Ember.Route.extend({
    showSnapshot: Ember.Route.transitionTo('snapshot.index'),
    showCluster: Ember.Route.transitionTo('cluster.index'),
    showRing: Ember.Route.transitionTo('ring.index'),
    showMapReduce: Ember.Route.transitionTo('mapreduce.index'),

    index: Ember.Route.extend({
      route: '/',
      redirectsTo: 'snapshot.index'
    }),

    snapshot: Ember.Route.extend({
      route: 'snapshot',

      connectOutlets: function(router) {
        router.get('applicationController').connectOutlet('snapshot');

        $.riakControl.appendScript('#snapshot-script', '/admin/ui/js/snapshot.js');
        $.riakControl.pub('templateSwitch', ['snapshot']);
        $.riakControl.markNavActive('nav-snapshot');
      },

      enter: function(router) {
        router.get('snapshotController').startInterval();
      },

      exit: function(router) {
        router.get('snapshotController').cancelInterval();
      },

      index: Ember.Route.extend({
        route: '/'
      })
    }),

    cluster: Ember.Route.extend({
      route: 'cluster',

      connectOutlets: function(router) {
        router.get('applicationController').connectOutlet('cluster');

        $.riakControl.appendScript('#cluster-script', '/admin/ui/js/cluster.js');
        $.riakControl.pub('templateSwitch', ['cluster']);
        $.riakControl.markNavActive('nav-cluster');
      },

      index: Ember.Route.extend({
        route: '/'
      })
    }),

    ring: Ember.Route.extend({
      route: 'ring',

      connectOutlets: function(router) {
        router.get('applicationController').connectOutlet('ring');

        $.riakControl.appendScript('#ring-script', '/admin/ui/js/ring.js');
        $.riakControl.pub('templateSwitch', ['ring']);
        $.riakControl.markNavActive('nav-ring');
      },

      index: Ember.Route.extend({
        route: '/'
      })
    }),

    mapreduce: Ember.Route.extend({
      route: 'mapreduce',

      connectOutlets: function(router) {
        router.get('applicationController').connectOutlet('mapReduce');

        $.riakControl.appendScript('#mapreduce-script', '/admin/ui/js/mapreduce.js');
        $.riakControl.pub('templateSwitch', ['mapreduce']);
        $.riakControl.markNavActive('nav-mapreduce');
      },

      index: Ember.Route.extend({
        route: '/'
      })
    })
  })
});
