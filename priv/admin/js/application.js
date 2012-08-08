var RiakControl = Ember.Application.create({
  ready: Ember.alias('initialize')
});

RiakControl.ApplicationController = Ember.Controller.extend();

RiakControl.SnapshotController    = Ember.ArrayController.extend();
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
      },

      index: Ember.Route.extend({
        route: '/'
      })
    })
  })
});
