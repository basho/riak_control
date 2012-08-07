var RiakControl = Ember.Application.create({
  ready: Ember.alias('initialize')
});

RiakControl.ApplicationController = Ember.Controller.extend();

RiakControl.SnapshotController = Ember.ArrayController.extend();
RiakControl.ClusterController  = Ember.ArrayController.extend();
RiakControl.RingController     = Ember.ArrayController.extend();

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

RiakControl.Router = Ember.Router.extend({
  root: Ember.Route.extend({
    index: Ember.Route.extend({
      route: '/',
      redirectsTo: 'snapshot.index'
    }),

    snapshot: Ember.Route.extend({
      route: 'snapshot',

      connectOutlets: function(router) {
        router.get('applicationController').connectOutlet('snapshot');
      },

      index: Ember.Route.extend({
        route: '/'
      })
    }),

    cluster: Ember.Route.extend({
      route: 'cluster',

      connectOutlets: function(router) {
        router.get('applicationController').connectOutlet('cluster');
      },

      index: Ember.Route.extend({
        route: '/'
      })
    }),

    ring: Ember.Route.extend({
      route: 'ring',

      connectOutlets: function(router) {
        router.get('applicationController').connectOutlet('ring');
      },

      index: Ember.Route.extend({
        route: '/'
      })
    })
  })
});
