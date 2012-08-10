minispade.register('app', function() {
  RiakControl = Ember.Application.create({
    ready: Ember.alias('initialize')
  });

  RiakControl.ApplicationController = Ember.Controller.extend();

  RiakControl.ApplicationView = Ember.View.extend({
    templateName: 'application'
  });

  RiakControl.Router = Ember.Router.extend({
    root: Ember.Route.extend({
      showSnapshot: Ember.Route.transitionTo('snapshot.index'),

      showCluster: Ember.Route.transitionTo('cluster.index'),

      showRing: Ember.Route.transitionTo('ring.index'),

      filterRing: Ember.Route.transitionTo('ring.filter'),

      index: Ember.Route.extend({
        route: '/',
        redirectsTo: 'snapshot.index'
      }),

      snapshot: Ember.Route.extend({
        route: 'snapshot',

        connectOutlets: function(router) {
          router.get('applicationController').connectOutlet('snapshot');
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
          $.riakControl.markNavActive('nav-cluster');

          $.riakControl.appendScript('#cluster-script', '/admin/ui/js/cluster-legacy.js');
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
          router.get('ringController').connectOutlet('partitionFilter', 'partitionFilter');
          $.riakControl.markNavActive('nav-ring');
        },

        enter: function(router) {
          router.get('ringController').startInterval();
        },

        exit: function(router) {
          router.get('ringController').cancelInterval();
        },

        index: Ember.Route.extend({
          route: '/'
        }),

        filter: Ember.Route.extend({
          route: '/:filterType/:filterValue',

          serialize: function(router, context) {
            return {
              filterType: context.type,
              filterValue: context.value
            };
          },

          deserialize: function(router, params) {
            return RiakControl.PartitionFilter.create({
              type: params.filterType,
              value: params.filterValue
            });
          }
        })
      })
    })
  });

  minispade.require('snapshot');
  minispade.require('cluster');
  minispade.require('ring');
});
