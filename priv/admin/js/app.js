minispade.register('app', function() {
  RiakControl = Ember.Application.create({
    ready: Ember.alias('initialize')
  });

  RiakControl.ApplicationController = Ember.Controller.extend();

  RiakControl.ClusterController = Ember.ArrayController.extend({
    init: function() {
      this.load();
    },

    load: function() {
      $.ajax({
        url: '/admin/cluster/list',
        dataType: 'json',
        context: this,
        success: function (data) {
          this.set('content', data);
        }
      });
    },

    startInterval: function() {
      this._intervalId = setInterval($.proxy(this.load, this), 5000);
    },

    cancelInterval: function() {
      if(this._intervalId) {
        clearInterval(this._intervalId);
      }
    }
  });

  RiakControl.MapReduceController = Ember.ObjectController.extend();

  RiakControl.ApplicationView = Ember.View.extend({
    templateName: 'application'
  });

  RiakControl.ClusterView = Ember.View.extend({
    templateName: 'cluster'
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

        enter: function(router) {
          router.get('clusterController').startInterval();
        },

        exit: function(router) {
          router.get('clusterController').cancelInterval();
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
          route: '/:filter'
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

  minispade.require('snapshot');
  minispade.require('ring');
});
