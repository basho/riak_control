minispade.register('router', function() {

  /**
   * @class
   *
   * Base application router for the RiakControl application.  At this
   * point its responsibilities include either rendering the new pure
   * ember-only components, or using the legacy pub/sub implementation
   * to render older pages which are scheduled for deprecation.
   */
  RiakControl.Router = Ember.Router.extend(
    /** @scope RiakControl.Router.prototype */ {
    root: Ember.Route.extend({
      showSnapshot: Ember.Route.transitionTo('snapshot.index'),

      showCluster: Ember.Route.transitionTo('cluster.index'),

      showNodes: Ember.Route.transitionTo('nodes.index'),

      showRing: Ember.Route.transitionTo('ring.index'),

      index: Ember.Route.extend({
        route: '/',
        redirectsTo: 'snapshot.index'
      }),

      snapshot: Ember.Route.extend({
        route: 'snapshot',

        connectOutlets: function(router) {
          router.get('applicationController').
            connectOutlet('snapshot', RiakControl.Node.find());
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
          router.get('applicationController').connectOutlet('cluster',
            RiakControl.CurrentAndPlannedCluster.create({
              stagedCluster: [], currentCluster: []
            }));
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

      nodes: Ember.Route.extend({
        route: 'nodes',

        connectOutlets: function(router) {
          router.get('applicationController').connectOutlet('nodes', RiakControl.Node.find());
          $.riakControl.markNavActive('nav-nodes');
        },

        enter: function(router) {
          router.get('nodesController').startInterval();
        },

        exit: function(router) {
          router.get('nodesController').cancelInterval();
        },

        index: Ember.Route.extend({
          route: '/'
        })
      }),

      ring: Ember.Route.extend({
        route: 'ring',

        unreachableNodes: Ember.Route.transitionTo('nodes'),

        ownershipHandoffs: Ember.Route.transitionTo('handoffs'),

        connectOutlets: function(router) {
          router.get('applicationController').
            connectOutlet('ring', RiakControl.RingStatus.find());
          router.get('ringController').
            connectOutlet('ringDetails', 'ringDetails', undefined);
          $.riakControl.markNavActive('nav-ring');
        },

        index: Ember.Route.extend({
          route: '/'
        }),

        nodes: Ember.Route.extend({
          route: 'nodes',

          connectOutlets: function(router) {
            router.get('ringController').
              connectOutlet('ringDetails', 'unreachableNodes', RiakControl.Node.find());
          },

          enter: function(router) {
            router.get('unreachableNodesController').startInterval();
          },

          exit: function(router) {
            router.get('unreachableNodesController').startInterval();
          }
        }),

        handoffs: Ember.Route.extend({
          route: 'handoffs',

          connectOutlets: function(router) {
            router.get('ringController').
              connectOutlet('ringDetails', 'handoffs', []);
          }
        })
      })
    })
  });

});
