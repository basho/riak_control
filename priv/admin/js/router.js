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
          router.get('applicationController').connectOutlet('snapshot', RiakControl.Node.find());
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

        filterRing: Ember.Route.transitionTo('ring.filtered.index'),

        paginateRing: Ember.Route.transitionTo('paginated'),

        connectOutlets: function(router) {
          router.get('applicationController').connectOutlet('ring', RiakControl.Partition.find());
          router.get('ringController').connectOutlet('partitionFilter', 'partitionFilter', RiakControl.Node.find());
          $.riakControl.markNavActive('nav-ring');
        },

        enter: function(router) {
          router.get('ringController').startInterval();
          router.get('partitionFilterController').startInterval();
        },

        exit: function(router) {
          router.get('ringController').cancelInterval();
          router.get('partitionFilterController').cancelInterval();
        },

        index: Ember.Route.extend({
          route: '/'
        }),

        paginated: Ember.Route.extend({
          route: '/page/:page_id',

          connectOutlets: function(router, context) {
            router.get('ringController').set('selectedPage', context.page_id);
          },

          exit: function(router) {
            router.get('ringController').set('selectedPage', undefined);
          }
        }),

        filtered: Ember.Route.extend({
          route: '/filter/:filterType/:filterValue',

          serialize: function(router, context) {
            if(context) {
              return {
                filterType: context.type,
                filterValue: context.value
              };
            } else {
              return {};
            }
          },

          deserialize: function(router, params) {
            return RiakControl.PartitionFilter.create({
              type: params.filterType,
              value: params.filterValue
            });
          },

          connectOutlets: function(router, context) {
            router.get('ringController').set('selectedPartitionFilter', context);
            router.get('partitionFilterController').set('selectedPartitionFilterValue', context.value);
          },

          exit: function(router) {
            router.get('ringController').set('selectedPartitionFilter', undefined);
            router.get('partitionFilterController').set('selectedPartitionFilterValue', undefined);
          },

          index: Ember.Route.extend({
            route: '/'
          }),

          paginated: Ember.Route.extend({
            route: '/page/:page_id',

            connectOutlets: function(router, context) {
              router.get('ringController').set('selectedPage', context.page_id);
            },

            exit: function(router) {
              router.get('ringController').set('selectedPage', undefined);
            }
          })
        })
      })
    })
  });

});
