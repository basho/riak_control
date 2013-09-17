minispade.register('router', function() {

  /**
   * @class
   *
   * Base application router for the RiakControl application.  At this
   * point its responsibilities include either rendering the new pure
   * ember-only components, or using the legacy pub/sub implementation
   * to render older pages which are scheduled for deprecation.
   */
  RiakControl.Router.map(function() {
    /** @scope Ember.RouterDSL callback */

    this.route('snapshot');
    this.route('cluster');
    this.route('nodes');
    this.route('ring');
  });

  RiakControl.IndexRoute = Ember.Route.extend(
    /** @scope Ember.Route.prototype */ {

    redirect: function() {
      this.transitionTo('snapshot');
    }
  });

  RiakControl.SnapshotRoute = Ember.Route.extend(
    /** @scope Ember.Route.prototype */ {

    model: function() {
      return this.store.find('node');
    },

    renderTemplate: function() {
      this.render('snapshot');
      $.riakControl.markNavActive('nav-snapshot');
    },

    activate: function() {
      this.controllerFor('snapshot').startInterval();
    },

    deactivate: function() {
      this.controllerFor('snapshot').cancelInterval();
    }
  });

  RiakControl.ClusterRoute = Ember.Route.extend(
    /** @scope Ember.Route.prototype */ {

    model: function() {
      var cluster = this.controllerFor('cluster');

      /*
       * Don't return a promise if we have previously
       * failed to load data.
       */
      if (cluster.get('cannotLoad') === true) {
        return cluster.get('content');

      } else {
        return cluster.load().then(function (d) {
          return d;
        });
      }
    },

    /*
     * In the event of an error, the cannotLoad property
     * will be set to true. This way we can still
     * transition into the state without getting caught
     * in an infinit .load() loop.
     */
    events: {
      error: function () {
        this.transitionTo('cluster');
      }
    },

    renderTemplate: function() {
      this.render('cluster');
      $.riakControl.markNavActive('nav-cluster');
    },

    activate: function() {
      this.controllerFor('cluster').startInterval();
    },

    deactivate: function() {
      this.controllerFor('cluster').cancelInterval();
    }
  });

  RiakControl.NodesRoute = Ember.Route.extend(
    /** @scope Ember.Route.prototype */ {

    model: function() {
      return this.store.find('node');
    },

    renderTemplate: function() {
      this.render('nodes');
      $.riakControl.markNavActive('nav-nodes');
    },

    activate: function() {
      this.controllerFor('nodes').startInterval();
    },

    deactivate: function() {
      this.controllerFor('nodes').cancelInterval();
    }
  });

  RiakControl.RingRoute = Ember.Route.extend(
    /** @scope Ember.Route.prototype */ {

    model: function() {
      var ring = this.controllerFor('ring');

      /*
       * Don't return a promise if we have previously
       * failed to load data.
       */
      if (ring.get('cannotLoad') === true) {
        return ring.get('content');

      } else {
        return ring.load().then(function (d) {
          return d;
        });
      }
    },

    /*
     * In the event of an error, the cannotLoad property
     * will be set to true. This way we can still
     * transition into the state without getting caught
     * in an infinit .load() loop.
     */
    events: {
      error: function () {
        this.transitionTo('ring');
      }
    },

    renderTemplate: function() {
      this.render('ring');
      $.riakControl.markNavActive('nav-ring');
    },

    activate: function() {
      this.controllerFor('ring').startInterval();
    },

    deactivate: function() {
      this.controllerFor('ring').cancelInterval();
    }
  });

  RiakControl.LoadingRoute = Ember.Route.extend({});
});
