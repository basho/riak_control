minispade.register('app', function() {

  /**
   * RiakControl Ember application.
   */
  RiakControl = Ember.Application.create({
    ready: Ember.alias('initialize')
  });

  /**
   * How often to refresh/poll the server for changes in cluster
   * and partition status
   */
  RiakControl.refreshInterval = 1000;

  RiakControl.ApplicationController = Ember.Controller.extend();

  RiakControl.ApplicationView = Ember.View.extend({
    templateName: 'application'
  });

  DS.Model.reopen({
    reload: function() {
        var store = this.get('store');
        store.get('adapter').find(store, this.constructor, this.get('id'));
      }
  });

  DS.RecordArray.reopen({
    reload: function() {
        Ember.assert("Can only reload base RecordArrays", this.constructor === DS.RecordArray);
        var store = this.get('store');
        store.get('adapter').findAll(store, this.get('type'));
      }
  });

  RiakControl.Store = DS.Store.extend({
    revision: 4,
    adapter: DS.RESTAdapter.create({ namespace: 'admin' })
  });

  RiakControl.store = RiakControl.Store.create();

  minispade.require('core');
  minispade.require('router');
  minispade.require('snapshot');
  minispade.require('cluster');
  minispade.require('ring');

});
