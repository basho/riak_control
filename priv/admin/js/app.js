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

  // Automatically add CSRF tokens to AJAX requests.
  $("body").bind("ajaxSend", function(elm, xhr, s){
    var csrf_token = $('meta[name=csrf_token]').attr('content');

    if(s.type === 'POST' || s.type === 'PUT') {
      xhr.setRequestHeader('X-CSRF-Token', csrf_token);
    }
  });

  // Set default content-type for AJAX requests.
  // From: http://stackoverflow.com/questions/1749272/jquery-how-to-put-json-via-ajax
  $.ajaxSetup({
      contentType: 'application/json',
      processData: false
  });

  // Prefilter for stringifying.
  // From: http://stackoverflow.com/questions/1749272/jquery-how-to-put-json-via-ajax
  $.ajaxPrefilter(function(options, originalOptions, jqXHR) {
    if(options.data) {
      options.data = JSON.stringify(options.data);
    }
  });

  minispade.require('core');
  minispade.require('router');
  minispade.require('shared');
  minispade.require('snapshot');
  minispade.require('cluster');
  minispade.require('nodes');
  minispade.require('ring');

});
