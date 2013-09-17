minispade.register('app', function() {

  /**
   * RiakControl Ember application.
   */
  RiakControl = Ember.Application.create();

  /**
   * How often to refresh/poll the server for changes in cluster
   * and partition status
   */
  RiakControl.refreshInterval = 1000;

  /**
   * Provide reload functionality for recordarrays.
   */
  DS.RecordArray.reopen({
    reload: function() {
      Ember.assert("Can only reload base RecordArrays",
        this.constructor === DS.RecordArray);
      var store = this.get('store');
      store.findAll(this.get('type'));
    }
  });

  /**
   * Data store configuration.
   */
  RiakControl.Store = DS.Store.extend({
    adapter: DS.RESTAdapter.reopen({ namespace: 'admin' })
  });

  RiakControl.store = RiakControl.Store.create();

  // Automatically add CSRF tokens to AJAX requests.
  $(document).ajaxSend(function(elm, xhr, s){
    var csrf_token = $('meta[name=csrf_token]').prop('content');

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
