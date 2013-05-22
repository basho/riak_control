minispade.register('ring', function() {

  /**
   * @class
   *
   * Controls filtering, pagination and loading/reloading of the partition list
   * for the cluster.
   */
  RiakControl.RingController = Ember.ObjectController.extend(
    /** @scope RiakControl.RingController.prototype */ {
  });

  /**
   * @class
   *
   * Container view for the ring page.
   */
  RiakControl.RingView = Ember.View.extend(
    /** @scope RiakControl.RingView.prototype */ {
    templateName: 'ring'
  });

});
