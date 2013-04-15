minispade.register('node_management', function() {

  /**
   * @class
   *
   * Node_managementView is responsible for allowing you to stop
   * or down a node.
   */
  RiakControl.Node_managementView = Ember.View.extend(
    /** @scope RiakControl.ClusterView.prototype */ {
    templateName: 'node_management'
  });

});
