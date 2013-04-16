minispade.register('nodes', function() {

  /**
   * @class
   *
   * Node_managementView is responsible for allowing you to stop
   * or down a node.
   */
  RiakControl.NodesView = Ember.View.extend(
    /** @scope RiakControl.ClusterView.prototype */ {
    templateName: 'nodes'
  });

});
