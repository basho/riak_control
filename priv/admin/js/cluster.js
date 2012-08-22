minispade.register('cluster', function() {

  /**
   * @class
   *
   * ClusterController is responsible for display the list of nodes
   * in the cluster.  This controller is basically a placeholder and
   * wrapper around the legacy cluster page until we rewrite it.
   */
  RiakControl.ClusterController = Ember.ArrayController.extend();

  /**
   * @class
   *
   * ClusterView is responsible for display the list of nodes
   * in the cluster.  This controller is basically a placeholder and
   * wrapper around the legacy cluster page until we rewrite it.
   */
  RiakControl.ClusterView = Ember.View.extend(
    /** @scope RiakControl.ClusterView.prototype */ {
    templateName: 'cluster'
  });

});
