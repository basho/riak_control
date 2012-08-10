minispade.register('cluster', function() {
  RiakControl.ClusterController = Ember.ArrayController.extend();

  RiakControl.ClusterView = Ember.View.extend({
    templateName: 'cluster'
  });
});
