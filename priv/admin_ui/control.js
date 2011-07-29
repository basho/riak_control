controlApp = Sammy('#container', function() {
    $container = $(this.$element);

    this.before(function(){
        $('#main').empty();
    });

    this.use('Template');
    this.use('NestedParams');

    // get top level node information
    this.get('#/nodes', function(context) {

        context.render('nodes.html.template').appendTo('#main');

        $.getJSON("/admin/nodes", function(data) {
            $.each(data.nodes, function(i, item) {
                context.render('node.html.template', {node:item}).appendTo('#nodes');
            });
        });
    });

    // add a new node to the cluster
    this.post('#/addNode', function(context) {
        var newNode = this.params['newNode'];
        var route = this;

        $.ajax({url: '/admin/nodes/'+newNode,
                type: 'PUT',
                contentType: 'application/json',
                success: function() {
                    route.redirect('#/nodes');
                },
                error: function(x, s, e) {
                    //show some kind of error flash
                }});
    });
});

/*
* Bootstrap the application
*/
jQuery(function($) {
  controlApp.run('#/nodes');
});
