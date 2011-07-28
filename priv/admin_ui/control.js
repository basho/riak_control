// node model
function node(name, cluster) {
    this.name = name;
    this.remove = function() { cluster.removeNode(name) };
    this.select = function() { cluster.selectNode(name) };
    this.deselect = function() { cluster.selectNode("") };
}

// cluster model
function cluster() {
    var self = this;
    this.nodes = ko.observableArray([]);
    this.newNodeName = ko.observable();
    this.message = ko.observable();
    this.selectedNodeName = ko.observable();

    this.clearMessage = function() {
        this.message("");
    }

    this.selectNode = function(node) {
        this.selectedNodeName(node);
    }

    // remove a node from the cluster
    this.removeNode = function(node) {
        $.ajax({url: '/admin/nodes/' + node,
                type: 'DELETE',
                success: function() {
                    // show some kind of success flash
                    self.message("Sent remove request for " + node);
                },
                error: function(x, s, e) {
                    // show some kind of error flash
                    self.message(s + ". " + e + ". " + x.responseText);
                }
        });
    }

    // add a node to the cluster
    this.addNode = function() {
        newNode = this.newNodeName();
        $.ajax({url: '/admin/nodes/' + this.newNodeName(),
                type: 'PUT',
                contentType: 'application/json',
                success: function() {
                    // show some kind of success flash
                    self.newNodeName("");
                    self.message("Sent add request for " + newNode );
                    $("#newNode").blur();
                },
                error: function(x, s, e) {
                    //show some kind of error flash
                    self.message(s + ". " + e + ". " + x.responseText);
                }});
    }

    // get a list of nodes in the cluster
    this.getNodes = function() {
        $.getJSON("/admin/nodes", function(data) {
            currentNodes = [];
            $.each(data.nodes, function(i, name) {
                currentNodes.push(new node(name, self));
            });
            self.mergeNodes(currentNodes);
        });
        setTimeout(self.getNodes, 5000);
    }

    // sync the model view with the last gotten view
    // gives a smoother updating display since the model isn't
    // fully rebuilt per get (less flicker)
    // pretty ineffecient, but I doubt we are dealing with 10,000 node clusters
    this.mergeNodes = function(nodeList) {
        // remove and node in the model that is not in the latest get result
        _.each(self.nodes(), function(inModel) {
            if(!_.detect(nodeList, function(inNodeList) {
                return inNodeList.name == inModel.name;
            })) {
                self.nodes.remove(function(n) { return n.name == inModel.name;});
            }
        });

        // add any node not in the model that is the latest get result
        _.each(nodeList, function(inNodeList) {
            if(!_.detect(self.nodes(), function(inModel) { return inModel.name == inNodeList.name;})) {
                self.nodes.push(inNodeList);
            }
        });
    }

    // init by grabbing nodes
    this.getNodes();
  }

/*
* Bootstrap the application
*/
jQuery(function($) {
    cluster = new cluster();

    cluster.selectedNode = ko.dependentObservable(function() {
        var nodeToFind = this.selectedNodeName();
        return ko.utils.arrayFirst(cluster.nodes(), function(n) { return n.name == nodeToFind });
    }, cluster);

    ko.applyBindings(cluster);
    ko.linkObservableToUrl(cluster.selectedNodeName, "node", "");
});