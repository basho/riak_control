// node model
function node(name, cluster) {
    var self = this;
    this.name = name;
    this.cluster = cluster;
    this.services = ko.observableArray([]);
    this.remove = function() { cluster.removeNode(name) };
    this.select = function() { cluster.selectNode(name) };
    this.deselect = function() { cluster.selectNode("") };

    this.getServices = function() {
        $.ajax({url: "/admin/nodes/" + self.name + "/services",
                type: "GET",
                success: function(data) {
                    $.each(data.services, function(i, s) {
                        self.services.push(new service(s.name, s.initial));
                    });
                    self.services.sort(function(l, r) { return l.initial == r.initial ? 0 : (l.initial < r.initial ? -1 : 1 ) });
                },
                error: function(x, s, e) {
                    //
                }
         });
    }
}

// partition model
function partition(idx, node, vnodes, cluster) {
    this.idx = idx;
    this.node = node;
    this.vnodes = vnodes;
    this.cluster = cluster;
}

// service model
function service(name, initial) {
    this.name = name;
    this.initial = initial;
}

// cluster model
function cluster() {
    var self = this;
    this.nodes = ko.observableArray([]);
    this.partitions = ko.observableArray([]);
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
                    self.message("Sent remove request for " + node);
                },
                error: function(x, s, e) {
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
                    self.newNodeName("");
                    self.message("Sent add request for " + newNode );
                    $("#newNode").blur();
                },
                error: function(x, s, e) {
                    self.message(s + ". " + e + ". " + x.responseText);
                }});
    }

    // get a list of nodes in the cluster
    this.getNodes = function() {
        $.getJSON("/admin/nodes", function(data) {
            currentNodes = [];
            self.partitions([]);
           
            $.each(data.nodes, function(i, name) {
                currentNodes.push(new node(name, self));
            });
            
            $.each(data.partitions, function(i, p) {
                self.partitions.push(new partition(i, p.owner, p.vnodes, self));
            });

            self.mergeNodes(currentNodes);
        });
        // setTimeout(self.getNodes, 10000); take this out until I find a better way than mergeNodes below
    }

    // sync the model view with the last gotten view
    // gives a smoother updating display since the model isn't
    // fully rebuilt per get (less flicker)
    // pretty ineffecient, but I doubt we are dealing with 10,000 node clusters
    this.mergeNodes = function(nodeList) {
        // remove any node in the model that is not in the latest get result
        _.each(self.nodes(), function(inModel) {
            if(!_.detect(nodeList, function(inNodeList) {
                return inNodeList.name == inModel.name;
            })) {
                self.nodes.remove(function(n) { return n.name == inModel.name;});
            }
        });

        // add any node not in the model that is in the latest get result
        _.each(nodeList, function(inNodeList) {
            if(!_.detect(self.nodes(), function(inModel) { return inModel.name == inNodeList.name;})) {
                inNodeList.getServices();
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