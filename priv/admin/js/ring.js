minispade.register('ring', function() {
  RiakControl.PartitionFilter = Ember.Object.extend(),

  RiakControl.Partition = Ember.Object.extend({
    vnodeStatus: function(vnode) {
      var partitionStatus = this.get('status');
      var vnodeReachable = this.get('reachable');
      var vnodeStatus = this.get('vnodes.' + vnode);
      var handoffStatus = this.get('handoffs.' + vnode);

      if(partitionStatus === "incompatible") { return "unknown"; }
      if(handoffStatus) { return "handoff"; }
      if(vnodeStatus === "primary" && vnodeReachable === true) { return "active"; }
      if(vnodeStatus === "fallback") { return "fallback"; }

      return "disabled";
    },

    kvStatus: function() {
      return this.vnodeStatus('riak_kv');
    }.property("vnodes", "handoffs"),

    pipeStatus: function() {
      return this.vnodeStatus('riak_pipe');
    }.property("vnodes", "handoffs"),

    searchStatus: function() {
      return this.vnodeStatus('riak_search');
    }.property("vnodes", "handoffs")
  });

  RiakControl.PartitionFilterController = Ember.ArrayController.extend({
    init: function() {
      this.load();
    },

    load: function() {
      $.ajax({
        url: '/admin/cluster/list',
        dataType: 'json',
        context: this,
        success: function (data) {
          var nodeFilters = data.map(function(item) {
            return RiakControl.PartitionFilter.create({
              type: 'node',
              name: 'Node: ' + item.name,
              value: item.name
            });
          });

          var statusFilters = [
            RiakControl.PartitionFilter.create({
              type: 'vnodes',
              value: 'fallback',
              name: 'Fallback'
            }),
            RiakControl.PartitionFilter.create({
              type: 'handoffs',
              value: '',
              name: 'Handoff'
            })
          ];

          var filters = nodeFilters.concat(statusFilters);
          this.set('content', filters);
        }
      });
    }
  });

  RiakControl.RingController = Ember.ArrayController.extend({
    content: [],

    init: function() {
      this.load();
    },

    load: function() {
      $.ajax({
        url: '/admin/ring/partitions',
        dataType: 'json',
        context: this,
        success: function (data) {
          var partitions = data.partitions.map(function(partition) {
            return RiakControl.Partition.create(partition);
          });

          this.set('content', partitions);
        }
      });
    },

    startInterval: function() {
      this._intervalId = setInterval($.proxy(this.load, this), 1000);
    },

    cancelInterval: function() {
      if(this._intervalId) {
        clearInterval(this._intervalId);
      }
    },

    filteredContent: function() {
      var selectedPartitionFilter = this.get('selectedPartitionFilter');

      if(selectedPartitionFilter) {
        var filterType = selectedPartitionFilter.get('type');
        var filterValue = selectedPartitionFilter.get('value');
        var self = this;
        var filtered;

        if(filterType) {
          if(filterType == 'vnodes') {
            filtered = ['riak_kv', 'riak_search', 'riak_pipe'].map(function(property) {
              return self.get('content').filterProperty('vnodes.' + property, filterValue);
            });

            return filtered.reduce(function(a, b) { return a.concat(b); });
          } else if(filterType == 'handoffs') {
            filtered = ['riak_kv', 'riak_search', 'riak_pipe'].map(function(property) {
              return self.get('content').filter(function(item) {
                if(item.get('handoffs.' + property) === undefined) {
                  return false;
                } else {
                  return true;
                }
              });
            });

            return filtered.reduce(function(a, b) { return a.concat(b); });
          } else {
            return this.get('content').filterProperty(filterType, filterValue);
          }
        } else {
          return this.get('content');
        }
      } else {
        return this.get('content');
      }
    }.property('selectedPartitionFilter', 'content')
  });

  RiakControl.PartitionFilterView = Ember.View.extend({
    templateName: 'partition_filter'
  });

  RiakControl.PartitionFilterSelectView = Ember.Select.extend({
    change: function(ev) {
      var selection = this.get('selection');

      if(selection) {
        RiakControl.get('router').send('filterRing', this.get('selection'));
      } else {
        RiakControl.get('router').send('showRing');
      }
    }
  });

  RiakControl.RingView = Ember.View.extend({
    templateName: 'ring'
  });

  RiakControl.PartitionView = Ember.CollectionView.extend({
    tagName: 'tbody',

    itemViewClass: Ember.View.extend({
      tagName: 'tr',

      classNames: 'partition',

      lightClasses: "gui-light gray",

      toIndicator: function(status) {
        if(status === "unknown") {
          return "red";
        } else if(status === "unreachable") {
          return "red";
        } else if(status === "fallback") {
          return "blue";
        } else if(status === "handoff") {
          return "orange";
        } else if(status === "active") {
          return "green";
        }
      },

      kvStatus: function() {
        return this.get('content.kvStatus');
      }.property("content"),

      kvIndicator: function() {
        return this.toIndicator(this.get("kvStatus"));
      }.property("kvStatus"),

      pipeStatus: function() {
        return this.get('content.pipeStatus');
      }.property("content"),

      pipeIndicator: function() {
        return this.toIndicator(this.get("pipeStatus"));
      }.property("pipeStatus"),

      searchStatus: function() {
        return this.get('content.searchStatus');
      }.property("content"),

      searchIndicator: function() {
        return this.toIndicator(this.get("searchStatus"));
      }.property("searchStatus")
    })
  });
});
