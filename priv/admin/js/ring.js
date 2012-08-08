minispade.register('ring', function() {
  RiakControl.Partition = Ember.Object.extend({
    kvStatus: function() {
      return this.get("vnodes.riak_kv");
    }.property("vnodes"),

    pipeStatus: function() {
      return this.get("vnodes.riak_pipe");
    }.property("vnodes"),

    searchStatus: function() {
      var status = this.get("vnodes.riak_search");

      if(status) {
        return status;
      } else {
        return "disabled";
      }
    }.property("vnodes")
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
          this.set('content', data);
        }
      });
    }
  });

  RiakControl.RingController = Ember.ArrayController.extend({
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
      // this._intervalId = setInterval($.proxy(this.load, this), 5000);
    },

    cancelInterval: function() {
      if(this._intervalId) {
        clearInterval(this._intervalId);
      }
    }
  });

  RiakControl.PartitionFilterView = Ember.View.extend({
    templateName: 'partition_filter'
  });

  RiakControl.PartitionFilterSelectView = Ember.Select.extend({
    change: function(ev) {
      Ember.Route.transitionTo('ring.filter', filter);
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
        if( status === "incompatible") {
          return "red";
        } else if(status === "fallback") {
          return "blue";
        } else if(status === "primary") {
          return "green";
        }
      },

      kvStatus: function() {
        return this.get('content.kvStatus');
      }.property('content'),

      kvIndicator: function() {
        return this.toIndicator(this.get("kvStatus"));
      }.property("kvStatus"),

      pipeStatus: function() {
        return this.get('content.pipeStatus');
      }.property('content'),

      pipeIndicator: function() {
        return this.toIndicator(this.get("pipeStatus"));
      }.property("pipeStatus"),

      searchStatus: function() {
        return this.get('content.searchStatus');
      }.property('content'),

      searchIndicator: function() {
        return this.toIndicator(this.get("searchStatus"));
      }.property("searchStatus")
    })
  });
});
