minispade.register('ring', function() {
  RiakControl.PaginationItemView = Ember.View.extend({
    templateName: 'pagination_item'

    tagName: 'li',
    spanClasses: 'paginator pageNumber',

    isActive: function() {
      var selectedPage = this.get('parentView.controller.selectedPage') || "1";
      var page_id = this.get('content.page_id');

      return selectedPage === page_id;
    }.property('parentView.controller.selectedPage')
  }),

  RiakControl.PaginationView = Ember.CollectionView.extend({
    tagName: 'ul',
    itemViewClass: RiakControl.PaginationItemView
  });

  RiakControl.PartitionFilter = Ember.Object.extend(),

  RiakControl.PartitionFilterController = Ember.ArrayController.extend({
    load: function() {
      this.get('content').reload();
    },

    startInterval: function() {
      this._intervalId = setInterval($.proxy(this.load, this), RiakControl.refreshInterval);
    },

    cancelInterval: function() {
      if(this._intervalId) {
        clearInterval(this._intervalId);
      }
    },

    selectedPartitionFilter: function() {
      var selectedPartitionFilterValue = this.get('selectedPartitionFilterValue');
      var filters = this.get('filters');
      var selectedPartitionFilter;

      selectedPartitionFilter = filters.find(function(item) {
        return item.get('value') === selectedPartitionFilterValue;
      });

      return selectedPartitionFilter;
    }.property('filters', 'selectedPartitionFilterValue'),

    filters: function() {
      var content = this.get('content');

      var nodeFilters = content.map(function(item) {
        return RiakControl.PartitionFilter.create({
          type: 'node',
          name: 'Node: ' + item.get('name'),
          value: item.get('name')
        });
      });

      var statusFilters = [
        RiakControl.PartitionFilter.create({
          type: 'vnodes',
          value: 'fallback',
          name: 'Fallback'
        }),
        RiakControl.PartitionFilter.create({
          type: 'vnodes',
          value: 'handoff',
          name: 'Handoff'
        })
      ];

      return nodeFilters.concat(statusFilters);
    }.property('content.@each')
  });

  RiakControl.RingController = Ember.ArrayController.extend({
    load: function() {
      this.get('content').reload();
    },

    startInterval: function() {
      this._intervalId = setInterval($.proxy(this.load, this), RiakControl.refreshInterval);
    },

    cancelInterval: function() {
      if(this._intervalId) {
        clearInterval(this._intervalId);
      }
    },

    pages: function() {
      var availablePages = this.get('availablePages'),
          pages = [],
          page;

      for (i = 0; i < availablePages; i++) {
        page = i + 1;
        pages.push({ page_id: page.toString() });
      }

      return pages;
    }.property('availablePages'),

    nextPage: function() {
      var availablePages = this.get('availablePages');
      var selectedPage = parseInt(this.get('selectedPage'));
      var pages = this.get('pages');
      var nextPage;

      nextPage = selectedPage + 1;

      if(nextPage > availablePages) {
        nextPage = nextPage - availablePages;
      }

      RiakControl.get('router').send('paginateRing', pages[nextPage - 1]);
    },

    prevPage: function() {
      var availablePages = this.get('availablePages');
      var selectedPage = parseInt(this.get('selectedPage'));
      var pages = this.get('pages');
      var nextPage;

      nextPage = selectedPage - 1;

      if(nextPage <= 0) {
        nextPage = nextPage + availablePages;
      }

      RiakControl.get('router').send('paginateRing', pages[nextPage - 1]);
    },

    availablePages: function() {
      var length = this.get('filteredContent.length');
      var itemsPerPage = 32;

      return (length / itemsPerPage) || 1;
    }.property('filteredContent.length'),

    paginatedContent: function() {
      var filteredContent = this.get('filteredContent');
      var selectedPage = this.get('selectedPage') || 1;

      var itemsPerPage = 32;
      var upperBound = (selectedPage * itemsPerPage);
      var lowerBound = (selectedPage * itemsPerPage) - itemsPerPage;

      return this.get('filteredContent').slice(lowerBound, upperBound);
    }.property('selectedPage', 'filteredContent.@each'),

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
              return self.get('content').filterProperty(property + '_vnode_status', filterValue);
            });

            return filtered.reduce(function(a, b) { return a.concat(b); }).uniq();
          } else {
            return this.get('content').filterProperty(filterType, filterValue);
          }
        } else {
          return this.get('content');
        }
      } else {
        return this.get('content');
      }
    }.property('selectedPartitionFilter', 'content.@each')
  });

  RiakControl.PartitionFilterView = Ember.View.extend({
    templateName: 'partition_filter'
  });

  RiakControl.PartitionFilterSelectView = Ember.Select.extend({
    updateDisplay: function() {
      var val = this.get('controller.selectedPartitionFilter.name');
      $.riakControl.updateDropdown(this.$(), val);
    }.observes('controller.selectedPartitionFilter'),

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
    templateName: 'ring',

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
      }.property("content.kvStatus"),

      kvIndicator: function() {
        return this.toIndicator(this.get("kvStatus"));
      }.property("kvStatus"),

      pipeStatus: function() {
        return this.get('content.pipeStatus');
      }.property("content.pipeStatus"),

      pipeIndicator: function() {
        return this.toIndicator(this.get("pipeStatus"));
      }.property("pipeStatus"),

      searchStatus: function() {
        return this.get('content.searchStatus');
      }.property("content.searchStatus"),

      searchIndicator: function() {
        return this.toIndicator(this.get("searchStatus"));
      }.property("searchStatus")
    })
  });
});
