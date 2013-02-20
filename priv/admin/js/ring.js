minispade.register('ring', function() {

  /**
   * @class
   *
   * PaginationItemView handles display of individual pagination links
   * used as navigation through the partition list.
   */
  RiakControl.PaginationItemView = Ember.View.extend(
    /** @scope RiakControl.PaginationItemView.prototype */ {
    templateName: 'pagination_item',

    tagName: 'li',
    spanClasses: 'paginator pageNumber',

    /**
     * Determine if this is the currently selected page.
     *
     * @returns {Boolean}
     */
    isActive: function() {
      var currentPage = this.get('parentView.controller.currentPage');
      var page_id = this.get('content.page_id');

      if(currentPage) {
        return currentPage.toString() === page_id.toString();
      } else {
        return false;
      }
    }.property('parentView.controller.currentPage')
  });

  /**
   * @class
   *
   * PartitionFilter provides a model for filters that will be applied to the
   * partition list.  Each filter contains a type it filters by and a value, such
   * as vnodes/handoff, node/dev@127.0.0.1, etc.
   */
  RiakControl.PartitionFilter = Ember.Object.extend();

  /**
   * @class
   *
   * Responsible for filtering the partition list.
   */
  RiakControl.PartitionFilterController = Ember.ArrayController.extend(
    /** @scope RiakControl.PartitionFilterController.prototype */ {

    /**
     * Reload the recordarray from the server.
     *
     * @returns {void}
     */
    reload: function() {
      this.get('content').reload();
    },

    /**
     * Called by the router to start the polling interval when the page is selected.
     *
     * @returns {void}
     */
    startInterval: function() {
      this._intervalId = setInterval($.proxy(this.reload, this), RiakControl.refreshInterval);
    },

    /**
     * Called by the router to stop the polling interval when the page is navigated
     * away from.
     *
     * @returns {void}
     */
    cancelInterval: function() {
      if(this._intervalId) {
        clearInterval(this._intervalId);
      }
    },

    /**
     * Given a list of filters and a selected filter value, return the filtering object.
     *
     * This is a necessary evil because we async load the partition list when someone directly
     * links to a filtered view.  This function will fire once the list of filters are loaded,
     * and correctly return the filter object given an id deserialized by the router upon navigation.
     *
     * @returns {Object}
     */
    selectedPartitionFilter: function() {
      var selectedPartitionFilterValue = this.get('selectedPartitionFilterValue');
      var filters = this.get('filters');
      var selectedPartitionFilter;

      selectedPartitionFilter = filters.find(function(item) {
        return item.get('value') === selectedPartitionFilterValue;
      });

      return selectedPartitionFilter;
    }.property('filters', 'selectedPartitionFilterValue'),

    /**
     * Given a list of nodes retrieved from the server async, convert to a list of filters
     * for the user interface, while injecting some default filters, such as by various vnode
     * statuses.
     *
     * @returns {Array}
     */
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

  /**
   * @class
   *
   * Controls filtering, pagination and loading/reloading of the partition list
   * for the cluster.
   */
  RiakControl.RingController = Ember.ArrayController.extend(
    /** @scope RiakControl.RingController.prototype */ {

    /**
     * Reload the recordarray from the server.
     *
     * @returns {void}
     */
    reload: function() {
      this.get('content').reload();
    },

    /**
     * Called by the router to start polling and reloading of partition list when route is entered.
     *
     * @returns {void}
     */
    startInterval: function() {
      this._intervalId = setInterval($.proxy(this.reload, this), RiakControl.refreshInterval);
    },

    /**
     * Called by the router to stop polling when route is left.
     *
     * @returns {void}
     */
    cancelInterval: function() {
      if(this._intervalId) {
        clearInterval(this._intervalId);
      }
    },

    /**
     * Given a known number of available pages, generate an array of objects which
     * can be used by the UI for filtering
     *
     * @returns {Array}
     */
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

    /**
     * Returns the currently selected page as Integer.
     *
     * @returns {Integer}
     */
    currentPage: function() {
      return this.get('selectedPage') || 1;
    }.property('selectedPage'),

    /**
     * Handle the nextPage event.  Call to the router to advance to the appropriate
     * page.
     *
     * @returns {void}
     */
    nextPage: function() {
      var availablePages = this.get('availablePages');
      var currentPage = parseInt(this.get('currentPage'), 10);
      var pages = this.get('pages');
      var nextPage;

      nextPage = currentPage + 1;

      if(nextPage > availablePages) {
        nextPage = nextPage - availablePages;
      }

      RiakControl.get('router').send('paginateRing', pages[nextPage - 1]);
    },

    /**
     * Handle the prevPage event.  Call to the router to advance to the appropriate
     * page.
     *
     * @returns {void}
     */
    prevPage: function() {
      var availablePages = this.get('availablePages');
      var currentPage = parseInt(this.get('currentPage'), 10);
      var pages = this.get('pages');
      var nextPage;

      nextPage = currentPage - 1;

      if(nextPage <= 0) {
        nextPage = nextPage + availablePages;
      }

      RiakControl.get('router').send('paginateRing', pages[nextPage - 1]);
    },

    /**
     * Determine the number of available pages for pagination.
     *
     * @returns {Number}
     */
    availablePages: function() {
      var length = this.get('filteredContent.length');
      var itemsPerPage = 32;

      return (length / itemsPerPage) || 1;
    }.property('filteredContent.length'),

    /**
     * Returns filtered content, paginated based on the currently applied
     * selectedPage property
     *
     * @returns {Array}
     */
    paginatedContent: function() {
      var filteredContent = this.get('filteredContent');
      var selectedPage = this.get('selectedPage') || 1;

      var itemsPerPage = 32;
      var upperBound = (selectedPage * itemsPerPage);
      var lowerBound = (selectedPage * itemsPerPage) - itemsPerPage;

      return this.get('filteredContent').slice(lowerBound, upperBound);
    }.property('selectedPage', 'filteredContent.@each'),

    /**
     * Returns content filtered based on the currently applied 
     * selectedPartitionFilter property.
     *
     * @returns {Array}
     */
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

  /**
   * @class
   *
   * View containing a select field and wrapper for filtering content.
   */
  RiakControl.PartitionFilterView = Ember.View.extend(
    /** @scope RiakControl.PartitionFilterView.prototype */ {
    templateName: 'partition_filter'
  });

  /**
   * @class
   *
   * View rendering the select box for filtering, handling on change
   * events and updating of the div wrapping it.
   */
  RiakControl.PartitionFilterSelectView = Ember.Select.extend(
    /** @scope RiakControl.PartitionFilterSelectView.prototype */ {

    /**
     * As the select is currently wrapped in a span for prettyfying the display,
     * as the selected value updates, update the display.
     *
     * @returns {void}
     */
    updateDisplay: function() {
      var val  = this.get('controller.selectedPartitionFilter.name'),
          item = this.$();
      Ember.run.next(function() {
        item.prev().prev().text(val);
      });
    }.observes('controller.selectedPartitionFilter'),

    /**
     * When the value changes, call to the router to navigate to the appropriate
     * filtered view.
     *
     * @returns {void}
     */
    change: function(ev) {
      var selection = this.get('selection');

      if(selection) {
        RiakControl.get('router').send('filterRing', this.get('selection'));
      } else {
        RiakControl.get('router').send('showRing');
      }
    }
  });

  /**
   * @class
   *
   * Container view for the ring page.
   */
  RiakControl.RingView = Ember.View.extend(
    /** @scope RiakControl.RingView.prototype */ {
    templateName: 'ring'
  });

  /**
   * @class
   *
   * PartitionView is a collection view for wrapping and rendering the
   * partition list.
   */
  RiakControl.PartitionView = Ember.CollectionView.extend(
    /** @scope RiakControl.PartitionView.prototype */ {
    tagName: 'tbody',

    itemViewClass: Ember.View.extend({
      tagName: 'tr',

      classNames: 'partition',

      lightClasses: "gui-light gray",

      /**
       * Given a textual status, return the appropriate color to be used for rendering
       * the indicator lights.
       *
       * @param {String} status
       *  Status in text.
       *
       * @returns {String}
       */
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

      /**
       * Binding for the kv vnode status field.
       *
       * @returns {String}
       */
      kvStatus: function() {
        return this.get('content.kvStatus');
      }.property("content.kvStatus"),

      /**
       * Binding for the kv vnode indicator light.
       *
       * @returns {String}
       */
      kvIndicator: function() {
        return this.toIndicator(this.get("kvStatus"));
      }.property("kvStatus"),

      /**
       * Binding for the pipe vnode status field.
       *
       * @returns {String}
       */
      pipeStatus: function() {
        return this.get('content.pipeStatus');
      }.property("content.pipeStatus"),

      /**
       * Binding for the pipe vnode indicator light.
       *
       * @returns {String}
       */
      pipeIndicator: function() {
        return this.toIndicator(this.get("pipeStatus"));
      }.property("pipeStatus"),

      /**
       * Binding for the search vnode status field.
       *
       * @returns {String}
       */
      searchStatus: function() {
        return this.get('content.searchStatus');
      }.property("content.searchStatus"),

      /**
       * Binding for the search vnode indicator light.
       *
       * @returns {String}
       */
      searchIndicator: function() {
        return this.toIndicator(this.get("searchStatus"));
      }.property("searchStatus")
    })
  });

});
