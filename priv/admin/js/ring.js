minispade.register('ring', function() {

  RiakControl.PartitionNValList = Ember.ArrayProxy.extend({});

  RiakControl.SelectedPartitionNValList = Ember.ArrayProxy.extend({
    selectionWatcher: function() {
      var partitions = this.get('partitions').
        findProperty('n_val', parseInt(this.get('selected'))).
          partitions;
      this.setProperties({ content: partitions });
    }.observes('selected'),

    n_vals: function() {

      return this.get('partitions').map(function(x) {
        return x.n_val;
      });
    }.property('partitions.@each')
  });

  /**
   * Creates the n_val dropdown menu.
   */
  RiakControl.NValSelectView = Ember.Select.extend({});

  /**
   * @class
   *
   * Partition represents one of the partitions in the
   * consistent hashing ring owned by the cluster.
   */
  RiakControl.Partition = Ember.Object.extend(
    /** @scope RiakControl.Partition.prototype */ {

    /* Whether unavailable nodes are present. */
    unavailable: function() {
      return this.get('unavailable_nodes').length > 0;
    }.property('unavailable_nodes'),

    /* Whether or not all primaries are down or not. */
    allPrimariesDown: function() {
      return this.get('available') === 0;
    }.property('available'),

    /* Whether or not a quorum of primaries are down. */
    quorumUnavailable: function() {
      return this.get('available') < this.get('quorum');
    }.property('quorum', 'available')
  });

  /**
   * @class
   *
   * Controls filtering, pagination and loading/reloading of the
   * partition list for the cluster.
   */
  RiakControl.RingController = Ember.ObjectController.extend(
    /**
     * Shares properties with RiakControl.NodesController
     */
    RiakControl.ClusterAndNodeControls, {

    /**
     * Refresh the list of partitions, using partitions returned as JSON,
     * and partitions already modeled in Ember.
     *
     * @returns {void}
     */
    refresh: function(newPartitions,
                      existingPartitions,
                      partitionFactory) {

      /*
       * For every object in newPartitions...
       */
      newPartitions.forEach(function(partition) {

        /*
         * Use a unique property to locate the corresponding
         * object in existingPartitions.
         */
        var exists = existingPartitions.findProperty('index',
                                                     partition.index);

        /*
         * If it doesn't exist yet, add it.  If it does, update it.
         */
        if(exists !== undefined) {
          exists.setProperties(partition);
        } else {
          existingPartitions.pushObject(
            partitionFactory.create(partition));
        }
      });

      /*
       * We've already updated corresponding objects and added
       * new ones. Now we need to remove ones that don't exist in
       * the new cluster.
       */

      var changesOccurred = false;
      var replacementPartitions = [];

      /*
       * For every object in the existingPartitions...
       */
      existingPartitions.forEach(function(partition, i) {

        /*
         * Use a unique property to locate the corresponding object
         * in newPartitions.
         */
        var exists = newPartitions.findProperty('index',
                                                partition.index);

        /*
         * If it doesn't exist in the newPartitions, destroy it.
         *
         * If this happens even one time, we'll mark changesOccurred as
         * true.
         *
         * Otherwise, this partition is a good partition and we can add
         * it to the replacementPartitions.
         */
        if(exists === undefined) {
          partition.destroy();
          changesOccurred = true;
        } else {
          replacementPartitions.pushObject(partition);
        }
      });

      /*
       * If we ended up having to remove any partitions,
       * replace the cluster.
       */
      if(changesOccurred) {
        existingPartitions.set('[]', replacementPartitions.get('[]'));
      }
    },

    /**
     * If we have tried and been unable to load data, this will
     * be set to true.
     */
    cannotLoad: function () {
      var content = this.get('content');
      if (!content) {
        return false;
      }
      return !content.content.length;
    }.property('content'),

    /**
     * Load data from the server.
     */
    load: function () {
      var that = this;

      return new Ember.RSVP.Promise(function(resolve, reject) {
        $.ajax({
          type:     'GET',
          url:      '/admin/partitions',
          dataType: 'json'
        }).then(
          // success...
          function (data) {
            Ember.run(function() {
              var curSelected, curPartitions, toRemove, i, content;

              /*
               * Instantiate content if it hasn't been created yet.
               */
              content = that.get('content');
              if (!content) {
                that.set('content', RiakControl.SelectedPartitionNValList.create({
                  content: [],
                  selected: undefined,
                  partitions: RiakControl.PartitionNValList.create({
                    content: []
                  })
                }));
              }

              curSelected = that.get('content.selected');
              curPartitions = that.get('content.partitions');
              toRemove = [];
              i;

              /*
               * Remove any old partition lists that no longer exist
               * within data.partitions.
               */
              curPartitions.forEach(function(hash) {
                if (!data.partitions.findProperty('n_val', hash.n_val)) {
                  hash.partitions.forEach(function (partition) {
                    partition.destroy();
                  });
                  toRemove.push(i)
                }
              });

              toRemove.forEach(function(pIndex) {
                curPartitions.removeAt(pIndex);
              });

              /*
               * Update each partition list.
               */
              data.partitions.forEach(function (hash) {
                var corresponder = curPartitions.findProperty('n_val', hash.n_val);
                if (!corresponder) {
                  corresponder = curPartitions.pushObject({
                    n_val: hash.n_val,
                    partitions: []
                  });
                }
                that.refresh(hash.partitions,
                             corresponder.partitions,
                             RiakControl.Partition);
              });

              /*
               * Manually select a dropdown item on the first ajax call.
               */
              if(that.get('content.selected') === undefined) {
                that.set('content.selected', curSelected || data.default_n_val);
              }
              resolve();
            });
          },

          // error...
          function (jqXHR, textStatus, errorThrown) {
            Ember.run(function() {

              /*
               * Instantiate content if it hasn't been created yet.
               */
              var content = that.get('content');
              if (!content) {
                that.set('content', RiakControl.SelectedPartitionNValList.create({
                  content: [],
                  selected: undefined,
                  partitions: RiakControl.PartitionNValList.create({
                    content: []
                  })
                }));
              }

              if(jqXHR.status === 404 || jqXHR.status === 0) {
                that.get('displayError')
                    .call(that,
                          undefined,
                          undefined,
                          "Partition data is currently unavailable.");
              } else {
                that.get('displayError')
                    .call(that, jqXHR, textStatus, errorThrown);
              }
              reject();
            });
          }
        );
      });
    },

    /**
     * Call the load function.
     */
    reload: function () {
      this.load();
    },

    /**
     * Called by the router, to start polling when this controller/view
     * is navigated to.
     *
     * @returns {void}
     */
    startInterval: function() {
      this._intervalId = setInterval(
          $.proxy(this.reload, this), RiakControl.refreshInterval);
    },

    /**
     * Called by the router, to stop polling when this controller/view
     * is navigated away from.
     *
     * @returns {void}
     */
    cancelInterval: function() {
      if(this._intervalId) {
        clearInterval(this._intervalId);
      }
    },

    /** Currently selected partition. */
    selectedPartition: undefined,

    /**
     * Determine if a partition is currently selected.
     *
     * @returns {boolean}
     */
    partitionSelected: function() {
      return this.get('selectedPartition') !== undefined;
    }.property('selectedPartition'),

    /**
     * Count of all partitions.
     *
     * @returns {number}
     */
    partitionCount: function() {
      return this.get('content.length');
    }.property('content.length'),

    /**
     * Degenerate partitions.
     *
     * @returns {array}
     */
    degenerates: function() {
      return this.get('content').filterProperty('distinct', false);
    }.property('content.@each.distinct'),

    /**
     * Do any partitions have all degenerate preflists?
     *
     * @returns {boolean}
     */
    degeneratesExist: function() {
      return this.get('degenerateCount') > 0;
    }.property('degenerateCount'),

    /**
     * Count of degenerate partitions.
     *
     * @returns {number}
     */
    degenerateCount: function() {
      return this.get('degenerates').length;
    }.property('degenerates'),

    /**
     * Partitions with primaries down.
     *
     * @returns {array}
     */
    allUnavailable: function() {
      return this.get('content').
        filterProperty('allPrimariesDown', true);
    }.property('content.@each.allPrimariesDown'),

    /**
     * Do any partitions have all primaries unreachable?
     *
     * @returns {boolean}
     */
    allUnavailableExist: function() {
      return this.get('allUnavailableCount') > 0;
    }.property('allUnavailableCount'),

    /**
     * Count of partitions with primaries down.
     *
     * @returns {number}
     */
    allUnavailableCount: function() {
      return this.get('allUnavailable').length;
    }.property('allUnavailable'),

    /**
     * Partitions with a quorum of primaries down.
     *
     * @returns {array}
     */
    quorumUnavailable: function() {
      return this.get('content').
        filterProperty('quorumUnavailable', true);
    }.property('content.@each.quorumUnavailable'),

    /**
     * Count of partitions with a quorum of primaries down.
     *
     * @returns {number}
     */
    quorumUnavailableCount: function() {
      return this.get('quorumUnavailable').length;
    }.property('quorumUnavailable'),

    /**
     * Do any partitions have all majority unreachable?
     *
     * @returns {boolean}
     */
    quorumUnavailableExist: function() {
      return this.get('quorumUnavailableCount') > 0;
    }.property('quorumUnavailableCount')
  });


  /**
   * @class
   *
   * Pie chart mixin.
   */
  RiakControl.PieChart = Ember.Mixin.create(
    /** @scope RiakControl.PieChart.prototype */ {

    /**
     * Pie chart dimensions.
     */
    width: 120,
    height: 120,

    /**
     * Radius of the pie chart.
     *
     * @returns {number}
     */
    radius: function() {
      var width = this.get('width');
      var height = this.get('height');

      return Math.min(width, height) / 2;
    }.property('width', 'height'),

    /**
     * Arc rendering function, computed from outer and inner radius.
     *
     * @returns {function}
     */
    arc: function() {
      var radius = this.get('radius');

      return d3.svg.arc().innerRadius(radius - 20).
                          outerRadius(radius - 9);
    }.property('radius'),

    /**
     * Generate an svg, and insert into the DOM.
     *
     * @returns {function}
     */
    svg: function() {
      var id = this.get('id');
      var width = this.get('width');
      var height = this.get('height');

      return d3.select(id).
        append("svg").
          attr("width", width).
          attr("height", height).
        append("g").
          attr("transform",
               "translate(" + width / 2 + "," + height / 2 + ")");
    }.property('width', 'height', 'id'),

    /**
     * Observer which redraws the path components into the svg element
     * as the data changes, while also triggering the motion tween.
     *
     * @returns {true}
     */
    path: function() {
      var svg =      this.get('svg');
      var arc =      this.get('arc');
      var pie =      this.get('pie');
      var data =     this.get('data');
      var arcTween = this.get('arcTween');

      var normalColor =   this.get('normalColor');
      var abnormalColor = this.get('abnormalColor');

      var path = svg.selectAll("path").data(pie(data));

      path.enter().append("path");

      path.attr("fill", function(d, i) {
                  return i === 0 ? abnormalColor : normalColor; }).
          attr("d", arc).
          style("stroke", "rgba(0, 0, 0, .7)").
          style("stroke-width", "2px").
          each(function(d) { this._current = d; });

      path.transition().duration(750).attrTween("d", arcTween);

      return true;
    }.observes('data'),

    /**
     * Tween interpolation function for arcs.
     *
     * @returns {function}
     */
    arcTween: function() {
      var arc = this.get('arc');

      return function(a) {
        var i = d3.interpolate(this._current, a);
        this._current = i(0);
        return function(t) {
          return arc(i(t));
        };
      };
    }.property('arc'),

    /**
     * Generate a pie chart layout.
     *
     * @returns {function}
     */
    pie: function() {
      return d3.layout.pie().sort(null);
    }.property(),

    /**
     * Whenever the view is reinserted into the DOM, re-render
     * the path components into the view.
     *
     * @returns {void}
     */
    didInsertElement: function() {
      // Force rendering when the view is reinserted into the DOM.
      this.path();
    },

    /**
     * Return normalized abnormal amount.
     *
     * @returns {number}
     *
     */
    normalizedAbnormal: function() {
      return this.get('data')[0];
    }.property('data'),

  });

  /**
   * @class
   *
   * Container view for the all unavailable chart.
   */
  RiakControl.AllUnavailableChart = Ember.View.extend(
    RiakControl.PieChart,
    /** @scope RiakControl.AllUnavailableChart.prototype */ {

    templateName: 'all_unavailable_chart',

    classNames: ['chart'],

    partitionCountBinding: 'controller.partitionCount',
    allUnavailableCountBinding: 'controller.allUnavailableCount',

    data: function() {
      var partitionCount = this.get('partitionCount');
      var allUnavailableCount = this.get('allUnavailableCount');

      var normalizedAbnormal;
      var normalizedPartitions;

      if(partitionCount > 0) {
        normalizedAbnormal =
          Math.round((allUnavailableCount / partitionCount) * 100);
        normalizedPartitions = 100 - normalizedAbnormal;
      } else {
        // Default to all partitions as good until otherwise known.
        normalizedAbnormal = 0;
        normalizedPartitions = 100;
      }

      return [normalizedAbnormal, normalizedPartitions];
    }.property('partitionCount', 'quorumUnavailableCount'),

    id: '#all-unavailable',

    abnormalColor: "#f65d5d",

    normalColor: "#84ff7e"

  });

  /**
   * @class
   *
   * Container view for the quorum unavailable chart.
   */
  RiakControl.QuorumUnavailableChart = Ember.View.extend(
    RiakControl.PieChart,
    /** @scope RiakControl.QuorumUnavailableChart.prototype */ {
    templateName: 'quorum_unavailable_chart',

    classNames: ['chart'],

    partitionCountBinding: 'controller.partitionCount',
    quorumUnavailableCountBinding: 'controller.quorumUnavailableCount',

    data: function() {
      var partitionCount = this.get('partitionCount');
      var quorumUnavailableCount = this.get('quorumUnavailableCount');

      var normalizedAbnormal;
      var normalizedPartitions;

      if(partitionCount > 0) {
        normalizedAbnormal =
          Math.round((quorumUnavailableCount / partitionCount) * 100);
        normalizedPartitions = 100 - normalizedAbnormal;
      } else {
        // Default to all partitions as good until otherwise known.
        normalizedAbnormal = 0;
        normalizedPartitions = 100;
      }

      return [normalizedAbnormal, normalizedPartitions];
    }.property('partitionCount', 'quorumUnavailableCount'),

    id: '#quorum-unavailable',

    abnormalColor: "#ffb765",

    normalColor: "#84ff7e"

  });

  /**
   * @class
   *
   * Container view for the degenerate preflist chart.
   */
  RiakControl.DegeneratePreflistChart = Ember.View.extend(
    RiakControl.PieChart,
    /** @scope RiakControl.DegeneratePreflistChart.prototype */ {
    templateName: 'degenerate_preflist_chart',

    classNames: ['chart'],

    partitionCountBinding: 'controller.partitionCount',
    degenerateCountBinding: 'controller.degenerateCount',

    data: function() {
      var partitionCount = this.get('partitionCount');
      var degenerateCount = this.get('degenerateCount');

      var normalizedAbnormal;
      var normalizedPartitions;

      if(partitionCount > 0) {
        normalizedAbnormal =
          Math.round((degenerateCount / partitionCount) * 100);
        normalizedPartitions = 100 - normalizedAbnormal;
      } else {
        // Default to all partitions as good until otherwise known.
        normalizedAbnormal = 0;
        normalizedPartitions = 100;
      }

      return [normalizedAbnormal, normalizedPartitions];
    }.property('partitionCount', 'degenerateCount'),

    id: '#degenerate',

    abnormalColor: "#3baaff",

    normalColor: "#84ff7e"

  });

  /**
   * @class
   *
   * Container view for the ring page.
   */
  RiakControl.RingView = Ember.View.extend(
    /** @scope RiakControl.RingView.prototype */ {
    templateName: 'ring',

    /**
     * Turns the details buttons into toggles that
     * hide and show the actual details section.
     */
    togglifyButtons: function () {

      /*
       * Get the element that was clicked and its
       * corresponding details section.
       */
      var that = $(this),
          info = that.parent().find('.details');

      /*
       * If the details section is currently invisible,
       * add the 'pressed' class to the button and show the
       * details section.
       */
      if (!info.is(':visible')) {
        that.addClass('pressed');
        info.slideDown(250);

      /*
       * Otherwise, remove the class and hide it.
       */
      } else {
        that.removeClass('pressed');
        info.slideUp(250);
      }
    },

    /**
     * After we've rendered the view, set up .togglifyButtons
     * to run whenever we click a details button.
     */
    didInsertElement: function () {
      $('.details-button').on('click', this.get('togglifyButtons'));
    },

    /**
     * If the details buttons get destroyed, we don't need to retain
     * their cached event handlers.
     */
    willDestroyElement: function () {
      $('.details-button').off('click', this.get('togglifyButtons'));
    }
  });

  /**
   * @class
   *
   * View for a single partition.
   */
  RiakControl.PartitionView = Ember.View.extend(
    /** @scope RiakControl.PartitionView.prototype */ {
    templateName: 'partition',

    indexBinding:       'content.index',
    quorumBinding:      'content.quorum',
    availableBinding:   'content.available',
    distinctBinding:    'content.distinct',

    allPrimariesDownBinding:   'content.allPrimariesDown',
    quorumUnavailableBinding:  'content.quorumUnavailable',


    /**
     * When we click on a partition square, display its info
     * in the context box and start it pulsing so we know it
     * is selected.
     */
    click: function () {
      var $this     = this.$(),
          pulsing   = $this.closest('#partition-container').find('.pulse'),
          partition = $this.find('.partition');

      /*
       * If we are clicking on a currently selected partition,
       * we're "turning it off".  Remove its pulse class and
       * set the selectedPartition to undefined.
       */
      if (partition.hasClass('pulse')) {
        pulsing.removeClass('pulse');
        this.get('controller').set('selectedPartition', undefined);

      /*
       * Otherwise, our attempt is to "turn it on". We'll stop any
       * other currently pulsing squares and add the pulse class to this one.
       * Then we set the selectedPartition to this object's content.
       */
      } else {
        pulsing.removeClass('pulse');
        partition.addClass('pulse');
        this.get('controller').set('selectedPartition', this.get('content'));
      }
    },

    /* Return how to colorize the partition.
     *
     * @returns {string}
     */
    color: function() {
      var colors = ['partition'];

      var allPrimariesDown  = this.get('allPrimariesDown');
      var quorumUnavailable = this.get('quorumUnavailable');
      var primariesDistinct = this.get('distinct');

      if(allPrimariesDown) {
        colors.push('red');
      } else if(!primariesDistinct) {
        colors.push('blue');
      } else if(quorumUnavailable) {
        colors.push('orange');
      } else {
        colors.push('green');
      }

      return colors.join(' ');
    }.property('allPrimariesDown', 'quorumUnavailable', 'distinct')

  });

  /**
   * @class
   *
   * Collection view for partitions.
   */
  RiakControl.PartitionsView = Ember.CollectionView.extend(
    /** @scope RiakControl.PartitionsView.prototype */ {
    itemViewClass: RiakControl.PartitionView
  });

});
