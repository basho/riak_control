minispade.register('ring', function() {

  /**
   * @class
   *
   * Controls filtering, pagination and loading/reloading of the
   * partition list for the cluster.
   */
  RiakControl.RingController = Ember.ObjectController.extend(
    /** @scope RiakControl.RingController.prototype */ {

    /**
     * Reloads the record array associated with this controller.
     *
     * @returns {void}
     */
    reload: function() {
      this.get('content').reload();
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

    /**
     * Count of all partitions.
     *
     * @returns {number}
     */
    partitionCount: function() {
      return this.get('content.length');
    }.property('content.@each'),

    /**
     * Count of degenerate partitions.
     *
     * @returns {number}
     */
    degenerateCount: function() {
      return this.get('content').
        filterProperty('distinct', false).length;
    }.property('content.@each'),

    /**
     * Count of partitions with primaries down.
     *
     * @returns {number}
     */
    allPrimariesDownCount: function() {
      return this.get('content').
        filterProperty('allPrimariesDown', true).length;
    }.property('content.@each'),

    /**
     * Count of partitions with a quorum of primaries down.
     *
     * @returns {number}
     */
    quorumUnavailableCount: function() {
      return this.get('content').
        filterProperty('quorumUnavailable', true).length;
    }.property('content.@each')
  });

  /**
   * @class
   *
   * Container view for the degenerate preflist chart.
   */
  RiakControl.DegeneratePreflistChart = Ember.View.extend(
    /** @scope RiakControl.DegeneratePreflistChart.prototype */ {
    templateName: 'degenerate_preflist_chart',

    classNames: ['chart'],

    partitionCountBinding: 'controller.partitionCount',
    degenerateCountBinding: 'controller.degenerateCount',

    data: function() {
      var partitionCount = this.get('partitionCount');
      var degenerateCount = this.get('degenerateCount');

      var normalizedDegenerate;
      var normalizedPartitions;

      if(partitionCount > 0) {
        normalizedDegenerate = (degenerateCount / partitionCount) * 100;
        normalizedPartitions = 100 - normalizedDegenerate;
      } else {
        // Default to all partitions as good until otherwise known.
        normalizedDegenerate = 0;
        normalizedPartitions = 100;
      }

      console.log('fired');

      return [normalizedDegenerate, normalizedPartitions];
    }.property('partitionCount', 'degenerateCount'),

    width: 100,

    height: 100,

    radius: function() {
      var width = this.get('width');
      var height = this.get('height');

      return Math.min(width, height) / 2;
    }.property('width', 'height'),

    arc: function() {
      var radius = this.get('radius');

      return d3.svg.arc().innerRadius(radius - 20).
                          outerRadius(radius - 10);
    }.property('radius'),

    svg: function() {
      var width = this.get('width');
      var height = this.get('height');

      return d3.select("#degenerate").append("svg").
          attr("width", width).
          attr("height", height).
        append("g").
          attr("transform",
               "translate(" + width / 2 + "," + height / 2 + ")");
    }.property('width', 'height'),

    color: function() {
      return d3.scale.category20();
    }.property(),

    path: function() {
      console.log('rendering path');

      var svg =      this.get('svg');
      var arc =      this.get('arc');
      var pie =      this.get('pie');
      var data =     this.get('data');
      var color =    this.get('color');
      var arcTween = this.get('arcTween');

      var path = svg.selectAll("path").
          data(pie(data)).
        enter().append("path").
          attr("fill", function(d, i) { return color(i); }).
          attr("d", arc).
          each(function(d) { this._current = d; });

      path.transition().duration(750).attrTween("d", arcTween);

      return true;
    }.observes('partitionCount', 'degenerateCount'),

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

    pie: function() {
      return d3.layout.pie().sort(null);
    }.property(),
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
   * View for a single partition.
   */
  RiakControl.PartitionView = Ember.View.extend(
    /** @scope RiakControl.PartitionView.prototype */ {
    templateName: 'partition',

    indexBinding:       'content.index',
    n_valBinding:       'content.n_val',
    quorumBinding:      'content.quorum',
    availableBinding:   'content.available',
    distinctBinding:    'content.distinct',

    allPrimariesDownBinding:   'content.allPrimariesDown',
    quorumUnavailableBinding:  'content.quorumUnavailable',

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
    }.property('allPrimariesDown', 'quorumUnavailable', 'distinct'),

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
