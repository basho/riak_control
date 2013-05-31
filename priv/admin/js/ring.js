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
      return this.get('content').filterProperty('allPrimariesDown', true);
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
      return this.get('content').filterProperty('quorumUnavailable', true);
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
    }
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

      var normalizedUnavailable;
      var normalizedPartitions;

      if(partitionCount > 0) {
        normalizedUnavailable = (allUnavailableCount / partitionCount) * 100;
        normalizedPartitions = 100 - normalizedUnavailable;
      } else {
        // Default to all partitions as good until otherwise known.
        normalizedUnavailable = 0;
        normalizedPartitions = 100;
      }

      return [normalizedUnavailable, normalizedPartitions];
    }.property('partitionCount', 'quorumUnavailableCount'),

    id: '#all-unavailable',

    normalizedUnavailable: function() {
      return this.get('data')[0];
    }.property('data'),

    abnormalColor: function() {
      return "#f65d5d";
    }.property(),

    normalColor: function() {
      return "#84ff7e";
    }.property()
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

      var normalizedUnavailable;
      var normalizedPartitions;

      if(partitionCount > 0) {
        normalizedUnavailable = (quorumUnavailableCount / partitionCount) * 100;
        normalizedPartitions = 100 - normalizedUnavailable;
      } else {
        // Default to all partitions as good until otherwise known.
        normalizedUnavailable = 0;
        normalizedPartitions = 100;
      }

      return [normalizedUnavailable, normalizedPartitions];
    }.property('partitionCount', 'quorumUnavailableCount'),

    id: '#quorum-unavailable',

    normalizedUnavailable: function() {
      return this.get('data')[0];
    }.property('data'),

    abnormalColor: function() {
      return "#ffb765";
    }.property(),

    normalColor: function() {
      return "#84ff7e";
    }.property()
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

      return [normalizedDegenerate, normalizedPartitions];
    }.property('partitionCount', 'degenerateCount'),

    id: '#degenerate',

    normalizedDegenerate: function() {
      return this.get('data')[0];
    }.property('data'),

    abnormalColor: function() {
      return "#3baaff";
    }.property(),

    normalColor: function() {
      return "#84ff7e";
    }.property()
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

    allUnavailableBinding:     'content.allUnavailable',
    quorumUnavailableBinding:  'content.quorumUnavailable',

    color: function() {
      var colors = ['partition'];

      var allUnavailable  =   this.get('allUnavailable');
      var quorumUnavailable = this.get('quorumUnavailable');
      var primariesDistinct = this.get('distinct');

      if(allUnavailable) {
        colors.push('red');
      } else if(!primariesDistinct) {
        colors.push('blue');
      } else if(quorumUnavailable) {
        colors.push('orange');
      } else {
        colors.push('green');
      }

      return colors.join(' ');
    }.property('allUnavailable', 'quorumUnavailable', 'distinct')

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

  /**
   * @class
   *
   * Degenerate preflists details view.
   */
  RiakControl.DegeneratePreflistsView = Ember.View.extend(
    /** @scope RiakControl.DegeneratePreflistsView.prototype */ {
    templateName: 'degenerate_preflists'
  });

  /**
   * @class
   *
   * Quorum unavailable details view.
   */
  RiakControl.QuorumUnavailableView = Ember.View.extend({
    templateName: 'quorum_unavailable'
  });

  /**
   * @class
   *
   * All unavailable details view.
   */
  RiakControl.AllUnavailableView = Ember.View.extend(
    /** @scope RiakControl.AllUnavailableView.prototype */ {
    templateName: 'all_unavailable'
  });

});
