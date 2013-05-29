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
    }

  });

  /**
   * @class
   *
   * Container view for the ring page.
   */
  RiakControl.RingView = Ember.View.extend(
    /** @scope RiakControl.RingView.prototype */ {
    templateName: 'ring',

    didInsertElement: function() {
        this.renderChart('degenerate');
        this.renderChart('unavailable');
        this.renderChart('fallbacks');
    },

    renderChart: function(tag) {
      var dataset = {
        apples: [53245, 28479, 19697, 24037, 40245],
        oranges: [200, 200, 200, 200, 200]
      };

      var width = 100,
          height = 100,
          radius = Math.min(width, height) / 2;

      var color = d3.scale.category20();

      var pie = d3.layout.pie().sort(null);

      var arc = d3.svg.arc().
                   innerRadius(radius - 20).
                   outerRadius(radius - 10);

                   console.log(tag);

      var svg = d3.select('#' + tag).append("svg")
          .attr("width", width)
          .attr("height", height)
        .append("g")
          .attr("transform", "translate(" + width / 2 + "," + height / 2 + ")");

      var path = svg.selectAll("path")
          .data(pie(dataset.apples))
        .enter().append("path")
          .attr("fill", function(d, i) { return color(i); })
          .attr("d", arc)
          .each(function(d) { this._current = d; }); // store the initial values
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

    indexBinding: 'content.index',
    n_valBinding: 'content.n_val',
    quorumBinding: 'content.quorum',
    availableBinding: 'content.available',
    distinctBinding: 'content.distinct',

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

    allPrimariesDown: function() {
      var available = this.get('available');

      return available === 0;

    }.property('available'),

    quorumUnavailable: function() {
      var quorum = this.get('quorum');
      var available = this.get('available');

      return available < quorum;

    }.property('quorum', 'available')
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
