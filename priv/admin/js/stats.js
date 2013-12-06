minispade.register('stats', function() {

  /**
   * @class
   *
   * Content for the add graph dropdown menu.
   */
  RiakControl.AddGraphSelectView = Ember.Select.extend({
    content: [
      '-- Choose a Statistic --',
      'KV - cpu_avg1',
      'KV - cpu_avg5',
      'KV - cpu_avg15',
      'KV - cpu_nprocs',
      'KV - node_get_fsm_active',
      'KV - node_get_fsm_active_60s',
      'KV - node_get_fsm_in_rate',
      'KV - node_get_fsm_objsize_95',
      'KV - node_get_fsm_objsize_99',
      'KV - node_get_fsm_objsize_100',
      'KV - node_get_fsm_objsize_mean',
      'KV - node_get_fsm_objsize_median',
      'KV - node_get_fsm_out_rate',
      'KV - node_get_fsm_rejected',
      'KV - node_get_fsm_rejected_60s',
      'KV - node_get_fsm_rejected_total',
      'KV - node_get_fsm_siblings_95',
      'KV - node_get_fsm_siblings_99',
      'KV - node_get_fsm_siblings_100',
      'KV - node_get_fsm_siblings_mean',
      'KV - node_get_fsm_siblings_median',
      'KV - node_get_fsm_time_95',
      'KV - node_get_fsm_time_99',
      'KV - node_get_fsm_time_100',
      'KV - node_get_fsm_time_mean',
      'KV - node_get_fsm_time_median',
      'KV - node_gets',
      'KV - node_gets_total',
      'KV - node_put_fsm_active',
      'KV - node_put_fsm_active_60s',
      'KV - node_put_fsm_in_rate',
      'KV - node_put_fsm_out_rate',
      'KV - node_put_fsm_rejected',
      'KV - node_put_fsm_rejected_60s',
      'KV - node_put_fsm_rejected_total',
      'KV - node_put_fsm_time_95',
      'KV - node_put_fsm_time_99',
      'KV - node_put_fsm_time_100',
      'KV - node_put_fsm_time_mean',
      'KV - node_put_fsm_time_median',
      'KV - node_puts',
      'KV - node_puts_total'
    ],
    selectedStat: '-- Choose a Statistic --',

    /**
     * When the select menu changes, use the selected value
     * to create a new time series graph.
     */
    didInsertElement: function () {
      this.set('selectionDidChange', function () {
        var selectedValue = this.$().find('option:selected').val();
        RiakControl.statGraphs.createGraph(selectedValue);
        this.set('selectedStat', '-- Choose a Statistiic --');
      });
    }
  });

  /**
   * @class
   */
  RiakControl.TimeSeriesView = Ember.Charts.TimeSeriesComponent.extend(
    /** @scope RiakControl.TimeSeriesView.prototype*/ {
  });

  /**
   * @class
   */
  RiakControl.StatsView = Ember.View.extend(
    /** @scope RiakControl.StatsView.prototype */ {
    templateName: 'stats'
  });

  /**
   * @class
   *
   * StatsController is responsible for displaying graphs related
   * to cluster statistics.
   */
  RiakControl.StatsController = Ember.ArrayController.extend(
    /**
     * Shares properties with RiakControl.ClusterController
     */
    RiakControl.ClusterAndNodeControls,
    /** @scope RiakControl.NodesController.prototype */ {

    /**
     * Gathers the current set of stats and hands them over
     * to the StatGraphCreator.
     *
     * @returns {void}
     */
    gatherNewStats: function () {
      var stats = [];
      this.get('content').map(function (item) {
        stats.push({name: item.get('name'), stats: item.get('stats')});
      });
      RiakControl.statGraphs.set('stats', stats);
    },

    /**
     * Reloads the record array associated with this controller.
     *
     * @returns {void}
     */
    reload: function() {
      this.gatherNewStats();
      this.get('content').reload();
    }
  });

  RiakControl.GraphController = Ember.ObjectController.extend({
    markerID: null,
    toolName: null,
    statName: null,
    parentController: null,
    json: [],
    toolNameReadable: function () {
      return this.get('toolName').replace(/\_/g, ' ');
    }.property('toolName'),
    statNameReadable: function () {
      return this.get('statName').replace(/\_/g, ' ');
    }.property('statName'),
    actions: {
      /**
       * Whenever the user clicks the remove graph button, the TimeSeries
       * object will run this method and pass itself in effectively destroying
       * itself and cleaning up the content array.
       */
      destroyObj: function () {
        var pc   = this.get('parentController'),
            id   = this.get('markerID'),
            that = this;
        $('div[data-graph-id="' + id + '"]').slideUp(300, function () {
          var myself = pc.findProperty('markerID', id);
          pc.removeObject(myself);
          that.destroy();
        });
      }
    }
  });

  /**
   * A place for storing graphs and their associated objects.
   */
  RiakControl.StatGraphs = Ember.ArrayController.extend({

    /**
     * Tracks available stats for every node.
     */
    stats: [],

    /**
     * A list of base colors for use in creating
     * color schemes for graphs.
     */
    possibleColors: [
      'rgb(240,95,95)',
      'rgb(240,163,95)',
      'rgb(240,211,95)',
      'rgb(177,240,95)',
      'rgb(95,240,163)',
      'rgb(95,240,211)',
      'rgb(95,177,240)',
      'rgb(119,95,240)',
      'rgb(177,95,240)',
      'rgb(240,95,211)'
    ],

    /**
     * Holds the most recent color used so we don't create
     * two graphs in a row with the same color.
     */
    currentColor: null,

    /**
     * Chooses a graph color
     */
    selectColor: function () {
      var colors   = this.get('possibleColors'),
          current  = this.get('currentColor'),
          selected = Math.round(Math.random() * (colors.length - 1));
      if (current === selected) {
        return this.selectColor();
      }
      this.set('currentColor', selected);
      return colors[selected];
    },

    /**
     * Whenever we get a new set of stats, we go through and
     * update the numbers for each graph.
     */
    updateGraphs: function () {
      var content = this.get('content'),
          that    = this;
      content.map(function (each) {
        var data = that.createJSON(each.get('toolName'), each.get('statName')),
            newJSON = each.get('json').concat(data);
        if (newJSON.length/data.length > 8) {
          newJSON = newJSON.slice(data.length);
        }
        each.set('json', newJSON);
      });
    }.observes('stats'),

    /**
     * Generates json data that can be understood by Addepar graphs.
     *
     * @param {String} toolName - Example: "kv"
     * @param {String} statName - Example: "cpu_nprocs"
     *
     * @returns {Object} - Containing the node name, a timestamp,
     *                     and the stat value.
     */
    createJSON: function (toolName, statName) {
      var date = new Date();
      return this.get('stats').map(function (each) {
        return {
          label: each.name,
          time:  date,
          value: each.stats[toolName][statName]
        };
      });
    },

    /**
     * Function for creating a new graph.
     *
     * @param {String} selected - Example: 'KV - cpu_avg1'
     */
    createGraph: function (selected) {

      /*
       * Get the stat name and clean stuff like "KV - " off the front of it.
       */
      var toolName = 'riak_' + selected.slice(0, selected.indexOf(' '))
                                       .toLowerCase(),
          statName = selected.replace(/^[^\s]+\s+\-\s+/, ''),
          json     = this.createJSON(toolName, statName),
          that     = this,
          graphObject,
          selectMenu;

      /*
       * If the selected item is not the default option...
       */
      if (selected !== '-- Choose a Statistic --') {

        /*
         * Create a new graph.
         */
        graphObject = RiakControl.GraphController.create({
          markerID: Ember.generateGuid(),
          toolName: toolName,
          statName: statName,
          parentController: that,
          colorBase: that.selectColor(),
          json: json
        });

        /*
         * Store the object in our array and
         * light it up.
         */
        this.pushObject(graphObject);
      }
    }

  });

  /**
   * Instantiates the StatGraphController
   */
  RiakControl.statGraphs = RiakControl.StatGraphs.create();


});
