(function() {
/*globals $*/

/**
  Copyright 2012 Christopher Meiklejohn and Basho Technologies, Inc.

  Licensed under the Apache License, Version 2.0 (the "License"); you
  may not use this file except in compliance with the License.  You may
  obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
  implied.  See the License for the specific language governing
  permissions and limitations under the License.

  All of the files in this project are under the project-wide license
  unless they are otherwise marked.
**/

/**
  @class

  CanvasView provides a responsive container for containing your SVG
  elements.

  A CanvasView containing a SVG ensure that the visualization scales up
  at a particular aspect ratio, and scales down properly at a minimum
  width.

  To use, wrap around your graph like so:

      {{#view Ember.CanvasView}}
        {{view Ember.HistogramView ...}}
      {{/view}}

  CanvasView provides sane defaults, but you may override the
  presentation using the following attributes: containerWidth,
  containerHeight, xMargin, yMargin, and aspectRatio.

**/
Ember.CanvasView = Ember.View.extend(
/** @scope Ember.CanvasView.prototype */ {

  xMargin: 70,

  yMargin: 20,

  containerWidth: 980,

  containerHeight: 300,

  aspectRatio: 980 / 300,

  viewBox: function() {
    var containerWidth  = this.get('containerWidth'),
        containerHeight = this.get('containerHeight');

    return "0 0 " + containerWidth + " " + containerHeight;
  }.property('containerWidth'),

  preserveAspectRatio: function() {
    return "xMinYMid";
  }.property('containerWidth'),

  width: function() {
    return this.get('containerWidth');
  }.property('containerWidth'),

  height: function() {
    return this.get('containerWidth') / this.get('aspectRatio');
  }.property('containerWidth'),

  didInsertElement: function() {
    var self = this,
        elem = this.$();

    Ember.run.next(function() {
      self.set('containerWidth', elem.width());

      $(window).resize(function(){
        self.set('containerWidth', elem.width());
      });
    });

    this._super();
  }

});

})();



(function() {
/*globals d3*/

/**
  Copyright 2012 Christopher Meiklejohn and Basho Technologies, Inc.

  Licensed under the Apache License, Version 2.0 (the "License"); you
  may not use this file except in compliance with the License.  You may
  obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
  implied.  See the License for the specific language governing
  permissions and limitations under the License.

  All of the files in this project are under the project-wide license
  unless they are otherwise marked.
**/

/**
  @class

  VisualizationView provides a base visualization class for rendering
  graph axes, performing axis and label formatting, and displaying
  gridlines.

  In any of your views, simply override xFormatter, and yFormatter to
  provide custom axis formatting.

  VisualizationView is also responsible for redrawing the element on the
  page when the content changes.

**/
Ember.VisualizationView = Ember.View.extend(
/** @scope Ember.VisualizationView.prototype */ {

  tagName: 'svg',

  attributeBindings: ['width',
                      'height',
                      'viewBox',
                      'preserveAspectRatio'],

  xMargin: 0,

  yMargin: 0,

  xScale: function() {
    return d3.scale.linear();
  }.property('content'),

  yScale: function() {
    return d3.scale.linear();
  }.property('content'),

  xFormatter: function(x) {
    return x;
  },

  yFormatter: function(y) {
    return y;
  },

  redraw: function() {
    this.rerender();
  }.observes('content'),

  didInsertElement: function() {
    var self       = this,
        id         = this.$().attr('id'),
        xScale     = this.get('xScale'),
        yScale     = this.get('yScale'),
        xMargin    = this.get('xMargin'),
        yMargin    = this.get('yMargin'),
        width      = this.get('width'),
        height     = this.get('height'),
        xFormatter = this.get('xFormatter'),
        yFormatter = this.get('yFormatter'),
        xAxis,
        yAxis,
        svg;

    svg = d3.select("#" + id);

    xAxis = d3.svg.axis().scale(xScale).orient("bottom").tickFormat(xFormatter);

    yAxis = d3.svg.axis().scale(yScale).orient("left").tickFormat(yFormatter);

    svg.append("g").attr("class", "x axis").
      attr("transform", "translate(0," + (height - yMargin) + ")").call(xAxis);

    svg.append("g").attr("class", "y axis").
      attr("transform", "translate(" + xMargin + ", 0)").call(yAxis);

    svg.insert("g", ":first-child").attr("class", "x grid").
        attr("transform", "translate(0," + (height - yMargin) + ")").
          call(xAxis.tickSize(-height + (yMargin * 2), 0, 0).tickFormat(""));

    svg.insert("g", ":first-child").attr("class", "y grid").
        attr("transform", "translate(" + xMargin + ", 0)").
          call(yAxis.tickSize(-width + (xMargin * 2), 0, 0).tickFormat(""));
  }

});

})();



(function() {
/*globals d3*/

/**
  Copyright 2012 Christopher Meiklejohn and Basho Technologies, Inc.

  Licensed under the Apache License, Version 2.0 (the "License"); you
  may not use this file except in compliance with the License.  You may
  obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
  implied.  See the License for the specific language governing
  permissions and limitations under the License.

  All of the files in this project are under the project-wide license
  unless they are otherwise marked.
**/

/**
  @class

  HistogramView provides a visualization class for rendering a histogram
  with axes, margins and gridlines.

  To use, simply instantiate the view with a content binding either
  pointing to an array of values which the histogram will be dervied
  from, or an array of objects with an x and y attribute for pre-binned
  histograms.

  For example:

      {{view Ember.HistogramView contentBinding="content"}}

**/
Ember.HistogramView = Ember.VisualizationView.extend(
/** @scope Ember.HistogramView.prototype */ {

  histogram: function() {
    var content      = this.get('content'),
        firstElement = content[0];

    if(firstElement.x && firstElement.y) {
      return content;
    } else {
      return d3.layout.histogram()(this.getPath('content'));
    }
  }.property('content'),

  xScale: function() {
    var histogram = this.get('histogram'),
        width     = this.get('width'),
        xMargin   = this.get('xMargin');

    Ember.assert(
      "You need to provide a width for the histogram view.",
      width !== undefined);

    return d3.scale.ordinal().domain(
        histogram.map(function(d) {
          return d.x;
        })).rangeRoundBands([0 + xMargin, width - xMargin]);
  }.property('histogram'),

  yScale: function() {
    var histogram = this.get('histogram'),
        height    = this.get('height'),
        yMargin   = this.get('yMargin');

    Ember.assert(
        "You need to provide a height for the histogram view.",
        height !== undefined);

    return d3.scale.linear().domain([0, d3.max(
          histogram.map(function(d) {
            return d.y;
          }))]).range([height - yMargin, 0 + yMargin]);
  }.property('histogram'),

  didInsertElement: function() {
    var self       = this,
        histogram  = this.get('histogram'),
        id         = this.$().attr('id'),
        xScale     = this.get('xScale'),
        yScale     = this.get('yScale'),
        xMargin    = this.get('xMargin'),
        yMargin    = this.get('yMargin'),
        width      = this.get('width'),
        height     = this.get('height'),
        xFormatter = this.get('xFormatter'),
        yFormatter = this.get('yFormatter'),
        xAxis,
        yAxis,
        svg;

    svg = d3.select("#" + id);

    svg.selectAll("rect").
        data(histogram).
      enter().append("rect").
        attr("class", "sample").
        attr("width", xScale.rangeBand()).
        attr("x", function(d) { return xScale(d.x); }).
        attr("y", function(d) { return yScale(d.y); }).
        attr("height", function(d) {
          return height - yMargin - yScale(d.y);
        });

    this._super();
  }

});

})();



(function() {
/*globals d3*/

/**
  Copyright 2012 Christopher Meiklejohn and Basho Technologies, Inc.

  Licensed under the Apache License, Version 2.0 (the "License"); you
  may not use this file except in compliance with the License.  You may
  obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
  implied.  See the License for the specific language governing
  permissions and limitations under the License.

  All of the files in this project are under the project-wide license
  unless they are otherwise marked.
**/

/**
  @class

  TimeSeriesView provides a visualization class for rendering a
  single-line time series graph, with axes, and gridlines.

  To use, simply instantiate the view with a content binding either
  pointing to an array of objects, each containing an x and y attribute.

  For example:

      {{view Ember.TimeSeriesView contentBinding="content"}}

**/
Ember.TimeSeriesView = Ember.VisualizationView.extend(
/** @scope Ember.TimeSeriesView.prototype */ {

  path: function() {
    var content = this.get('content'),
        xScale  = this.get('xScale'),
        yScale  = this.get('yScale');

    return d3.svg.line().
      interpolate("linear").
      x(function(d) { return xScale(d.x); }).
      y(function(d, i) { return yScale(d.y); })(content);
  }.property('content'),

  xScale: function() {
    var content       = this.get('content'),
        width         = this.get('width'),
        xMargin       = this.get('xMargin'),
        first_sample  = content[0],
        last_sample   = content[content.length-1];

    if(first_sample && last_sample) {
      return d3.time.scale().
        domain([first_sample.x, last_sample.x]).
        range([0 + xMargin, width - xMargin]);
    }
  }.property('content'),

  yScale: function() {
    var content = this.get('content'),
        height  = this.get('height'),
        yMax    = this.get('yMax'),
        yMargin = this.get('yMargin');

    return d3.scale.linear().
      domain([0, yMax]).
      range([height - yMargin, 0 + yMargin]);
  }.property('content'),

  yMax: function() {
    var content = this.get('content');

    return d3.max(content.map(function(el) {
      return parseFloat(el.y);
    }));
  }.property('content'),

  didInsertElement: function() {
    var self       = this,
        path       = this.get('path'),
        id         = this.$().attr('id'),
        xScale     = this.get('xScale'),
        yScale     = this.get('yScale'),
        xMargin    = this.get('xMargin'),
        yMargin    = this.get('yMargin'),
        width      = this.get('width'),
        height     = this.get('height'),
        xFormatter = this.get('xFormatter'),
        yFormatter = this.get('yFormatter'),
        xAxis,
        yAxis,
        svg;

    svg = d3.select("#" + id);

    svg.append("g").append("path").
      attr("class", "series").attr("d", path);

    this._super();
  }

});


})();



(function() {
/**
  Copyright 2012 Christopher Meiklejohn and Basho Technologies, Inc.

  Licensed under the Apache License, Version 2.0 (the "License"); you
  may not use this file except in compliance with the License.  You may
  obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
  implied.  See the License for the specific language governing
  permissions and limitations under the License.

  All of the files in this project are under the project-wide license
  unless they are otherwise marked.
**/

})();

