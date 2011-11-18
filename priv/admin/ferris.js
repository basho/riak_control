/*
    Name: ferris.js
    Author: John Newman
    Date: 11-18-2011
    License: MIT
    Version: 1.0

    Description:  
    Super simple html templating engine for jQuery.  It works with ajax and
    caches your templates after the first retrieval to save on bandwidth and time.
    Save your html templates in text files with the extension ".ferris".  
    Then set up your template container like this:

    // First assume you have created two template files called firstTemplateName.ferris
    // and secondTemplateName.ferris.

    $(function () {
        // Make sure we're in the docready

        $.ferris.setup('#elementToBePopulated', {

            "templateLocation" : "/path/to/templates/folder/",

            "swapin" : {
                "firstTemplateName" : ["click", "#someButton"],
                "secondTemplateName" : ["click", "#someOtherButton"]
            }
 
        });

    }());

    // Now, when I click on #someButton, my first template will be swapped in.
    // When I click on #someOtherButton, my second template will be swapped in.

*/

(function ($) {
    "use strict";

    // Closure variables...
        // object for pub/sub.  CURRENTLY NOT USED BUT MIGHT BE IF THIS EXPANDS
    var _events = {},

        // object for caching html upon first retrieval
        _cache = {},

        // publish function.  CURRENTLY NOT USED BUT MIGHT BE IF THIS EXPANDS
        _pub = $.publish || function (eventname, args, context) {
            var i, l = (this.events[eventname].length || 0);
            for (i = 0; i < l; i += 1) {
                this.events[eventname][i].apply((context || null), args);
            }
            return this.events[eventname];
        },

        // subscribe function.  CURRENTLY NOT USED BUT MIGHT BE IF THIS EXPANDS
        _sub = $.subscribe || function (eventname, func) {
            if (this.events[eventname]) {
                this.events[eventname] = [func];
            } else {
                this.events[eventname].push(func);
            }
            return this.events[eventname];
        };

    // turn it into a jQuery plugin
    $.ferris = $.ferris || {

        // publish function.  CURRENTLY NOT USED BUT MIGHT BE IF THIS EXPANDS
        "_pub" : $.publish || function (eventname, args, context) {
            var i, l = (this.events[eventname].length || 0);
            for (i = 0; i < l; i += 1) {
                this.events[eventname][i].apply((context || null), args);
            }
            return this.events[eventname];
        },

        // subscribe function.  CURRENTLY NOT USED BUT MIGHT BE IF THIS EXPANDS
        "_sub" : $.subscribe || function (eventname, func) {
            if (this.events[eventname]) {
                this.events[eventname] = [func];
            } else {
                this.events[eventname].push(func);
            }
            return this.events[eventname];
        },

        // function for clearing cache.  you can clear a single property or the whole object
        "clearCache" : function (elem) {
            var i;
            if (elem !== undefined && _cahce[elem]) {
                delete _cache[elem];
            } else {
                _cache = {};
            }
        },

        // function for swapping out a template
        "swapInTemplate" : function (elem, tempName, tempLocation) {
            if (_cache[tempName]) {
                $(elem).empty().html(_cache[tempName]);
            } else {
                $.ajax({
                    "url" : tempLocation,
                    "success" : function (x, y, z) {
                        _cache[tempName] = x;
                        $(elem).empty().html(x);
                    }
                });
            }
        },

        // function for setting up an html element to receive templates
        "setup" : function (container, settings) {
            var that = this, tempName;

            if (!settings.templateLocation) {
                settings.templateLocation = '/';
            }

            if (settings.swapin && Object.prototype.toString.call(settings.swapin) !== '[object Object]') {
                throw new Error('Your swapins have to be in JSON format.');
            }
            
            function attachHandlers (nameOfTemplate) {
                var template = nameOfTemplate;
                var node = settings.swapin[template][1];
                var action = settings.swapin[template][0];
                var location = settings.templateLocation + template + '.ferris';
                $(node).on(action, function () {
                    that.swapInTemplate(container, template, location);
                });
            }

            for (tempName in settings.swapin) {
                if (Object.prototype.hasOwnProperty.call(settings.swapin, tempName)) {
                    attachHandlers(tempName);
                }
            }

        } // setup

    }; // $.ferris declaration

}(jQuery));
