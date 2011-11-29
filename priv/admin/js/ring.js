// polls the ring status every so often
var global = 0;
$(document).ready(function () {

    function initialize () {
        // Make sure our data holders exist
        $.riakControl.ringData = $.riakControl.ringData || {};
        $.riakControl.filter = $.riakControl.filter || {
            "ring" : {
                "dropdown" : "__all_nodes__",
                "primary"  : true,
                "fallback" : true
            }
        };
        get_partitions();
        get_filters();
    }
    
    function get_filters () {
        $.ajax({
            method:'GET',
            url:'/admin/cluster/list',
            dataType:'json',
            success:update_filters,
        });
    }
    
    function get_partitions () {
        $.ajax({
            method:'GET',
            url:'/admin/ring/partitions',
            dataType:'json',
            failure:ping_partitions,
            success: function (d) {
                update_partitions(d);
            }
        });
    }
    
    function update_filters (data) {
        var html = '', i, l = data.length;
    
        // add the all options
        html += '<option value="__all_nodes__">All Owners</option>';
        html += '<option value="">-------------------------</option>';
    
        for(i = 0; i < l; i += 1) {
            var node = data[i].name;
    
            // add this node as an option
            html += '<option value="' + node + '">';
            html += (node + '</option>');
        }
    
        // add the other common options
        
        
        // update the page
        $('#filter').html(html);
        
    }
    
    function set_light_color (jqObj, newColor) {
        var colors = ['green', 'gray', 'orange', 'red', 'blue'], i, l = colors.length;
        newColor = newColor.toLowerCase();
        for (i = 0; i < l; i += 1) {
            if (colors[i] === newColor) {
                jqObj.addClass(newColor);
            } else {
                jqObj.removeClass(colors[i]);
            }
        }
    }

    function set_operability_class (jqObj, newClass) {
        var classes = ['unreachable', 'disabled', 'down', 'normal'], i, l = classes.length;
        newClass = newClass.toLowerCase();
        for (i = 0; i < l; i += 1) {
            if (classes[i] === newClass) {
                jqObj.addClass(newClass);
            } else {
                jqObj.removeClass(classes[i]);
            }
        }
    }
    
    function filter_row_visibility (infoObj, row) {
        // collect all current filter values
        var dropval = $.riakControl.filter.ring.dropdown,
            showfallback = $.riakControl.filter.ring.fallback,
            showprimary = $.riakControl.filter.ring.primary,
            showhandoff = $.riakControl.filter.ring.handoff,
            isHandOff = (function () {
                var i, accum = 0;
                for (i in infoObj.handoff) {
                    if (Object.prototype.hasOwnProperty.call(infoObj.handoff, i)) {
                        accum += 1;
                    }
                }
                return (accum > 0);
            }()), visiblePartitions;

        var i, j, k;

        row.show();
        
        if (!showprimary) {
            for (i in infoObj.vnodes) {
                if (Object.prototype.hasOwnProperty.call(infoObj.vnodes, i)) {
                    if (infoObj.vnodes[i] === 'primary') {
                        row.hide();
                    }
                }
            }
        }
        
        if (!showfallback) {
            for (j in infoObj.vnodes) {
                if (Object.prototype.hasOwnProperty.call(infoObj.vnodes, j)) {
                    if (infoObj.vnodes[j] === 'fallback') {
                        row.hide();
                    }
                }
            }
        }

        if (!showhandoff) {
            if (isHandOff) {
                row.hide();
            }
        }
        
        if (dropval && dropval !== '__all_nodes__') {
            if (row.find('.owner').text() !== dropval) {
                row.hide();
            }
        }

        visiblePartitions = $('.partition').not('.partition-template').not(':hidden').length;
        if (!visiblePartitions) {
            $('#no-matches').removeClass('hide');
        } else {
            $('#no-matches').addClass('hide');
        }
        
    }
        
    
    function partition_row (infoObj, updateDraw) {
        // called by update_partitions()
        
        var owner = infoObj.node;
        var numID = infoObj['i'];
        var row;

        function deal_with_lights (obj, row) {
            // Deal with all green lights 
            if (obj.reachable === true) {
                set_operability_class($('.owner', row), 'normal');
                if (obj.vnodes.riak_kv === 'primary') {
                    set_light_color($('.kv-light', row),   'green');
                    $('.kv-status', row).html('Active');
                }
                if (obj.vnodes.riak_pipe === 'primary') {
                    set_light_color($('.pipe-light', row), 'green');
                    $('.pipe-status', row).html('Active');
                }
            }
            // Deal with all red lights
            if (obj.reachable === false) {
                set_operability_class($('.owner', row), 'unreachable');
                if (obj.vnodes.riak_kv === 'undefined') {
                    set_light_color($('.kv-light', row),   'red');
                    $('.kv-status', row).html('Unreachable');
                }
                if (obj.vnodes.riak_pipe === 'undefined') {
                    set_light_color($('.pipe-light', row), 'red');
                    $('.pipe-status', row).html('Unreachable');
                }
            }
            // Deal with all blue lights
            if (obj.vnodes.riak_kv === 'fallback') {
                set_light_color($('.kv-light', row),   'blue');
                $('.kv-status', row).html('Fallback');
            } 
            if (obj.vnodes.riak_pipe === 'fallback') {
                set_light_color($('.pipe-light', row), 'blue');
                $('.pipe-status', row).html('Fallback');
            }
            // Deal with all orange lights
            if (obj.handoffs.riak_kv) {
                set_light_color($('.kv-light', row), 'orange');
                $('.kv-status', row).html('Handoff');
            } 
            if (obj.handoffs.riak_pipe) {
                set_light_color($('.pipe-light', row), 'orange');
                $('.pipe-status', row).html('Handoff');
            }
        }

        // if updateDraw === 'draw'...
        if (updateDraw === 'draw') {
            // clone the partition template
            row = $('.partition-template').clone();
            row.attr('id', 'partition-' + numID);
            row.removeClass('partition-template');
            row.show();

            // apply proper text, classes, colors, and whatnot
            $('.partition-number', row).text(numID);
            $('.owner', row).text(owner);
            deal_with_lights(infoObj, row);

            // append the row to the table
            $('#ring-table-body').append(row);

        // else if updateDraw === 'update'...
        } else if (updateDraw === 'update') {
            // select the row from the table
            row = $('#ring-table #partition-' + numID);

            // apply proper text, classes, colors, and whatnot
            $('.partition-number', row).text(numID);
            $('.owner', row).text(owner);
            deal_with_lights(infoObj, row);
        }

    }
        
    
    function update_partitions (data) {
        // called by get_partitions() which is called by initialize() and ping_partitions()
        
        var i, l = data.length,
        partitions = $('.partition').not('.partition-template'),
        drawnPartitions = partitions.length;
    
        // define a function to check properties against each other
        function keys_are_equal (oldObj, newObj) {
            var i
            for (i in oldObj) {
                if (Object.prototype.hasOwnProperty.call(oldObj, i)) {
                    if (Object.prototype.toString.call(oldObj[i]) === '[object Object]') {
                        if (!keys_are_equal(oldObj[i], newObj[i])) {
                            return false;
                        }
                    } else {
                        if (oldObj[i] !== newObj[i]) {
                            return false;
                        }
                    }
                }
            }
            return true;
        }

        // for each object in data array...
        for (i = 0; i < l; i += 1) {
            // if we have a length of drawn partitions, we have already drawn the ring
            // this also means we have prepopulated the $.riakControl.ringData object
            if (drawnPartitions) {              
                // check new data against old data to see if there are status changes
                // if keys are not equal...
                if (!keys_are_equal($.riakControl.ringData[data[i]['i']], data[i])) {
                    // populate $.riakControl.ringData[data[i].i] with the new data
                    $.riakControl.ringData[data[i]['i']] = data[i];
                    // send the corresponding node through the partitioning process
                    partition_row(data[i], 'update');
                }
                // send each node through the filtering process
                filter_row_visibility(data[i], $('#ring-table #partition-' + data[i]['i']));
            // if we count 0 drawn partitions, we have not drawn the ring
            } else {
                // populate $.riakControl.ringData[data[i].i] with the new data
                $.riakControl.ringData[data[i]['i']] = data[i];
                // send new data through the partitioning process and draw each node
                partition_row(data[i], 'draw');
            }
        }
        
        
        // hide the #spinner if it is showing
        if ($('#ring-spinner').css('display') !== 'none') {
            $('#ring-spinner').hide();
        }

        // if the #partition-list is hidden, fade it in
        if ($('#partition-list').css('display') !== 'block') {
            $('#partition-list').fadeIn(300);
        }

        // call self through ping_partitions()
        ping_partitions();
        
    }
    
    
    
    function ping_partitions () {
        setTimeout(get_partitions, 1000);
    }
    
    // Define what to do when the filter dropdown value changes 
    $(document).on('change', '#filter', function (e) {
        $.riakControl.filter.ring.dropdown = $(this).val();
    });

    // Define what to do when the filter checkboxes change
    $(document).on('change', '#ring-filter .gui-checkbox', function (e) {
        var me = $(this), myID = me.attr('id');
        if (myID === 'primary-nodes') {
            (me.attr('checked') === 'checked') ? $.riakControl.filter.ring.primary = true : $.riakControl.filter.ring.primary = false;
        } else if (myID === 'fallback-nodes') {
            (me.attr('checked') === 'checked') ? $.riakControl.filter.ring.fallback = true : $.riakControl.filter.ring.fallback = false;
        } else if (myID === 'handoff-nodes') {
            (me.attr('checked') === 'checked') ? $.riakControl.filter.ring.handoff = true : $.riakControl.filter.ring.handoff = false;
        }
    });
    
    // Start everything on initial load
    initialize();
    
});