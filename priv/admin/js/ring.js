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
        var partitionIndex = infoObj['index'];
        var owner = infoObj.node;
        var numID = infoObj['i'];
        var row;

        function deal_with_lights (obj, row) {
            var i, kind;
            if (obj.reachable === true) {
                set_operability_class($('.owner', row), 'normal');
            } else {
                set_operability_class($('.owner', row), 'unreachable');
            }
            for (i in obj.vnodes) {
                if (Object.prototype.hasOwnProperty.call(obj.vnodes, i)) {
                    kind = i.split('_')[1];
                    if (obj.reachable === true && (obj.vnodes[i] || obj.vnodes[i] !== 'undefined')) {
                        set_light_color($('.' + kind + '-light', row), 'green');
                        $('.' + kind + '-status', row).html('Active');
                    } else if (obj.vnodes[i] === 'fallback') {
                        set_light_color($('.' + kind + '-light', row), 'blue');
                        $('.' + kind + '-status', row).html('Fallback');
                    } else if (!obj.vnodes[i] || obj.vnodes[i] === 'undefined') {
                        set_light_color($('.' + kind + '-light', row), 'red');
                        $('.' + kind + '-status', row).html('Unreachable');
                    }
                }
            }
            for (i in obj.handoffs) {
                if (Object.prototype.hasOwnProperty.call(obj.handoffs[i])) {
                    kind = i.split('_')[1];
                    if (obj.handoffs[i] || obj.handoffs[i] !== 'undefined') {
                        set_light_color($('.' + kind + '-light', row), 'orange');
                        $('.' + kind + '-status', row).html('Handoff');
                    }
                }
            }
        }

        // if updateDraw === 'draw'...
        if (updateDraw === 'draw') {
            console.log(infoObj);
            // clone the partition template
            row = $('.partition-template').clone();
            row.attr('id', 'partition-' + numID);
            row.removeClass('partition-template');
            row.show();

            // apply proper text, classes, colors, and whatnot
            $('.partition-number', row).text(numID);
            $('.owner', row).text(owner);
            $('.partition-index', row).text(partitionIndex);
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
            var i; 
            // loop through the new object because it's more likely to have extra properties
            for (i in newObj) {
                // avoid prototypal mistakes
                if (Object.prototype.hasOwnProperty.call(newObj, i)) {
                    // we only want to loop through a subobject if we can prove it's JSON for now
                    if (typeof newObj[i] === 'object' && (oldObj[i] && typeof oldObj[i] === 'object')) {
                        if (!keys_are_equal(oldObj[i], newObj[i])) {
                            return false;
                        }
                    } else {
                        if (!oldObj[i] || oldObj[i] !== newObj[i]) {
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
        setTimeout(function () {
            if ($('#ring-headline').length) {
                get_partitions();
            } else {
                ping_partitions();
            }
        }, 1000);
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