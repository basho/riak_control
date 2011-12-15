// polls the ring status every so often
$(document).ready(function () {

    var pingAllowed = true;

    var currentPage;

    var pageAmount;

    var mainTimer;

    function initialize() {
        // Make sure our data holders exist
        $.riakControl.ringData = $.riakControl.ringData || {};
        $.riakControl.filter = $.riakControl.filter || {
            "ring" : {
                "dropdown" : "__all_nodes__",
                "primary"  : true,
                "fallback" : true
            }
        };
        get_partitions(1, 64);
        get_filters();
    }
    
    function get_filters() {
        $.ajax({
            method:'GET',
            url:'/admin/cluster/list',
            dataType:'json',
            success:update_filters,
        });
    }
    
    function get_partitions(pageNum, amountPerPage) {
        $.ajax({
            method:'GET',
            url:'/admin/ring/partitions?p=' + pageNum /* + '&n=' + amountPerPage */,
            dataType:'json',
            failure:ping_partitions,
            success: function (d) {
                draw_pagination(d.page, d.pages);
                update_partitions(d.contents);
            }
        });
    }

    function draw_pagination(pageNum, totalPages) {
        var i, pagination, thisPage;

        // Die if we don't need to change anything
        if (pageNum === currentPage && totalPages === pageAmount) {
            return false;
        }

        currentPage = pageNum;
        
        if ($('.pagination li').length) {
            if (currentPage === 1) {
                $('.pagination li[name=prev]').addClass('disabled');
            } else {
                $('.pagination li[name=prev]').removeClass('disabled');
            }

            if (currentPage === totalPages) {
                $('.pagination li[name=next]').addClass('disabled');
            } else {
                $('.pagination li[name=next]').removeClass('disabled');
            }
        }

        // Die if there is only one page
        if (totalPages === 1) {
            $('.pagination').hide();
            return false;
        }

        // Don't redraw pagination if the amount of pages hasn't changed
        if (totalPages === pageAmount) {
            return false;
        }

        pageAmount = totalPages;

        pagination = $('.pagination');

        // Always put in the 'previous' button
        pagination.empty().show().append('<li name="prev"><span class="paginator">Prev</span></li>');

        // Add page links as necessary
        for (i = 0; i < totalPages; i += 1) {
            thisPage = i + 1;
            pagination.append('<li name="' + thisPage + '"><span class="paginator pageNumber' + ((pageNum === thisPage) ? ' active' : '')  + '">' + thisPage + '</span></li>');
        }

        // Always put in the 'next' button
        pagination.append('<li name="next"><span class="paginator">Next</span></li>');

        if (currentPage === 1) {
            $('.pagination li[name=prev]').addClass('disabled');
        }
    }
    
    function update_filters(data) {
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
    
    function set_light_color(jqObj, newColor) {
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

    function set_operability_class(jqObj, newClass) {
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
    
    function filter_row_visibility(infoObj, row) {
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
        
    function partition_row(infoObj, updateDraw) {
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
                    if (obj.reachable === true && (obj.vnodes[i] === 'primary')) {
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
                if (Object.prototype.hasOwnProperty.call(obj.handoffs, i)) {
                    kind = i.split('_')[1];
                    if (obj.handoffs[i]) {
                        set_light_color($('.' + kind + '-light', row), 'orange');
                        $('.' + kind + '-status', row).html('Handoff');
                    }
                }
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

    // define a function to check properties against each other
    // We're doing it this long, convoluted way to guard against erlang giving
    // us equal objects where the keys are in a different order
    function keys_are_equal(oldObj, newObj) {
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
        // now run over the old object in case the new object did lose keys
        for (i in oldObj) {
            if (Object.prototype.hasOwnProperty.call(oldObj, i)) {
                if (typeof oldObj[i] === 'object' && (newObj[i] && typeof newObj[i] === 'object')) {
                    if (!keys_are_equal(newObj[i], oldObj[i])) {
                        return false;
                    }
                } else {
                    if (!newObj[i] || newObj[i] !== oldObj[i]) {
                        return false;
                    }
                }
            }
        }

        return true;
    }
        
    function update_partitions(data) {
        // called by get_partitions() which is called by initialize() and ping_partitions()
        
        var i,
        lowerBound = data[0]['i'],
        upperBound = data[data.length-1]['i'],
        partitions = $('.partition').not('.partition-template'),
        drawnPartitions = partitions.length;

        if (!$.riakControl.ringData[lowerBound] && drawnPartitions) {
            // empty out any drawn partitions that might already exist
            $('#ring-table-body').empty();
            // clear out the current ringData object
            $.riakControl.ringData = {};
        }

        // for each object in data array...
        for (i = lowerBound; i < upperBound; i += 1) {
            // if we have a length of drawn partitions, we have already drawn the ring
            // this also means we have prepopulated the $.riakControl.ringData object.
            // however, if there is a drawn ring but no item in question in the ringData object,
            // it means that we have moved to a new page of partitions
            if (drawnPartitions && $.riakControl.ringData[i]) {
                // check new data against old data to see if there are status changes
                // if keys are not equal...
                if (!keys_are_equal($.riakControl.ringData[i], data[i-lowerBound])) {
                    // populate $.riakControl.ringData[data[i].i] with the new data
                    $.riakControl.ringData[i] = data[i-lowerBound];
                    // send the corresponding node through the partitioning process
                    partition_row(data[i-lowerBound], 'update');
                }
                // send each node through the filtering process
                filter_row_visibility(data[i-lowerBound], $('#ring-table #partition-' + i));

            // if we count 0 drawn partitions, we have not drawn the ring
            } else {
                // populate $.riakControl.ringData[data[i].i] with the new data
                $.riakControl.ringData[i] = data[i-lowerBound];
                // send new data through the partitioning process and draw each node
                partition_row(data[i-lowerBound], 'draw');
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

        // call self indirectly through ping_partitions()
        ping_partitions();
        
    }
    
    function ping_partitions() {
        mainTimer = setTimeout(function () {
            if ($('#ring-headline').length && pingAllowed === true) {
                get_partitions(currentPage, 64);
            } else {
                // If we're not on the ring page or pinging is not allowed, the script dies here.
                pingAllowed = false;
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

    // Define what to do when we click on a non-disabled paginator
    $(document).on('click', '.pagination li:not(.disabled)', function (e) {
        var that = $(this), pageNum = that.attr('name');

        // Die if we're already on that page
        if (pageNum === currentPage) {
            return false;
        }

        // Stop the ping timer so we don't get double pings
        clearTimeout(mainTimer);

        if (pageNum === 'prev') {
            if (currentPage > 1) {
                $('.paginator.active').removeClass('active');
                $('.pagination li[name=' + (currentPage - 1) + '] .paginator').addClass('active');
                get_partitions(currentPage - 1, 64);
            }
        } else if (pageNum === 'next') {
            if (currentPage < pageAmount) {
                $('.paginator.active').removeClass('active');
                $('.pagination li[name=' + (currentPage + 1) + '] .paginator').addClass('active');
                get_partitions(currentPage + 1, 64);
            }
        } else {
            $('.paginator.active').removeClass('active');
            $('.pagination li[name=' + pageNum + '] .paginator').addClass('active');
            get_partitions(pageNum, 64);
        }
    });
    
    // Start everything on initial load
    initialize();

    // Subscribe to the 'templateSwitch' event.
    // This function will run when a template is switched.
    $.riakControl.sub('templateSwitch', function (templateName) {
        if (templateName === 'ring') {
            pingAllowed = true;
            initialize();
        }
    });
    
});