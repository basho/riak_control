// polls the ring status every so often

$(document).ready(function () {

    function initialize () {
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
        html += '<option value="__all_nodes__">Filter partitions...</option>';
        html += '<option value="__all_nodes__">All</option>';
        html += '<option value="">-------------------------</option>';
    
        for(i = 0; i < l; i += 1) {
            var node = data[i].name;
    
            // add this node as an option
            html += '<option value="' + node + '">';
            html += 'Owner: ' + node + '</option>';
        }
    
        // add the other common options
        html += '<option value="">-------------------------</option>';
        
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
    
    function partition_row (index) {
        //console.log(index);
        //var html = '';
        var owner = index.node;
        
        var numID = index['i'];
        var row = $('#ring-table #partition-' + numID);
        if (!row.length) {
            row = $('.partition-template').clone();
            row.attr('id', 'partition-' + numID);
            row.removeClass('partition-template');
            row.show();
        }
        $('.partition-number', row).text(numID);
        $('.owner', row).text(owner);


        if (index.vnodes.riak_kv === 'primary') {
            set_light_color($('.kv-light', row), 'green');
            $('.kv-status', row).html('Active');
        } else {
            set_light_color($('.kv-light', row), 'gray');
            $('.kv-status', row).html('Idle');
        }
        
        if (index.vnodes.riak_pipe === 'primary') {
            set_light_color($('.pipe-light', row), 'green');
            $('.pipe-status', row).html('Active');
        } else {
            set_light_color($('.pipe-light', row), 'gray');
            $('.pipe-status', row).html('Idle');
        }
        
        if (index.vnodes.riak_search === 'primary') {
            set_light_color($('.search-light', row), 'green');
            $('.search-status', row).html('Active');
        } else {
            set_light_color($('.search-light', row), 'gray');
            $('.search-status', row).html('Idle');
        }

        // Deal with all red lights
        if (index.reachable === false) {
            set_operability_class($('.owner', row), 'unreachable');
            if (index.vnodes.riak_kv === 'undefined') {
                set_light_color($('.kv-light', row),   'red');
                $('.kv-status', row).html('Unreachable');
            }
            if (index.vnodes.riak_pipe === 'undefined') {
                set_light_color($('.pipe-light', row), 'red');
                $('.pipe-status', row).html('Unreachable');
            }
        }

        // Deal with all blue lights
        if (index.vnodes.riak_kv === 'fallback') {
            set_light_color($('.kv-light', row),   'blue');
            $('.kv-status', row).html('Fallback');
        } 
        if (index.vnodes.riak_pipe === 'fallback') {
            set_light_color($('.pipe-light', row), 'blue');
            $('.pipe-status', row).html('Fallback');
        }
        
        //console.log(index);
        return row[0];
        
    }
    
    function update_partitions (data) {
        var html = $(), i, l = data.length, 
            dropVal = $.riakControl.filter.ring.dropdown,
            showPrimary = $.riakControl.filter.ring.primary,
            showFallback = $.riakControl.filter.ring.fallback;

        function all_or_by_owner (obj) {
            if (dropVal === '__all_nodes__') {
                html.push(partition_row(data[obj]));
            } else if (dropVal === data[obj].node) {
                html.push(partition_row(data[obj]));
            }
        }

        // loop over each index
        for(i = 0;i < l; i += 1) {
            console.log(data[i]);
            if (showPrimary && showFallback) {
                all_or_by_owner(i);
            } else if (showPrimary && !showFallback) {
                if (data[i].vnodes.riak_kv !== 'fallback') {
                    all_or_by_owner(i);
                }
            } else if (!showPrimary && showFallback) {
                if (data[i].vnodes.riak_kv === 'fallback') {
                    all_or_by_owner(i);
                }
            }

        }
        
        $('#spinner').hide();
        $('#partition-list').fadeIn(300);
        if ($('#ring-headline').length) {
            $('#total-number').html('(' + l + ' ' + ((l === 1)?'Partition':'Partitions') + ' Total)');
        }

        // update the table
        $('#ring-table-body').html(html);
    
        // check again in a little bit
        //ping_partitions();
    }
    
    function ping_partitions () {
        setTimeout(get_partitions, 1000);
    }
    
    // Make sure our filter data holder exists
    $.riakControl.filter = $.riakControl.filter || {
        "ring" : {
            "dropdown" : "__all_nodes__",
            "primary"  : true,
            "fallback" : true
        }
    };
    
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
        }
    });
    
    // Start everything on initial load
    initialize();
    
});