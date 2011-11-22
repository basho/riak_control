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
            url:'/admin/ring/partitions', //$('#filter').val(),
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
        //html += '<option value="__home_nodes__">Home</option>';
        //html += '<option value="__away_nodes__">Away</option>';
        
        // update the page
        $('#filter').html(html);
        
        console.log($('#filter'));
    }
    
    function set_light_color (jqObj, newColor) {
        var colors = ['green', 'gray', 'orange', 'red'], i, l = colors.length;
        newColor = newColor.toLowerCase();
        for (i = 0; i < l; i += 1) {
            if (colors[i] === newColor) {
                jqObj.addClass(newColor);
            } else {
                jqObj.removeClass(colors[i]);
            }
        }
    }
    
    function partition_row (index) {
        /*
        var i = index.i;
        //var bgcolor = (i % 2) === 0 ? '#fff' : '#e0e0e0';
        //var bgcolor = (i % 2) === 0 ? 'transparent' : 'transparent';
        //var onclick = "show_partition_actions(" + i + ")";
        //var html = '<div id="' + i + '" onclick="' + onclick + '")">';
        var html = '<tr id="' + i + '" class="gui-text">'
            
        // create the single table row with the partition index
        //html += '<table width="100%"><tr>';
        html += '<td class="partition-number">' + i + '</td>';
        
        function vnode_icon (type) {
            if (false) { //index.handoffs[type]) {
                return '<img src="/admin/ui/images/orange-arrow.png">';
            } else if (index.vnodes[type] === "primary") {
                return '<img src="/admin/ui/images/green-circle.png">';
            } else if (index.vnodes[type] === "fallback") {
                return '<img src="/admin/ui/images/blue-rect.png">';
            } else {
                return '';
            }
        }
    
        if (index.online === false) {
            index.node = '<font color="#f00"><b>' + index.node + '</b></font>';
        }
        
        // show whether or not the home node is down
        html += '<td>' + index.node + '</td>';
        
        // show vnode worker processes
        html += '<td>';
        html += vnode_icon('riak_kv') + '</td>';
        html += '<td>';
        html += vnode_icon('riak_pipe') + '</td>';
        html += '<td>';
        html += vnode_icon('riak_search') + '</td>';
    
        // done, return the row
        //return html + '</tr></table></div>';
        return html + '</tr>';
        */
        
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
            $('.kv-status', row).html('active');
        } else {
            set_light_color($('.kv-light', row), 'gray');
            $('.kv-status', row).html('idle');
        }
        
        if (index.vnodes.riak_pipe === 'primary') {
            set_light_color($('.pipe-light', row), 'green');
            $('.pipe-status', row).html('active');
        } /*else if (index.vnodes.riak_pipe === 'undefined') {
            set_light_color($('.pipe-light', row), 'red');
            $('.pipe-status', row).html('unreachable');
        } */else {
            set_light_color($('.pipe-light', row), 'gray');
            $('.pipe-status', row).html('idle');
        }
        
        if (index.vnodes.riak_search === 'primary') {
            set_light_color($('.search-light', row), 'green');
            $('.search-status', row).html('active');
        } else {
            set_light_color($('.search-light', row), 'gray');
            $('.search-status', row).html('idle');
        }
        
        //console.log(index);
        return row[0];
        
    }
    
    function update_partitions (data) {
        var html = $(), i, l = data.length, filterVal = $.riakControl.filter.ring;

        $.riakControl.filter.temp = [];
        // loop over each index
        for(i = 0;i < l; i += 1) {
            //html.push(partition_row(data[i]));
            if (filterVal && filterVal === '__all_nodes__') {
                html.push(partition_row(data[i]));
            } else if (filterVal) {
                if (data[i].node === filterVal) {
                    html.push(partition_row(data[i]));
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
        ping_partitions();
    }
    
    function ping_partitions () {
        setTimeout(get_partitions, 1000);
    }
    
    // Make sure our filter data holder exists
    $.riakControl.filter = $.riakControl.filter || {"ring":"__all_nodes__"};
    
    // Define what to do when the filter value changes 
    $(document).on('change', '#filter', function (e) {
        $.riakControl.filter.ring = $(this).val();
    });
    
    // Start everything on initial load
    initialize();
    
});