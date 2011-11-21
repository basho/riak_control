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
            url:$('#filter').val(),
            dataType:'json',
            success:update_partitions,
            failure:ping_partitions
        });
    }
    
    function update_filters (data) {
        var html = '<select id="filter">', i, l = data.length;
    
        // add the all options
        html += '<option value="/admin/ring/partitions">All</option>';
        html += '<option value="/admin/ring/partitions/filter/none">';
        html += '-------------------------</option>';
    
        for(i = 0; i < l; i += 1) {
            var node = data[i].name;
    
            // add this node as an option
            html += '<option value="/admin/ring/partitions/filter/node/' + node + '">';
            html += 'Owner: ' + node + '</option>';
        }
    
        // add the other common options
        html += '<option value="/admin/ring/partitions/filter/none">';
        html += '-------------------------</option>';
        html += '<option value="/admin/ring/partitions/filter/home">Home</option>';
        html += '<option value="/admin/ring/partitions/filter/away">Away</option>';
        
        // update the page
        $('#ring-filter').html(html + '</select>');
    }
    
    function partition_row (index) {
        
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
                return '<img src="/admin/ui/orange-arrow.png">';
            } else if (index.vnodes[type] === "primary") {
                return '<img src="/admin/ui/green-circle.png">';
            } else if (index.vnodes[type] === "fallback") {
                return '<img src="/admin/ui/blue-rect.png">';
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
        
        /*
        console.log(index);
        var html = '';
        var owner = index.node;
        
        var numID = index['i'];
        var row = $('#ring-table #partition-' + numID);
        if (!row.length) {
            row = $('.partition-template').clone();
            row.attr('id', 'partition-' + numID);
            row.removeClass('partition-template');
            row.show();
        }
        
        return row;
        */
    }
    
    function update_partitions (data) {
        var html = '', i, l = data.length;
    
        // loop over each index
        for(i = 0;i < l; i += 1) {
            html = html + partition_row(data[i]);
        }
        
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
    
    initialize();
    
});