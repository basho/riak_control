
// polls the ring status every so often


$(document).ready(function () {

    var this_node = undefined;
    
    function initialize () {
    
    }
    
    function get_partitions () {
        $.ajax({
            method:'GET',
            url:'/admin/ring/partitions',
            dataType:'json',
            success:update_partitions,
            failure:ping_partitions
        });
    }
    
    function partition_table (partitions, show_node) {
        var html = '<table width="100%">';
    
        // header for table
        html += '<tr>';
        html += '<td class="header">Index</td>';
    
        // optioanlly show owner and home status
        if (show_node) {
            html += '<td>Owning Node</td>';
            html += '<td>Away</td>';
        }
    
        // vnode worker processes
        html += '<td width="24px" class="header">K</td>';
        html += '<td width="24px" class="header">P</td>';
        html += '<td width="24px" class="header">S</td>';
        html += '</tr>';
    
        // loop over each index
        for(var i = 0;i < partitions.length;i++) {
            var index = partitions[i];
            var bgcolor = (i % 2) == 0 ? '#e0e0e0' : '#ffffff';
    
            // create row
            html += '<tr bgcolor="' + bgcolor + '">';
            html += '<td>' + index.i + '</td>';
    
            function vnode_icon (type) {
                if (index.handoffs[type]) {
                    return '<img src="/admin/ui/orange-arrow.png">';
                } else if (index.vnodes.indexOf(type) >= 0) {
                    return '<img src="/admin/ui/green-circle.png">';
                } else {
                    return '';//<img src="/admin/ui/red-circle-filled-16.png">';
                }
            }
    
            // show whether or not the home node is down
            if (show_node) {
                html += '<td>' + index.node + '</td>';
                
                // indicate home or away status
                if (index.home == false) {
                    html += '<td><img src="/admin/ui/red-circle.png"></td>';
                } else {
                    html += '<td></td>';
                }
            }
    
            // show vnode worker processes
            html += '<td>' + vnode_icon('riak_kv_vnode') + '</td>';
            html += '<td>' + vnode_icon('riak_pipe_vnode') + '</td>';
            html += '<td>' + vnode_icon('riak_search_vnode') + '</td>';
            html += '</tr>';
        }
    
        return html;
    }
    
    function ping_partitions ()
    {
        setTimeout(get_partitions, 1000);
    }
    
    initialize();

});
