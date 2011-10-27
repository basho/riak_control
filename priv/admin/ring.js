// polls the ring status every so often

$(document).ready(function () { initialize(); });

function initialize ()
{
    get_partitions();
    get_filters();
}

function get_filters ()
{
    $.ajax({
        method:'GET',
        url:'/admin/cluster/list',
        dataType:'json',
        success:update_filters,
    });
}

function get_partitions ()
{
    $.ajax({
        method:'GET',
        url:$('#filter').val(),
        dataType:'json',
        success:update_partitions,
        failure:ping_partitions
    });
}

function update_filters (data)
{
    var html = '<select id="filter">';

    // add the all options
    html += '<option value="/admin/ring/partitions">All</option>';
    html += '<option value="/admin/ring/partitions/filter/none">';
    html += '-------------------------</option>';

    for(var i = 0;i < data.length;i++) {
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

function partition_row (index)
{
    var i = index.i;
    var bgcolor = (i % 2) == 0 ? '#fff' : '#e0e0e0';
    var onclick = "show_partition_actions(" + i + ")";
    var html = '<div id="' + i + '" onclick="' + onclick + '")">';
        
    // create the single table row with the partition index
    html += '<table width="100%"><tr>';
    html += '<td>' + i + '</td>';
    
    function vnode_icon (type) {
        if (index.handoffs[type]) {
            return '<img src="/admin/ui/orange-arrow.png">';
        } else if (index.vnodes[type] == "primary") {
            return '<img src="/admin/ui/green-circle.png">';
        } else if (index.vnodes[type] == "fallback") {
            return '<img src="/admin/ui/blue-rect.png">';
        } else {
            return '';
        }
    }

    if (index.online == false) {
        index.node = '<font color="#f00"><b>' + index.node + '</b></font>';
    }
    
    // show whether or not the home node is down
    html += '<td align="right" width="300px">' + index.node + '</td>';
    
    // show vnode worker processes
    html += '<td align="center" width="24px">';
    html += vnode_icon('riak_kv') + '</td>';
    html += '<td align="center" width="24px">';
    html += vnode_icon('riak_pipe') + '</td>';
    html += '<td align="center" width="24px">';
    html += vnode_icon('riak_search') + '</td>';

    // done, return the row
    return html + '</tr></table></div>';
}

function update_partitions (data)
{
    var html = '';

    // loop over each index
    for(var i = 0;i < data.length;i++) {
        html = html + partition_row(data[i]);
    }

    // update the table
    $('#ring-table').html(html);

    // check again in a little bit
    ping_partitions();
}

function ping_partitions ()
{
    setTimeout(get_partitions, 1000);
}