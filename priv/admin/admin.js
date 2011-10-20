// polls the ring status every so often

var node = undefined;

$(document).ready(function () { initialize(); });

function initialize ()
{
    get_stats();
    get_cluster_status();
    get_partitions();
}

function get_stats ()
{
    $.ajax({
        method:'GET',
        url:'/stats',
        dataType:'json',
        success:update_stats,
        failure:ping_stats
    });
}

function ping_stats ()
{
    setTimeout(get_stats, 5000);
}

function update_stats (stats)
{
    var html='<table>';
    var actions='';
    var node=stats['nodename'];

    // capture the name of the node and update the title
    $('#name').html('<h3>' + node + '</h3><ul>' + actions + '</ul>');

    // capture each stat in the table
    for(var stat in stats) {
        var value = stats[stat];

        // append to the table
        html += '<tr><td align="left" nowrap>' + stat + '</td>';
        html += '<td align="left" nowrap>' + value + '</td></tr>';
    }

    // write them out
    $('#stats').html(html + '</table>');

    // wait a little and update
    ping_stats();
}

function get_cluster_status ()
{
    $.ajax({
        method:'GET',
        url:'/admin/cluster',
        dataType:'json',
        success:update_cluster_status,
        failure:ping_cluster_status
    });
}

function node_action (action)
{
    function success (data) {
        if (data.result == 'ok') {
            alert("ok");

            // update the cluster status now
            get_cluster_status();
        } else {
            alert("error: " + data.error);
        }
    }

    $.ajax({
        method:'GET',
        url:action,
        dataType:'json',
        success:success,
        failure:alert
    });
}

function join_node ()
{
    node_action('/admin/node/' + $('#node').val() + '/join');
}

function update_cluster_status (nodes)
{
    var html = '<h3>Cluster Status</h3><table>';

    for(var i = 0;i < nodes.length;i++) {
        var stat = nodes[i].status;
        var ping = nodes[i].reachable;
        var name = nodes[i].name;
        var port = nodes[i].port;
        var addr = name.split('@')[1];
        var node;

        // make the node red if it isn't reachable
        if (ping == false) {
            node = '<font color="#FF0000"><b>' + name + '</b></font>';
        } else {
            var href = 'http://' + addr + ':' + port + '/admin';
            var link = '<a href="' + href + '">' + name + '</a>';

            // make it a link
            node = link;
        }

        // add a link to that node in the cluster's admin page
        html += '<tr><td align="left" nowrap>' + node + '</td>';
        html += '<td align="left" nowrap>' + stat + '</td>';

        function button (value,action) {
            return '<input type="button"' + 
                ' onclick="node_action(\'' + action + '\')"' + 
                ' value="' + value + 
                '">';
        }

        // if it's valid, then it can leave, too
        if (ping && stat == 'valid') {
            var leave='/admin/node/' + name + '/leave';
            var kill='/admin/node/' + name + '/kill';

            // create buttons
            html += '<td>' + button('Leave',leave) + '</td>';
            html += '<td>' + button('Kill',kill) + '</td>';
        } else if (ping == false) {
            var down='/admin/node/' + name + '/down';

            // create node-unreachable buttons
            if (stat != 'down') {
                html += '<td>' + button('Down',down) + '</td>';
            }
        }

        // done
        html += '</tr>';
    }
    
    // update the page
    $('#cluster').html(html + '</table>');

    // wait a little and update
    ping_cluster_status();
}

function ping_cluster_status ()
{
    setTimeout(get_cluster_status, 2000);
}

function get_partitions ()
{
    $.ajax({
        method:'GET',
        url:'/admin/ring/partitions',
        dataType:'json',
        success:update_partitions,
        failure:ping_partitions
    });
}

function update_partitions (partitions)
{
    var html = "<h3>Ring Status</h3><table>";

    // header for table
    html += '<tr>';
    html += '<td>Partition Index</td>';
    html += '<td>Owning Node</td>';
    html += '<td>KV</td>';
    html += '<td>Pipe</td>';
    html += '<td>Search</td>';
    html += '</tr>';

    for(var i = 0;i < partitions.length;i++) {
        var index = partitions[i];
        var bgcolor = (i % 2) == 0 ? '#e0e0e0' : '#ffffff';

        // create row
        html += '<tr bgcolor="' + bgcolor + '">';
        html += '<td>' + index.i + '</td>';
        html += '<td>' + index.node + '</td>';

        function vnode_icon (type) {
            if (index.handoffs[type]) {
                return '<img src="/admin/ui/orange-arrow-right-16.png">';
            } else if (index.vnodes.indexOf(type) >= 0) {
                return '<img src="/admin/ui/green-circle-filled-16.png">';
            } else {
                return '';//<img src="/admin/ui/red-circle-filled-16.png">';
            }
        }

        html += '<td>' + vnode_icon('riak_kv_vnode') + '</td>';
        html += '<td>' + vnode_icon('riak_pipe_vnode') + '</td>';
        html += '<td>' + vnode_icon('riak_search_vnode') + '</td>';
        html += '</tr>';
    }

    // update partition status
    $('#partitions').html(html + '</table>');

    // reping in a little bit
    ping_partitions();
}

function ping_partitions ()
{
    setTimeout(get_partitions, 1000);
}