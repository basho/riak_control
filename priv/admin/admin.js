// polls the ring status every so often

$(document).ready(function () { initialize(); });

function initialize ()
{
    get_stats();
    get_cluster_status();
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

    // capture the name of the node and update the title
    $('#name').html('<h3>' + stats['nodename'] + '</h3>');

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

function update_cluster_status (nodes)
{
    var html = '<table>';

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
        html += '<td align="left" nowrap>' + stat + '</td></tr>';
    }
    
    // update the page
    $('#cluster').html(html + '</table>');

    // wait a little and update
    ping_cluster_status();
}

function ping_cluster_status ()
{
    setTimeout(get_cluster_status, 5000);
}