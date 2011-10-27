// polls the ring status every so often

var this_node = undefined;

$(document).ready(function () { initialize(); });

function initialize ()
{
    get_cluster_status();
}

function get_cluster_status ()
{
    $.ajax({
        method:'GET',
        url:'/admin/cluster/list',
        dataType:'json',
        success:update_cluster_status,
        failure:ping_cluster_status
    });
}

function perform_node_action (action)
{
    $.ajax({
        url:action,
        dataType:'json',
        success:alert,
        failure:alert
    });
}

function add_node ()
{
    perform_node_action('/admin/node/' + $('#node-to-add').val() + '/add');
}

function down_node ()
{
    perform_node_action('/admin/cluster/down/' + this_node)
}

function stop_node ()
{
    perform_node_action('/admin/node/' + this_node + '/stop');
    show_node_actions();
}

function leave_cluster ()
{
    perform_node_action('/admin/node/' + this_node + '/leave');
}

function show_node_actions (node)
{
    $('#node-name').html(node);
    $('#node-pong-actions').hide();
    $('#node-pang-actions').hide();
    $('#node-actions').html('<img src="/admin/ui/spinner.gif">');
    $('#node-actions').show();

    function action (url, label) {
        return '<input type="button" value="' + label + '" onclick="perform_node_action(\'' + url + '\')">';
    }

    function show_actions (data) {
        this_node = node;

        // get rid of the spinner
        $('#node-actions').hide();

        // show either pong or pang actions
        if (data.result != "ok") {
            $('#node-pang-actions').show();
        } else {
            $('#node-pong-actions').show();
        }
    }

    $.ajax({
        url:'/admin/node/' + node + '/ping',
        dataType:'json',
        success:show_actions
    });
}

function cluster_node_row (node)
{
    var stat = node.status;
    var ping = node.reachable;
    var name = node.name;
    var addr = name.split('@')[1];
    var port = node.port;
    var node;

    // create a div we can click
    var onclick = "show_node_actions('" + name + "')";
    var html = '<div id="' + name + '" onclick="' + onclick + '")">'
        
    // create the single table row
    html += '<table width="100%"><tr>';

    // make the node red if it isn't reachable
    if (ping == false) {
        node = '<font color="#f00"><b>' + name + '</b></font>';
    } else {
        node = name;
    }
    
    // add a link to that node in the cluster's admin page
    html += '<td align="left">' + node + '</td>';
    html += '<td align="right" width="80px">' + stat + '</td>';

    // done, return the row
    return html + '</tr></table></div>';
}

function update_cluster_status (nodes)
{
    var html = '';

    for(var i = 0;i < nodes.length;i++) {
        html += cluster_node_row(nodes[i]);
    }
    
    // update the page
    $('#cluster-table').html(html);

    // wait a little and update
    ping_cluster_status();
}

function ping_cluster_status ()
{
    setTimeout(get_cluster_status, 2000);
}