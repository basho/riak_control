// polls the ring status every so often

var this_node = undefined;

$(document).ready(function () { initialize(); });

function initialize ()
{
    get_cluster_status();

    // hide the menu when we leave it
    $('#cmenu').mouseleave(function () {
        $('#cmenu').hide();
    });
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
        failure:function (err) { alert(err); }
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

function update_node_row (node, row)
{
    $('.name', row).text(node.name);
    $('.status', row).text(node.status);
        
    // highlight offline nodes
    if (node.reachable == false) {
        $('.name', row).addClass('offline');
    } else {
        $('.name', row).removeClass('offline');
    }
}

function cluster_node_row (node)
{
    var id = node.name.split('@')[0];
    var rows = $('#cluster-table #' + id);

    // create a new row
    if (rows.length == 0) {
        row = $('.row-template').clone();

        // initialize the row
        update_node_row(node, row);

        // set the id for this row and display it
        $(row).attr('id', id);
        $(row).removeClass('row-template');
        $(row).show();

        // create a click handler for this row
        $(row).click(function (e) {
            $('#cmenu').show();
            $('#cmenu').offset({ top:e.pageY - 10, 
                                 left:e.pageX - 10
                               });
        });

        // add it to the table
        $('#cluster-table').append(row);
    } else {
        update_node_row(node, rows[0]);
    }
}

function remove_node_rows (nodes)
{
    var rows = $('#cluster-table .node');

    // check to see if a node is listed in the cluster
    function node_in_cluster_p (node) {
        for(var i = 0;i < nodes.length;i++) {
            if (node == nodes[i].name) {
                return true;
            }
        }
        return false;
    }

    // remove any node rows no long in the cluster
    for(var i = 0;i < rows.length;i++) {
        var node = $('.name', rows[i]).text();

        if (node_in_cluster_p(node) == false) {
            $(rows[i]).remove();
        }
    }
}

function update_cluster_status (nodes)
{
    var html = '';

    $('#spinner').hide();

    for(var i = 0;i < nodes.length;i++) {
        cluster_node_row(nodes[i]);
    }

    remove_node_rows(nodes);
    
    // wait a little and update
    ping_cluster_status();
}

function ping_cluster_status ()
{
    setTimeout(get_cluster_status, 2000);
}