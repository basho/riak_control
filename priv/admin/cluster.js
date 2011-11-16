// polls the ring status every so often


$(document).ready(function () {

    var this_node = undefined;

    function initialize () {
        get_cluster_status();
    
        // hide the menu when we leave it
        $('#cmenu').mouseleave(function () {
            $('#cmenu').hide();
        });
    }
    
    function get_cluster_status () {
        $.ajax({
            method:'GET',
            url:'/admin/cluster/list',
            dataType:'json',
            success:update_cluster_status,
            failure:ping_cluster_status
        });
    }
    
    function perform_node_action (action) {
        $.ajax({
            url: action,
            dataType: 'json',
            complete: enable_adding,
            success: function(x,y,z) {
                if (x.result.toLowerCase() === 'ok') {
                    $('#add-node').val('');
                }
            },
            failure: function (err) { alert(err); }
        });
    }
    
    function disable_adding() {
        $('#add-node').addClass('disabled');
        $('#node-to-add').attr('disabled', 'disabled');
        $('#add-node-button').unbind('click');
    }
    
    function enable_adding () {
        $('#add-node').removeClass('disabled');
        $('#node-to-add').removeAttr('disabled', 'disabled');
        $('#add-node-button').bind('click', add_node);
    }
    
    function add_node () {
        disable_adding();
        perform_node_action('/admin/node/' + $('#node-to-add').val() + '/add');
    }
    
    function down_node () {
        perform_node_action('/admin/cluster/down/' + this_node)
    }
    
    function stop_node () {
        perform_node_action('/admin/node/' + this_node + '/stop');
        show_node_actions();
    }
    
    function leave_cluster () {
        perform_node_action('/admin/node/' + this_node + '/leave');
    }
    
    function show_node_actions (node) {
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
    
    function update_node_row (node, row) {
        var status = node.status.toLowerCase();
        $('.name', row).text(node.name);
        $('.status', row).text(node.status);
            
        // handle lights
        if (status === 'valid') {
            if (node.reachable === true) {
                $('.name', row).removeClass('offline');
                set_light_color($('.light-reachable .gui-light', row), 'green');
            } else if (node.reachable === false) {
                $('.name', row).addClass('offline');
                set_light_color($('.light-reachable .gui-light', row), 'red');
            }
        } else if (status === 'leaving') {
            //$('.name', row).addClass('offline');
            set_light_color($('.light-reachable .gui-light', row), 'orange');
        } else if (status === 'down') {
            //$('.name', row).addClass('offline')
            set_light_color($('.light-reachable .gui-light', row), 'gray');
        }
        
    }
    
    function cluster_node_row (node) {
        var id = node.name.split('@')[0];
        var rows = $('#cluster-table #' + id);
    
        // create a new row
        if (rows.length === 0) {
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
    
    function remove_node_rows (nodes) {
        var rows = $('#cluster-table .node'), i, r;
    
        // check to see if a node is listed in the cluster
        function node_in_cluster_p (node) {
            var j, l = nodes.length;
            for(j = 0; j < l; j += 1) {
                if (node === nodes[j].name) {
                    return true;
                }
            }
            return false;
        }
    
        // remove any node rows no long in the cluster
        r = rows.length;
        for(i = 0; i < 4; i += 1) {
            var node = $('.name', rows[i]).text();
    
            if (node_in_cluster_p(node) === false) {
                $(rows[i]).remove();
            }
        }
    }
    
    function update_cluster_status (nodes) {
        var html = '', i, l = nodes.length;
        
        $('#spinner').hide();
        $('#node-list').fadeIn(300);
        $('#total-number').html('(' + l + ' ' + ((l === 1)?'Node':'Nodes') + ' Total)');
    
        for(i = 0; i < l; i += 1) {
            cluster_node_row(nodes[i]);
        }
    
        remove_node_rows(nodes);
        
        resize_split_bar();
        
        // wait a little and update
        ping_cluster_status();
    }
    
    function ping_cluster_status () {
        setTimeout(get_cluster_status, 2000);
    }
    
    function resize_split_bar () {
        var splitbar = $('#split-bar');
        var parentheight = splitbar.parent().css('height');
        splitbar.css('height', parentheight);
    }
    
    resize_split_bar();
    initialize();
    enable_adding();
    
});