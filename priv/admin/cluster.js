// polls the ring status every so often


$(document).ready(function () {

    var this_node = undefined;




    /* MAKE SLIDING SWITCHES WORK */
    // Define a function that will show the proper message when the slider moves
    function showMsg (elem) {
        var me = elem;
        var myHandle = me.find('.ui-slider-handle');
        var handlePos = parseInt(myHandle.css('left'));
        var myMsg = me.parent().find('.gui-slider-msg');
        if (handlePos === 100) {
            myMsg.filter('.isRight').fadeIn(200);
        } else if (handlePos === 0) {
            myMsg.filter('.isLeft').fadeIn(200);
        }
    }
    // Enable jQuery UI slider method on divs classed 'gui-slider-groove'.
    // Contains some extra handling for when someone lets go of the slider
    // before it has moved all the way over and for handling the message
    // as well.
    $('.gui-slider-groove').live('initSlider', function () {
        $(this).slider({
            slide : function() {
                var me = $(this);
                var myMsg = me.parent().find('.gui-slider-msg');
                myMsg.fadeOut(200);
            },
            change: function() {
                showMsg($(this));
            },
            stop  : function() {
                var me = $(this);
                var myHandle = me.find('.ui-slider-handle');
                var handlePos = myHandle.css('left');
                var node = $(this).closest('tr').find('.name').text();
                if (handlePos === '100%') {
                    leave_cluster(node);
                } else if (parseInt(handlePos) < (me.width() * .66)) {
                    myHandle.animate({left:'0px'},{
                        queue:false,
                        duration:200,
                        complete:function() {
                            showMsg($(this).parent());
                        }
                    });
                } else {
                    myHandle.animate({left:'100%'},{
                        queue:false,
                        duration:200,
                        complete:function () {
                            var node = $(this).closest('tr').find('.name').text();
                            showMsg($(this).parent());
                            leave_cluster(node);
                        }
                    });
                }
            }
        });
        $(this).find('a').removeAttr('href');
    });
    /*
    Enable this section if your slider message container does not extend to the edge
    of the slider.
    */
    $('.gui-slider-activate').live('click', function() {
        var me = $(this);
        var myHandle = me.next('.gui-slider-groove').find('.ui-slider-handle');
        var handlePos = parseInt(myHandle.css('left'));
        me.next('.gui-slider-groove').find('.gui-slider-msg').fadeOut(200);
        if (handlePos < 100) {
            myHandle.animate({left:'100%'},{
                queue:false,
                duration:1000,
                complete:function () {
                    var node = $(this).closest('tr').find('.name').text();
                    showMsg($(this).parent());
                    leave_cluster(node);
                }
            });
        }
    });
    // END CODE FOR SLIDING SWITCHES



    
    function initialize () {
        get_cluster_status();
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
            complete: function (x,y) {
                var err, errortextbox, errorlinkbox;
                console.log(x);
                if (y.toLowerCase() === 'error') {
                    err = x.responseText.split('<title>')[1].split('</title>')[0] + ' <a class="monospace">-></a> ' + this.url + '.';
                    $('#node-error .error-text').html(err);
                    $('#node-error .error-link').html('View in Logs &raquo;')
                    $('#node-error').show();
                }
                enable_adding();
            },
            success: function(res) {
                if (res.result.toLowerCase() === 'ok') {
                    $('#node-to-add').val('');
                }
            }
        });
    }

    function disable_adding() {
        $('#add-node').addClass('disabled');
        $('#node-to-add').attr('disabled', 'disabled');
        $('#add-node-button').unbind('click');
    }

    function enable_adding () {
        var button = $('#add-node-button');
        var boundevents = button.data('events');
        $('#add-node').removeClass('disabled');
        $('#node-to-add').removeAttr('disabled', 'disabled');
        if (!boundevents || !boundevents.click || boundevents.click.length < 1) {
            button.bind('click', function () {
                $('#node-error').hide();
                if ($('#node-to-add').val().length) {
                    add_node();
                }
            });
        }
    }

    function add_node () {
        disable_adding();
        perform_node_action('/admin/cluster/join/' + $('#node-to-add').val());
    }

    function down_node (node) {
        perform_node_action('/admin/cluster/down/' + (node || this_node))
    }

    function stop_node () {
        perform_node_action('/admin/node/' + this_node + '/stop');
        show_node_actions();
    }

    function leave_cluster (node) {
        perform_node_action('/admin/node/' + (node || this_node) + '/leave');
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

    function set_operability_class (jqObj, newClass) {
        var classes = ['offline', 'disabled', 'down', 'normal'], i, l = classes.length;
        newClass = newClass.toLowerCase();
        for (i = 0; i < l; i += 1) {
            if (classes[i] === newClass) {
                jqObj.addClass(newClass);
            } else {
                jqObj.removeClass(classes[i]);
            }
        }
    }

    function update_node_row (node, row) {
        var status = node.status.toLowerCase();
        $('.name', row).text(node.name);
        $('.status', row).text(node.status);
        $('.gui-slider-groove').trigger('initSlider');
        
        // if the node is the one hosting the console you cannot eff with it
        if (node.me === true) {
            //$(row).attr('name', 'host');
            $('.markdown-button', row).addClass('hide');
            $('.leave-box', row).html('<a class="current-host gui-text">Hosting Riak Control</a>');
            set_operability_class($('.name', row), 'normal');
            set_light_color($('.gui-light', row), 'green');
        } else {
            //$(row).attr('name', '');
            
            // handle colors and operability
            if (status === 'valid') {
                if (node.reachable === true) {
                    $('.markdown-button', row).addClass('hide');
                    $('.gui-slider', row).removeClass('hide');
                    set_operability_class($('.name', row), 'normal');
                    set_light_color($('.gui-light', row), 'green');
                } else {
                    $('.markdown-button', row).removeClass('hide').removeClass('pressed');
                    $('.gui-slider', row).addClass('hide');
                    set_light_color($('.gui-light', row), 'red');
                    set_operability_class($('.name', row), 'offline');
                }
            } else if (status === 'leaving') {
                set_light_color($('.gui-light', row), 'orange');
                $('.gui-slider', row).addClass('hide');
                $('.gui-slider-leaving', row).removeClass('hide');
                $('.gui-rect-button-leaving', row).removeClass('hide');
                set_operability_class($('.status', row), 'disabled');
                set_operability_class($('.name', row), 'disabled');
            } else if (status === 'down') {
                $('.markdown-button', row).removeClass('hide').addClass('pressed');
                set_operability_class($('.name', row), 'down');
                set_light_color($('.gui-light', row), 'gray');
            }
            
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

        // wait a little and update
        ping_cluster_status();
    }

    function ping_cluster_status () {
        setTimeout(get_cluster_status, 2000);
    }

    /* MAKE THE MARKDOWN BUTTON STAY DOWN ONCE CLICED */
    $('.markdown-button').live('click', function () {
        var node = $(this).closest('tr').find('.name').text();
        down_node(node);
        $(this).addClass('pressed');
    });

    initialize();
    enable_adding();

});