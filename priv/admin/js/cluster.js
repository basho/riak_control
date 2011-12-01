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
        if (handlePos > 99) {
            myMsg.filter('.isRight').fadeIn(200);
        } else if (handlePos === 0) {
            myMsg.filter('.isLeft').fadeIn(200);
        }
    }
    // Enable jQuery UI slider method on divs classed 'gui-slider-groove'.
    // Contains some extra handling for when someone lets go of the slider
    // before it has moved all the way over and for handling the message
    // as well.
    $(document).on('initSlider', '.gui-slider-groove', function () {
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
                var siblingRowID = $(this).closest('tr').attr('id') + '-more-actions';
                if (handlePos === '100%') {
                    //leave_cluster(node);
                    open_sibling_row(siblingRowID, node);
                } else if (parseInt(handlePos) < (me.width() * .66)) {
                    myHandle.animate({left:'0px'},{
                        queue:false,
                        duration:200,
                        complete:function() {
                            showMsg($(this).parent());
                            close_sibling_row(siblingRowID);
                        }
                    });
                } else {
                    myHandle.animate({left:'100%'},{
                        queue:false,
                        duration:200,
                        complete:function () {
                            var node = $(this).closest('tr').find('.name').text();
                            var siblingRowID = $(this).closest('tr').attr('id') + '-more-actions';
                            showMsg($(this).parent());
                            open_sibling_row(siblingRowID, node);
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
    $(document).on('click', '.gui-slider-activate', function() {
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
                    var siblingRowID = $(this).closest('tr').attr('id') + '-more-actions';
                    showMsg($(this).parent());
                    open_sibling_row(siblingRowID, node);
                }
            });
        }
    });
    // END CODE FOR SLIDING SWITCHES

    function open_sibling_row(idText, node) {
        var row = $('#' + idText);
        var actionsPointer = row.find('.actions-pointer');
        var actionsBox = row.find('.actions-box');
        row.show(function () {
            actionsPointer.slideDown(100);
            actionsBox.slideDown(200);
        });
    }

    function close_sibling_row(idText) {
        var row = $('#' + idText);
        var actionsBox = row.find('.actions-box');
        var actionsPointer = row.find('.actions-pointer');
        actionsPointer.slideUp(200);
        actionsBox.slideUp(200);
    }
    
    function initialize () {
        get_cluster_status();
    }

    function get_cluster_status () {
        $.ajax({
            method:'GET',
            url:'/admin/cluster/list',
            dataType:'json',
            success: function (d) {
                update_cluster_status(d);
            },
            failure:ping_cluster_status
        });
    }

    function perform_node_action (action) {
        $.ajax({
            url: action,
            dataType: 'json',
            complete: function (x,y) {
                var err, errortextbox, errorlinkbox;
                if (y.toLowerCase() === 'error') {
                    err = x.responseText.split('<title>')[1].split('</title>')[0] + ' <a class="monospace">-></a> ' + this.url + '.';
                    $('#node-error .error-text').html(err);
                    $('#node-error .error-link').html('View in Logs &raquo;')
                    $('#node-error').show();
                }
                enable_adding((y.toLowerCase() === 'success') ? true : false);
            },
            success: function(res) {
                if (res.result.toLowerCase() === 'ok') {
                    $('#node-to-add').val('');
                }
            }
        });
    }

    function enable_adding (clear) {
        var nodeToAdd = $('#node-to-add');
        $('#add-node-button').removeClass('pressed').removeClass('disabled');
        nodeToAdd.removeAttr('disabled').removeClass('disabled');
        if (clear === true) {
            nodeToAdd.val('');
        } else {
            $('#node-to-add').trigger('focus');
        }
        
        // Make super sure that event handlers are not accumulating...
        $(document).off('click', '#add-node-button');
        
        $(document).one('click', '#add-node-button', function () {
            var nodeToAdd = (nodeToAdd && nodeToAdd.length) ? nodeToAdd : $('#node-to-add');
            $('#node-error').hide();
            if (nodeToAdd.val()) {
                $(this).addClass('pressed').addClass('disabled');
                nodeToAdd.attr('disabled', 'disabled').addClass('disabled');
                add_node();
            } else {
                enable_adding();
            }
        });
    }

    function add_node () {
        perform_node_action('/admin/cluster/join/' + $('#node-to-add').val());
    }

    function down_node (node) {
        perform_node_action('/admin/cluster/down/' + (node || this_node))
    }

    function stop_node (node) {
        perform_node_action('/admin/node/' + (node || this_node) + '/stop');
    }

    function leave_cluster (node) {
        perform_node_action('/admin/node/' + (node || this_node) + '/leave');
    }

    function set_light_color (jqObj, newColor) {
        var colors = ['green', 'gray', 'orange', 'red', 'blue'], i, l = colors.length;
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
        var classes = ['unreachable', 'disabled', 'down', 'normal'], i, l = classes.length;
        newClass = newClass.toLowerCase();
        for (i = 0; i < l; i += 1) {
            if (classes[i] === newClass) {
                jqObj.addClass(newClass);
            } else {
                jqObj.removeClass(classes[i]);
            }
        }
    }

    function reset_slider (sliderHandle, rowNode) {
        var myActions = $('#' + rowNode.id + '-more-actions');
        var hiddenActions = $('.actions-box', myActions[0]).css('display') !== 'block';
        if (sliderHandle.css('left') === '100%' && hiddenActions) {
            sliderHandle.css('left', '0');
            sliderHandle.parent().find('.isLeft').fadeIn(200);
            sliderHandle.parent().find('.isRight').hide();
            $('.gui-slider-groove', rowNode).trigger('initSlider');
        }
    }
    
    function set_leaving_status (row, textObj) {
        var myActions = (row) ? $('#' + row.id + '-more-actions') : null;
        var slider = $('.gui-slider', row);
        var slider_leaving = $('.gui-slider-leaving', row);
        if (myActions) {
            set_light_color($('.status-light', row), 'orange');

            if (textObj.status !== 'Leaving') {
                $('.status', row).text('Leaving');
            }

            slider.addClass('hide');
            slider_leaving.removeClass('hide').removeClass('down');

            myActions.find('.markdown-button, .markdown-label').addClass('disabled').addClass('pressed');
            myActions.find('.shutdown-button, .shutdown-label').addClass('disabled').addClass('pressed');
            myActions.find('.leave-cluster-button, .leave-cluster-label').addClass('disabled').addClass('pressed');

            set_operability_class($('.status', row), 'disabled');
            set_operability_class($('.name', row), 'disabled');
            set_operability_class($('.ring_pct', row), 'disabled');
            set_operability_class($('.pending_pct', row), 'disabled');
        }
    }
    
    function set_down_status (row, textObj) {
        var myActions = (row) ? $('#' + row.id + '-more-actions') : null;
        if (myActions) {
            if (textObj.status !== 'Down') {
                $('.status', row).text('Down');
            }

            $('.gui-slider', row).addClass('hide');
            $('.gui-slider-leaving', row).removeClass('hide').addClass('down');

            myActions.find('.markdown-button, .markdown-label').removeClass('disabled').addClass('pressed');
            myActions.find('.shutdown-button, .shutdown-label').addClass('disabled').addClass('pressed');
            myActions.find('.leave-cluster-button, .leave-cluster-label').addClass('disabled').addClass('pressed');

            set_operability_class($('.name', row), 'down');
            set_operability_class($('.status', row), 'down');
            set_operability_class($('.ring_pct', row), 'down');
            set_operability_class($('.pending_pct', row), 'down');
            set_light_color($('.status-light', row), 'gray');
        }
    }
    
    function set_valid_reachable_status (row, textObj) {
        var myActions = (row) ? $('#' + row.id + '-more-actions') : null;
        var sliderHandle = $('.ui-slider-handle', row);

        if (myActions) {
            myActions.find('.markdown-button, .markdown-label').addClass('disabled').addClass('pressed');
            myActions.find('.shutdown-button, .shutdown-label').removeClass('disabled').removeClass('pressed');
            myActions.find('.leave-cluster-button, .leave-cluster-label').removeClass('disabled').removeClass('pressed');

            $('.gui-slider', row).removeClass('hide');
            $('.gui-slider-leaving', row).addClass('hide');
            reset_slider($('.ui-slider-handle', row), row);

            if (textObj.status !== 'Valid') {
                $('.status', row).text('Valid');
                set_operability_class($('.status', row), 'normal');
            }
            set_operability_class($('.name', row), 'normal');
            set_operability_class($('.ring_pct', row), 'normal');
            set_operability_class($('.pending_pct', row), 'normal');

            //set_light_color($('.status-light', row), 'green');
        }
    }
    
    function set_valid_unreachable_status (row, textObj) {
        var myActions = (row) ? $('#' + row.id + '-more-actions') : null;
        if (myActions) {
            myActions.find('.markdown-button, .markdown-label').removeClass('disabled').removeClass('pressed');
            myActions.find('.shutdown-button, .shutdown-label').addClass('pressed').addClass('disabled');
            myActions.find('.leave-cluster-button, .leave-cluster-label').addClass('pressed').addClass('disabled');
            $('.gui-slider', row).removeClass('hide');
            $('.gui-slider-leaving', row).addClass('hide');
            if (textObj.status !== 'Unreachable') {
                $('.status', row).text('Unreachable');
            }
            reset_slider($('.ui-slider-handle', row), row);
            set_light_color($('.status-light', row), 'red');
            set_operability_class($('.name', row), 'unreachable');
        }
    }
    
    function set_host_node_status (row, textObj) {
        $('.markdown-button', row).addClass('hide');
        if (textObj.slider !== 'Hosting Riak Control') { 
            $('.more-actions-slider-box', row).html('<a class="current-host gui-text">Hosting Riak Control</a>'); 
        }
        if (textObj.status !== 'Valid') {
            $('.status', row).text('Valid');
        }
        set_operability_class($('.name', row), 'normal');
        //set_light_color($('.status-light', row), 'green');
    }

    function round_pct (num, decPlaces) {
        return Math.round(num*Math.pow(10,decPlaces))/Math.pow(10,decPlaces);
    }

    function update_node_row (node, row) {
        var status = node.status.toLowerCase();
        var texts = {
            "status" : $('.status', row).text(),
            "name"   : $('.name', row).text(),
            "slider" : $('.more-actions-slider-box a', row).text(),
            "ring_pct" : $('.ring_pct', row).text(),
            "pending_pct" : $('.pending_pct', row).text()
        };

        node.ring_pct = round_pct(node.ring_pct * 100, 1) + '%';
        node.pending_pct = round_pct(node.pending_pct * 100, 1) + '%';

        if (texts.name !== node.name) {
            $('.name', row).text(node.name);
        }
        if (texts.ring_pct !== node.ring_pct) {
            $('.ring_pct', row).text(node.ring_pct);
        }
        if (texts.pending_pct !== node.pending_pct) {
            $('.pending_pct', row).text(node.pending_pct);
        }
        if ($('.ring_pct', row).text() !== $('.pending_pct', row).text() && status !== 'leaving') {
            set_light_color($('.status-light', row), 'orange');
        } else {
            if (node.reachable && status !== 'leaving' && status !== 'joining' && status !== 'down') {
                set_light_color($('.status-light', row), 'green');
            }
        }

        $('.gui-slider-groove').trigger('initSlider');
        
        // if the node is the one hosting the console you cannot eff with it
        if (node.me === true) {
            set_host_node_status(row, texts);
        } else {
            // handle colors and operability
            if (status === 'valid') {
                if (node.reachable === true) {
                    set_valid_reachable_status(row, texts);
                } else {
                    set_valid_unreachable_status(row, texts);
                }
            } else if (status === 'leaving') {
                set_leaving_status(row, texts);
            } else if (status === 'down') {
                set_down_status(row, texts);
            }
            
        }
    }

    function cluster_node_row (node) {
        var id = node.name.split('@')[0];
        var rows = $('#cluster-table #' + id) || null;
        var row, extraRow;

        // create a new row
        if (rows.length === 0) {
            row = $('.row-template').clone();
            extraRow = $('.more-actions-template').clone();


            // set the id for this row and display it
            row.attr('id', id);
            extraRow.attr('id', id + '-more-actions');
            row.removeClass('row-template');
            extraRow.removeClass('more-actions-template');

            // add it to the table
            $('#cluster-table').append(row).append(extraRow);

            // initialize the row
            update_node_row(node, row[0]);

            // and lastly, show it
            row.show();

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

        function check_and_remove (i) {
            var nodeName = $('.name', rows[i]).text(),
                theRow = $(rows[i]),
                theSibling;
            if (theRow.length) {
                theSibling = $('#' + rows[i].id + '-more-actions');
            }
            if (node_in_cluster_p(nodeName) === false) {
                theRow.remove();
                if (theSibling) {
                    theSibling.remove();
                }
            }
        }

        // remove any node rows no long in the cluster
        r = rows.length;
        for(i = 0; i < 4; i += 1) {
            check_and_remove(i);
        }

    }

    function ping_cluster_status () {
        setTimeout(function () {
            if ($('#cluster-headline').length) {
                get_cluster_status();
            } else {
                ping_cluster_status();
            }
        }, 1000);
    }

    function update_cluster_status (nodes) {
        var html = '', i, l = nodes.length;

        if ($('#cluster-headline').length) {
            $('#total-number').html('(' + l + ' ' + ((l === 1)?'Node':'Nodes') + ' Total)');
        }

        for(i = 0; i < l; i += 1) {
            cluster_node_row(nodes[i]);
        }

        remove_node_rows(nodes);
        
        $('#cluster-spinner').hide();
        $('#node-list').fadeIn(300);

        $('.gui-slider-groove').trigger('initSlider');

        // wait a little and update
        ping_cluster_status();
    }

    /* MAKE THE MARKDOWN BUTTON STAY DOWN ONCE CLICKED */
    $(document).on('click', '.markdown-button:not(.pressed)', function () {
        var siblingId = $(this).closest('tr').attr('id');
        var node = $('#' + siblingId.split('-more-actions')[0]);
        close_sibling_row(siblingId);
        set_down_status(node[0], {"status" : node.find('.status').text()}, true);
        down_node(node.find('.name').text());
    });

    /* MAKE THE SHUTDOWN BUTTON STAY DOWN ONCE CLICKED */
    $(document).on('click', '.shutdown-button:not(.pressed)', function () {
        var siblingId = $(this).closest('tr').attr('id');
        var node = $('#' + siblingId.split('-more-actions')[0]);
        set_valid_unreachable_status(node[0], {"status" : node.find('.status').text()});
        stop_node(node.find('.name').text());
    });
    
    /* MAKE THE LEAVE CLUSTER LINK WORK */
    $(document).on('click', '.leave-cluster-button:not(.disabled)', function () {
        var myParentID = $(this).closest('tr').attr('id');
        var node = $('#' + myParentID.split('-more-actions')[0]);
        var name = node.find('.name').text();
        close_sibling_row(myParentID);
        set_leaving_status(node[0], {"status" : node.find('.status').text()});
        leave_cluster(name);
    });

    initialize();
    enable_adding();

});