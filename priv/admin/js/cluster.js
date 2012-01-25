// polls the ring status every so often

$(document).ready(function () {

    var this_node = undefined;

    var stopping = {};

    var pingAllowed = true;




    /* MAKE SLIDING SWITCHES WORK */
    // Define a function that will show the proper message when the slider moves
    function showMsg(elem) {
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
            start : function () {
                // Disallow pings while the slider handle is dragging
                pingAllowed = false;
            },
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
                var siblingRowName = $(this).closest('tr').attr('name') + '-more-actions';
                // Re-allow pings when we let go of the slider handle
                // And tell it to ping since the ping loop will have died
                pingAllowed = true;
                get_cluster_status();
                if (handlePos === '100%') {
                    //leave_cluster(node);
                    open_sibling_row(siblingRowName, node);
                } else if (parseInt(handlePos) < (me.width() * .66)) {
                    myHandle.animate({left:'0px'},{
                        queue:false,
                        duration:200,
                        complete:function() {
                            showMsg($(this).parent());
                            close_sibling_row(siblingRowName);
                        }
                    });
                } else {
                    myHandle.animate({left:'100%'},{
                        queue:false,
                        duration:200,
                        complete:function () {
                            var node = $(this).closest('tr').find('.name').text();
                            var siblingRowName = $(this).closest('tr').attr('name') + '-more-actions';
                            showMsg($(this).parent());
                            open_sibling_row(siblingRowName, node);
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
                duration:200,
                complete:function () {
                    var node = $(this).closest('tr').find('.name').text();
                    var siblingRowName = $(this).closest('tr').attr('name') + '-more-actions';
                    showMsg($(this).parent());
                    open_sibling_row(siblingRowName, node);
                }
            });
        }
    });
    // END CODE FOR SLIDING SWITCHES

    function open_sibling_row(nameText, node) {
        var row = $('[name="' + nameText + '"]');
        var actionsPointer = row.find('.actions-pointer');
        var actionsBox = row.find('.actions-box');
        row.show(function () {
            actionsPointer.slideDown(100);
            actionsBox.slideDown(200);
        });
    }

    function close_sibling_row(nameText) {
        var row = $('[name="' + nameText + '"]');
        var actionsBox = row.find('.actions-box');
        var actionsPointer = row.find('.actions-pointer');
        actionsPointer.slideUp(200);
        actionsBox.slideUp(200);
    }
    
    function initialize() {
        get_cluster_status();
    }

    function get_cluster_status() {
        $.ajax({
            method:'GET',
            url:'/admin/cluster/list',
            dataType:'json',
            success: function (d) {
                update_cluster_status(d);
            },
            failure: ping_cluster_status
        });
    }

    function perform_node_action(action) {
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

    function enable_adding(clear) {
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

    function add_node() {
        perform_node_action('/admin/cluster/join/' + $('#node-to-add').val());
    }

    function down_node(node) {
        perform_node_action('/admin/cluster/down/' + (node || this_node))
    }

    function stop_node(node) {
        stopping[node] = true;
        perform_node_action('/admin/node/' + (node || this_node) + '/stop');
    }

    function leave_cluster(node) {
        perform_node_action('/admin/node/' + (node || this_node) + '/leave');
    }

    function set_light_color(jqObj, newColor) {
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

    function set_operability_class(jqObj, newClass) {
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

    function set_handoff_light_class(jqObj, newClass) {
        var classes = ['pct-static', 'pct-gaining', 'pct-losing'], i, l = classes.length;
        newClass = newClass.toLowerCase();
        for (i = 0; i < l; i += 1) {
            if (classes[i] === newClass) {
                jqObj.addClass(newClass);
            } else {
                jqObj.removeClass(classes[i]);
            }
        }
    }

    function reset_slider(sliderHandle, rowNode) {
        var myActions = $('[name="' + rowNode.getAttribute('name') + '-more-actions"]');
        var hiddenActions = $('.actions-box', myActions[0]).css('display') !== 'block';
        if (sliderHandle.css('left') === '100%' && hiddenActions) {
            sliderHandle.css('left', '0');
            sliderHandle.parent().find('.isLeft').fadeIn(200);
            sliderHandle.parent().find('.isRight').hide();
            $('.gui-slider-groove', rowNode).trigger('initSlider');
        }
    }
    
    function set_leaving_status(row, textObj) {
        var myActions = (row) ? $('[name="' + row.getAttribute('name') + '-more-actions"]') : null;
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
    
    function set_down_status(row, textObj) {
        var myActions = (row) ? $('[name="' + row.getAttribute('name') + '-more-actions"]') : null;
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
    
    function set_valid_reachable_status(row, textObj) {
        var myActions = (row) ? $('[name="' + row.getAttribute('name') + '-more-actions"]') : null;
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
                set_light_color($('.status-light', row), 'green');
            }
            set_operability_class($('.name', row), 'normal');
            set_operability_class($('.ring_pct', row), 'normal');
            set_operability_class($('.pending_pct', row), 'normal');

        }
    }
    
    function set_valid_unreachable_status(row, textObj) {
        var myActions = (row) ? $('[name="' + row.getAttribute('name') + '-more-actions"]') : null;
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
    
    function set_host_node_status(row, textObj) {
        $('.markdown-button', row).addClass('hide');
        if (textObj.slider !== 'Hosting Riak Control') { 
            $('.more-actions-slider-box', row).html('<a class="current-host gui-text">Hosting Riak Control</a>'); 
        }
        if (textObj.status !== 'Valid') {
            $('.status', row).text('Valid');
        }
        set_operability_class($('.name', row), 'normal');
        set_light_color($('.status-light', row), 'green');
    }

    function round_pct(num, decPlaces) {
        return Math.round(num*Math.pow(10,decPlaces))/Math.pow(10,decPlaces);
    }

    function set_pct(node, texts, row) {
        node.ring_pct = round_pct(node.ring_pct * 100, 0);
        node.pending_pct = round_pct(node.pending_pct * 100, 0);

        if (texts.name !== node.name) {
            $('.name', row).text(node.name);
        }
        if (texts.ring_pct !== node.ring_pct) {
            $('.ring_pct', row).text(node.ring_pct + '%');
        }

        if (node.ring_pct > node.pending_pct) {
            set_handoff_light_class($('.pct-arrows', row), 'pct-losing');
        } else if (node.ring_pct < node.pending_pct) {
            set_handoff_light_class($('.pct-arrows', row), 'pct-gaining');
        } else {
            if (!$('.pct-arrows', row).hasClass('pct-static')) {
                set_handoff_light_class($('.pct-arrows', row), 'pct-static');
                $('.green-pct-arrow', row).show(0, function () {
                    $(this).fadeOut(2000);
                });
            } else {
                set_handoff_light_class($('.pct-arrows', row), 'pct-static');
            }
        }
    }

    function set_memory(node, texts, row) {
        var memdivider, mem_erlang, mem_non_erlang, mem_free;

        if (node.reachable === false) {
            $('.unknown-mem', row).show();
            $('.mem-color', row).hide();
            $('.free-memory', row).text('?? Free');
        } else {
            memdivider = node.mem_total / 100;
            mem_erlang = Math.ceil(node.mem_erlang / memdivider);
            mem_non_erlang = Math.round((node.mem_used / memdivider) - mem_erlang);
            mem_free = Math.round((node.mem_total - node.mem_used) / memdivider);

            $('.unknown-mem', row).hide();
            $('.mem-color', row).show();
        
            if (texts.mem_erlang !== mem_erlang) {
                $('.erlang-mem', row).attr('name', mem_erlang).css('width', mem_erlang + '%');
            }
            if (texts.mem_non_erlang !== mem_non_erlang) {
                $('.non-erlang-mem', row).attr('name', mem_non_erlang).css('width', mem_non_erlang + '%');
            }
            if (texts.mem_free !== mem_free) {
                $('.free-memory', row).text(mem_free + '% Free');
            }
        }
    }

    function dispatch_to_status_funcs(status, node, texts, row) {
        // if the node is the one hosting the console you cannot eff with it
        if (node.me === true) {
            set_host_node_status(row, texts);
        } else {
            // handle colors and operability
            if (status === 'valid' && !stopping[texts.name]) {
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

    function update_node_row(node, row) {
        var status = node.status.toLowerCase();
        var texts = {
            "status" : $('.status', row).text(),
            "name"   : $('.name', row).text(),
            "slider" : $('.more-actions-slider-box a', row).text(),
            "ring_pct" : $('.ring_pct', row).text(),
            "mem_erlang" : $('.erlang-mem', row).attr('name'),
            "mem_non_erlang" : $('.non-erlang-mem'. row).attr('name'),
            "mem_free" : parseInt($('.free-memory', row).text())
        };

        if (!node.reachable) {
            // Once a node actually shows up as being unreachable we can
            // unflag it as a node that is currently stopping.
            delete stopping[texts.name];
        }

        set_pct(node, texts, row);
        set_memory(node, texts, row);
        dispatch_to_status_funcs(status, node, texts, row);

        $('.gui-slider-groove').trigger('initSlider');
    }

    function cluster_node_row(node) {
        //var id = node.name.split('@')[0];
        var nodename = node.name;
        var rows = $('#cluster-table [name="' + nodename + '"]') || null;
        var row, extraRow;

        //console.log(node);

        // create a new row
        if (rows.length === 0) {
            row = $('.row-template').clone();
            extraRow = $('.more-actions-template').clone();


            // set the id for this row and display it
            row.attr('name', nodename);
            extraRow.attr('name', nodename + '-more-actions');
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

    function remove_node_rows(nodes) {
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
                theSibling = $('[name="' + rows[i].getAttribute('name') + '-more-actions"]');
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

    function ping_cluster_status() {
        setTimeout(function () {
            if ($('#cluster-headline').length && pingAllowed === true) {
                get_cluster_status();
            } else {
                // If we're no longer on the cluster page, we disallow pinging and
                // the script dies here.
                pingAllowed = false;
            }
        }, 1000);
    }

    function update_cluster_status(nodes) {
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
        var siblingName = $(this).closest('tr').attr('name');
        var node = $('[name="' + siblingName.split('-more-actions')[0] + '"]');
        close_sibling_row(siblingName);
        set_down_status(node[0], {"status" : node.find('.status').text()}, true);
        down_node(node.find('.name').text());
    });

    /* MAKE THE SHUTDOWN BUTTON STAY DOWN ONCE CLICKED */
    $(document).on('click', '.shutdown-button:not(.pressed)', function () {
        var siblingName = $(this).closest('tr').attr('name');
        var node = $('[name="' + siblingName.split('-more-actions')[0] + '"]');
        set_valid_unreachable_status(node[0], {"status" : node.find('.status').text()});
        stop_node(node.find('.name').text());
    });
    
    /* MAKE THE LEAVE CLUSTER LINK WORK */
    $(document).on('click', '.leave-cluster-button:not(.disabled)', function () {
        var myParentName = $(this).closest('tr').attr('name');
        var node = $('[name="' + myParentName.split('-more-actions')[0] + '"]');
        var name = node.find('.name').text();
        close_sibling_row(myParentName);
        set_leaving_status(node[0], {"status" : node.find('.status').text()});
        leave_cluster(name);
    });

    initialize();
    enable_adding();

    // Subscribe to the 'templateSwitch' event.
    // As soon as we switch back to the cluster page, we re-initialize
    $.riakControl.sub('templateSwitch', function (templateName) {
        if (templateName === 'cluster') {
            pingAllowed = true;
            initialize();
        }
    });

});