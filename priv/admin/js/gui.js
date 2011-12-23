// Framework for the cool UI tricks

// Set up a safe, global namespace in the jQuery object.
$.riakControl = $.riakControl || {};

$(function() {

    /* ALLOWS YOU TO HIT ENTER IN THE ADD-NODE FIELD WITHOUT MAKING IT A FORM */
    $(document).on('keyup', '#node-to-add', function (event) {
        if(event.keyCode === 13){
            $('#add-node-button').trigger('click');
        }
    });

    /* MAKE ALL CLOSE ERROR BUTTONS WORK */
    $(document).on('click', '.close-error', function () {
        $(this).parent().hide();
    });
    

    /* ENABLE THE SPLIT BAR */
    var splitBar = $('#split-bar');
    var splitBarParent = splitBar.parent();
    splitBar.css('height', splitBarParent.css('height'));
    
    /* If you need to resize the split bar when the window resizes
    var timer;
    $(window).resize(function(){
        clearTimeout(timer);
        timer = setTimeout(function(){
            var newHeight = splitBarParent.css('height');
            console.log(newHeight);
        }, 100);
    });
    */
    /* TURN ON TOGGLING FOR THE SPLIT BAR */
    splitBar.on('click', function () {
        var nav = $('#navigation'), navwidth = nav.css('width');
        var navbox = $('#nav-box'), boxwidth = navbox.css('width');
        if (navwidth === '218px') {
            nav.animate({"width":"54px"},{queue:false,duration:200});
            navbox.animate({"width":"62px"},{queue:false,duration:200});
        } else {
            nav.animate({"width":"218px"},{queue:false,duration:200});
            navbox.animate({"width":"226px"},{queue:false,duration:200});
        } 
    });
    
    
    
    /* HANDLE ACTIVE INDICATOR ANIMATION */
    $('.nav-li').on('click', function () {
        var me = $(this), indicator = $('#active-nav');
        indicator.animate({"top":me.position().top},{queue:false,duration:200});   
    });
    

    /* MAKE DROPDOWNS WORK */
    $(document).on('change', '.gui-dropdown', function (e) {
        var me = $(this), textSpot = me.prev().prev(), selected = me.find('option:selected').text();
        textSpot.text(selected);
    });
    
    
    /*
    MAKE ON/OFF SWITCHES WORK
    To set up: each div.gui-switch should contain two radio buttons.  One with an
    'off' value and one with an 'on' value.  Whichever is checked should be controlled
    by whether the node is actually on or off.
    */
    // Define a re-usable function to use any time a new node is created.
    // If the node is on or off, set switch class accordingly.
    function setGuiSwitch (i, e) {
        var me = $(e),
            isOn = (me.find('input[checked=checked]').attr('value').toLowerCase() === 'on');
        if (isOn) {
            me.removeClass('off').addClass('on');
        } else {
            me.removeClass('on').addClass('off');
        }
    }
    // When the document is ready, run setGuiSwitch on all on/off switches
    $('.gui-switch').each(setGuiSwitch);
    // When a switch changes, alter its class accordingly.
    $('.gui-switch input').on('change', function(e) {
        var that = $(this),
            theValue = that.attr('value').toLowerCase(),
            isChecked = (that.attr('checked') === 'checked'),
            theParent = that.closest('.gui-switch');
        if (theValue === 'on') {
            theParent.removeClass('off').addClass('on');
        } else if (theValue === 'off') {
            theParent.removeClass('on').addClass('off');
        }
    });
    // END CODE FOR ON/OFF SWITCHES
    
    
    /* MAKE CHECKBOXES WORK WHEN YOU CLICK THEM */
    $(document).on('change', '.gui-checkbox', function(e) {
        var me = $(this), parent = me.parent(); checked = me.attr('checked');
        if (checked) {
            parent.css('background-position', 'left top');
        } else {
            parent.css('background-position', 'left bottom');
        }
    });

    
    /* CODE FOR ALL THE TOOLTIPS */
    var wait;
    function emptyTips () {
        wait = setTimeout(function () {
            $('#tooltips').slideUp(function() {
                $('#display-tips').empty();
            });
        }, 500);
    }
    function displayTips (str) {
        var disp = $('#tooltips').css('display');
        if (disp === 'none') {
            $('#tooltips').slideDown();
        }
        $('#display-tips').html(str);
        clearTimeout(wait);
    }

    // Navigation

    // Basho Logo
    $(document).on('mouseover', '#basho-logo', function () {
        displayTips('Riak and Riak Control are products of Basho Technologies.  Visit our website to learn more.');
    }).on('mouseout', '#basho-logo', emptyTips);

    // Cluster link in navigation
    $(document).on('mouseover', '#nav-cluster', function () {
        displayTips('The cluster view allows you to add and remove nodes from your cluster as well as stop Riak on various nodes or mark them as down.  You will also be able to view percentages indicating how much ring data is owned by each node.');
    }).on('mouseout', '#nav-cluster', emptyTips);
    
    // Ring link in navigation
    $(document).on('mouseover', '#nav-ring', function () {
        displayTips('The ring view shows a list of your partitions indicating which node owns each partition.  You have the ability to apply filters to your view of the partitions and see indicators showing the status of various node workers, whether they are in active, fallback, or handoff states.');
    }).on('mouseout', '#nav-ring', emptyTips);

    // Snapshot link in navigation
    $(document).on('mouseover', '#nav-snapshot', function () {
        displayTips('Selecting the snapshot view commands Riak Control to gather some general information related to the current health of your cluster.  It is useful in quickly determining whether or not there are any issues to be concerned about.');
    }).on('mouseout', '#nav-snapshot', emptyTips);

    // Cluster View

    // Add new node area
    $(document).on('mouseover', '#add-node table', function () {
        displayTips('Type a node name (for example: dev2@127.0.0.1) into the text field and hit "Add Node" to add it to this cluster.  The node will then take ownership of partitions in the ring to help ensure balanced data across the cluster.');
    }).on('mouseout', '#add-node table', emptyTips);
    
    // Name of a node
    $(document).on('mouseover', '.node .name', function () {
        var name = $(this).text();
        displayTips('This node is named ' + name + '.  It is a member of this cluster.');
    }).on('mouseout', '.node .name', emptyTips);
    
    // View actions cluster sliders
    $(document).on('mouseover', '.more-actions-slider-box .gui-slider', function () {
        displayTips('Move the slider over to view possible actions for this node.  Move the slider back to hide those actions again.');
    }).on('mouseout', '.more-actions-slider-box .gui-slider', emptyTips);
    
    // Leave cluster button
    $(document).on('mouseover', '.leave-cluster-button', function () {
        displayTips('This will cause the node to begin relinquishing ownership of its data to other nodes in the cluster.  You will not be able to interact with the node via Riak Control during this process.  Once completed, Riak will shutdown on this node and it will leave the cluster.');
    }).on('mouseout', '.leave-cluster-button', emptyTips);
    
    // The 'Hosting Riak Control' message
    $(document).on('mouseover', '.current-host', function () {
        displayTips('This node cannot be shutdown or removed via Riak Control because it is currently hosting the app.');
    }).on('mouseout', '.current-host', emptyTips);
    
    // Markdown button
    $(document).on('mouseover', '.markdown-button', function () {
        displayTips('This button becomes active when the node becomes unreachable.  While in an unreachable state, a node may hinder cluster membership changes of other nodes.  Marking this node as "down" will allow other nodes to engage in membership changes unimpeded.');
    }).on('mouseout', '.markdown-button', emptyTips);

    // Shutdown button
    $(document).on('mouseover', '.shutdown-button', function () {
        displayTips('This button will stop the Riak process on this node.  If this button is not active, your node is already unreachable.  It will have to be restarted manually.');
    }).on('mouseout', '.shutdown-button', emptyTips);

    // Ring ownership percent
    $(document).on('mouseover', '.pct-box, .pct-arrows', function () {
        displayTips('The portion of the ring owned by this node.  When an arrow LED is lit, it is either in the process of receiving a partition from another node (up) or sending a partition to another node (down).');
    }).on('mouseout', '.pct-box, .pct-arrows', emptyTips);

    // Memory usage
    $(document).on('mouseover', '.membar-bg, .free-memory', function () {
        var parent = $(this).parent(), 
            free_mem = parent.find('.free-memory').text(),
            erlang_mem, non_erlang_mem;
        if (free_mem.charAt(0) === '?') {
            displayTips('Because this node is currently unreachable, Riak Control is not able to assess its memory usage.');
        } else {
            free_mem = parseInt(free_mem);
            erlang_mem = parseInt(parent.find('.erlang-mem').attr('name'));
            non_erlang_mem = parseInt(parent.find('.non-erlang-mem').attr('name'));
            displayTips('The machine running this node currently has ' + free_mem + '% free memory.  Of the ' + (erlang_mem + non_erlang_mem) + '% currently in use, ' + erlang_mem + '% is being used by Riak and ' + non_erlang_mem + '% is being used by other processes.');
        }
    }).on('mouseout', '.membar-bg, .free-memory', emptyTips);
    
    // Node status
    $(document).on('mouseover', '.status-box', function () {
        var mytext = $(this).find('.status').text().toLowerCase();
        if (mytext === 'valid') {
            displayTips('This node is currently online and working.');
        } else if (mytext === 'unreachable') {
            displayTips('This node is unreachable.  Riak may need to be restarted or there may be other connectivity issues.  Cluster membership changes like "join" and "leave" cannot complete until this node is reachable or marked as "down".');
        } else if (mytext === 'down') {
            displayTips('This node has been marked as "down". While in this state it can not be interacted with but it will not impede cluster membership changes of other nodes.  To return to a "valid" state, simply restart Riak on this node.');
        } else if (mytext === 'leaving') {
            displayTips('This node is in process of leaving the cluster.  When it has finished relinquishing ownership and transferring data to other nodes, Riak will stop on this node and it will cease to be a member of the cluster.  You can not interact with this node during this process.');
        }
    }).on('mouseout', '.status-box', emptyTips);

    // Ring View

    // Ring filter
    $(document).on('mouseover', '#ring-filter', function () {
        displayTips('Select a node from the dropdown to view only the partitions owned by that node.  If you uncheck the boxes next to "Primary" or "Fallback", partitions in primary or fallback states will be removed from the view.');
    }).on('mouseout', '#ring-filter', emptyTips);

    // Partition owner
    $(document).on('mouseover', '.owner', function () {
        var partitionIndex = $(this).next().text();
        displayTips('The name of the node that owns this partition. (Partition Index: ' + partitionIndex + ')');
    }).on('mouseout', '.owner', emptyTips);

    // Worker Lights
    $(document).on('mouseover', '.kv-light, .pipe-light, .search-light', function () {
        var that = $(this);
        var texts;

        if (that.hasClass('kv-light')) {
            texts = {"status": that.find('.kv-status').text().toLowerCase(), "msg" : "Key-Value Store: "};
        } else if (that.hasClass('pipe-light')) {
            texts = {"status": that.find('.pipe-status').text().toLowerCase(), "msg" : "Pipeline Queue Process: "};
        } else if (that.hasClass('search-light')) {
            texts = {"status": that.find('.search-status').text().toLowerCase(), "msg" : "Riak Search: "};
        }

        if (texts.status === 'active') {
            displayTips(texts.msg + 'This node worker is active and ready.');
        } else if (texts.status === 'fallback') {
            displayTips(texts.msg + 'This node worker is not currently active.  This may be because its owner node is down or unreachable.  Operations normally handled by this node are currently in fallback to another node.');
        } else if (texts.status === 'handoff') {
            displayTips(texts.msg + 'This node worker is currently in the process of handing off its data to other nodes.');
        }
    }).on('mouseout', '.kv-light, .pipe-light, .search-light', emptyTips);
    
    
        
    
});