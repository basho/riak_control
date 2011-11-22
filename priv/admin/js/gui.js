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
    
    
    
    /* CODE FOR ALL THE TOOLTIPS */
    function emptyTips () {
        $('#display-tips').empty();
    }
    function displayTips (str) {
        $('#display-tips').html(str);
    }
    
    // Add new node area
    $(document).on('mouseover', '#add-node table', function () {
        displayTips('Type a node name (for example: dev2@127.0.0.1) and hit "Add Node" to add it to this cluster.  The node will then take ownership of partitions in the ring to help ensure balanced data across the cluster.');
    }).on('mouseout', '#add-node table', emptyTips);
    
    // Name of a node
    $(document).on('mouseover', '.node .name', function () {
        var name = $(this).text();
        displayTips('This node is named ' + name + '.  It is a member of this cluster.');
    }).on('mouseout', '.node .name', emptyTips);
    
    // Leave cluster sliders
    $(document).on('mouseover', '.leave-box .gui-slider', function () {
        displayTips('This will cause the node to begin relinquishing ownership of its data to other nodes in the cluster.  You will not be able to interact with the node via Riak Control during this process.  Once completed, Riak will shutdown on this node and it will leave the cluster.');
    }).on('mouseout', '.leave-box .gui-slider', emptyTips);
    
    // The 'Hosting Riak Control' message
    $(document).on('mouseover', '.current-host', function () {
        displayTips('This node cannot be shutdown or removed via Riak Control because it is currently hosting the app.');
    }).on('mouseout', '.current-host', emptyTips);
    
    // Markdown button
    $(document).on('mouseover', '.markdown-button', function () {
        displayTips('This node is currently unreachable.  While in this state, it may hinder cluster membership changes of other nodes.  Marking this node as "down" will allow other nodes to engage in membership changes unimpeded.');
    }).on('mouseout', '.markdown-button', emptyTips);
    
    // Node status
    $(document).on('mouseover', '.status-box', function () {
        var mytext = $(this).find('.status').text();
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
    
    
        
    
});