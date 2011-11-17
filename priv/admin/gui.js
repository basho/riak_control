// Framework for the cool UI tricks

$(function() {

    /* ALLOWS YOU TO HIT ENTER IN THE ADD-NODE FIELD WITHOUT MAKING IT A FORM */
    $('#node-to-add').keyup(function(event){
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
    splitBar.click(function () {
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
    $('.nav-li').click(function () {
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
    $('.gui-switch input').live('change', function(e) {
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
    
    
    /*
    MAKE TEXT FIELDS WORK
    To set up: wrap an input[type=text] classed 'gui-field-input' in a div with
    the class 'gui-field'.
    */
    // Auto-surround the input field with background div and graphic caps.
    $('.gui-field').append('<div class="gui-field-cap-left"></div><div class="gui-field-cap-right"></div>');
    $('.gui-field-input').wrap('<div class="gui-field-bg">');
    
    
    /* CODE FOR ALL THE TOOLTIPS */
    function emptyTips () {
        $('#display-tips').empty();
    }
    function displayTips (str) {
        $('#display-tips').html(str);
    }
    $('#add-node table').hover(function () {
        displayTips('Type a node name (for example: dev2@127.0.0.1) and hit "Add Node" to add it to the cluster.');
    }, emptyTips);
    $('.leave-box .gui-slider').live('mouseover', function () {
        displayTips('This will cause the node to begin the process of leaving the cluster. It will handoff its partition data to other partitions in the ring and then take itself offline.');
    }).live('mouseout', emptyTips);
    
        
    
});