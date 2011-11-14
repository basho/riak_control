// Framework for the cool UI tricks

$(function() {
    
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
    
    
    
    /*
    MAKE SLIDING SWITCHES WORK
    To set up:  requires jQuery and jQuery UI.  HTML should contain at least one div
    with the class 'gui-slider'.  This div may optionally contain a div classed
    'gui-slider-activate' if you want to activate the slider by clicking the right edge.
    It should also contain a div classed 'gui-slider-groove' which may optionally
    contain two divs classed 'gui-slider-msg'.  If so, each should respectively also
    be given the class 'isLeft' or 'isRight'.
    */
    // Define a function to execute when the slider gets moved all the way over
    function leaveEvent (track, handle) {
        console.log('all the way right');
    }
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
    $('.gui-slider-groove').slider({
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
            if (handlePos === '100%') {
                leaveEvent(me, myHandle);
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
	                    showMsg($(this).parent()); 
                        leaveEvent(me, myHandle);
                    }
                });
            }
        }
    });
    /*
    Enable this section if your slider message container does not extend to the edge
    of the slider.
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
                    showMsg($(this).parent()); 
                    leaveEvent(me, myHandle);
                }
            });
        }
    });
    */
    // END CODE FOR SLIDING SWITCHES    
    
});