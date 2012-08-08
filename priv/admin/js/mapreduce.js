
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
                //var node = $(this).closest('tr').find('.name').text();
                //var siblingRowName = $(this).closest('tr').attr('name') + '-more-actions';
                // Re-allow pings when we let go of the slider handle
                // And tell it to ping since the ping loop will have died
                pingAllowed = true;
                //get_cluster_status();
                if (handlePos === '100%') {
                    //leave_cluster(node);
                    //open_sibling_row(siblingRowName, node);
                } else if (parseInt(handlePos) < (me.width() * .66)) {
                    myHandle.animate({left:'0px'},{
                        queue:false,
                        duration:200,
                        complete:function() {
                            showMsg($(this).parent());
                            //close_sibling_row(siblingRowName);
                        }
                    });
                } else {
                    myHandle.animate({left:'100%'},{
                        queue:false,
                        duration:200,
                        complete:function () {
                            //var node = $(this).closest('tr').find('.name').text();
                            //var siblingRowName = $(this).closest('tr').attr('name') + '-more-actions';
                            showMsg($(this).parent());
                            //open_sibling_row(siblingRowName, node);
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

    // Code for opening rows and whatnot
    $(document).on('click', '.edit-phase-button', function () {
        var parentRow = $(this).parent().parent(),
            siblingRow = parentRow.next('tr'),
            editButton = parentRow.find('.edit-phase-button');
        parentRow.addClass('highlight');
        siblingRow.show();
        editButton.fadeOut();
        $('.query-content', siblingRow).slideDown();
    });
    $(document).on('click', '.update-query-content', function () {
        var contentBox = $(this).closest('.query-content'),
            parentRow = contentBox.closest('.query-specifics'),
            siblingRow = parentRow.prev('tr'),
            editButton = siblingRow.find('.edit-phase-button');
            editButton.fadeIn();
        contentBox.slideUp('fast', function () {
            parentRow.hide();
            siblingRow.removeClass('highlight');
        });
    });
    // End code for opening rows and whatnot


    function prepPage() {
        $('.gui-slider-groove').trigger('initSlider');
        $('.gui-dropdown').trigger('change');
    }

    $.riakControl.sub('templateSwitch', function (templateName) {
        if (templateName === 'mapreduce') {
            prepPage();
        }
    });

    prepPage();

});