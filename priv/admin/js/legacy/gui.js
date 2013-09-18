// Framework for the cool UI tricks
//
// TODO: These should be moved to didInsertElement hooks on the
// Ember Views once they are no longer needed by the pages scheduled
// for deprecation.
//
// Set up a safe, global namespace in the jQuery object.
$(document).ready(function() {
  $.riakControl = $.riakControl || {};

  // MARK THE ACTIVE NAV ICON WITH THE PROPER CLASS
  $.riakControl.markNavActive = $.riakControl.markNavActive || function markNavActive(id) {
    Ember.run.next(function() {
      var listItems = $('nav li'), activeItem = $("#" + id);
      listItems.each(function (index, each) {
        if (each !== activeItem[0]) {
          $(each).removeClass('active');
        } else {
          $(each).addClass('active');
        }
      });
    });
  };

  // MAKE CHECKBOXES WORK WHEN YOU CLICK THEM
  $(document).on('change', '.gui-checkbox', function(e) {
    var me      = $(this),
        parent  = me.parent(),
        checked = me.prop('checked');

    if (checked) {
      parent.css('background-position', 'left bottom');
    } else {
      parent.css('background-position', 'left top');
    }
  });

  // MAKE RADIO BUTTONS WORK WHEN YOU CLICK THEM
  $(document).on('change', '.gui-radio', function(e) {
    var me      = $(this),
        parent  = me.parent(),
        checked = me.prop('checked'),
        group   = $('input[type="radio"][name="' + me.prop('name') + '"]');

    /*
     * If the radio button is checked...
     */
    if (checked) {
      /*
       * Change the position of the background image sprite.
       */
      parent.css('background-position', 'left bottom');
      /*
       * Loop over all other radio buttons in the group and set their
       * background positions to reflect the unchecked state.
       */
      group.each(function (index, item) {
        var $item = $(item);
        if ($item[0] !== me[0]) {
          $item.parent().css('background-position', 'left top');
        }
      });
      /*
       * If the checked radio button is the 'replace' radio button...
       */
      if (me.prop('value') === 'replace') {
        /*
         * Enable the extra replacement actions.
         */
        parent.parent().find('.extra-actions').addClass('active').find('.disabler').hide();
      /*
       * Otherwise disable the replacement actions.
       */
      } else {
        parent.parent().find('.extra-actions').removeClass('active').find('.disabler').show();
      }
    }
  });


});
