// Framework for the cool UI tricks
//
// TODO: These should be moved to didInsertElement hooks on the
// Ember Views once they are no longer needed by the pages scheduled
// for deprecation.
//
// Set up a safe, global namespace in the jQuery object.
$(document).ready(function() {
  $.riakControl = $.riakControl || {};

  /*
  // ALLOWS YOU TO HIT ENTER IN THE ADD-NODE FIELD WITHOUT MAKING IT A FORM
  $(document).on('keyup', '#node-to-add', function (event) {
    if(event.keyCode === 13){
      $('#add-node-button').trigger('click');
    }
  });
  */

  // MARK THE ACTIVE NAV ICON WITH THE PROPER CLASS
  $.riakControl.markNavActive = $.riakControl.markNavActive || function markNavActive(id) {
    Ember.run.next(function() {
      var lis = $('nav li'), activeli = $("#" + id);
      lis.each(function (index, item) {
        if (item !== activeli[0]) {
          $(item).removeClass('active');
        } else {
          $(item).addClass('active');
        }
      });
    });
  };

  
  // UPDATE DROPDOWN MENUS
  $.riakControl.updateDropdown = $.riakControl.updateDropdown || function updateDropdown (me, val) {
    Ember.run.next(function() {
      var textSpot = me.prev().prev(); textSpot.text(val);
    });
  };

  /*
  // MAKE HIDE/SHOW SWITCHES WORK
  $(document).on('click', '.gui-switch', function (e) {
    var that = $(this),
        parent = that.parent().parent(),
        corresponder = that.closest('tr').next(),
        box = corresponder.find('.actions-box');
    if (that.hasClass('off')) {
      corresponder.show();
      box.slideDown('fast');
    } else {
      box.slideUp('fast', function () {
        corresponder.hide();
      });
    }
    parent.toggleClass('on off');
    that.toggleClass('on off');
  });
  */

  // MAKE CHECKBOXES WORK WHEN YOU CLICK THEM
  $(document).on('change', '.gui-checkbox', function(e) {
      var me      = $(this),
          parent  = me.parent(),
          checked = me.attr('checked');

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
          checked = me.attr('checked'),
          group   = $('input[type="radio"][name="' + me.attr('name') + '"]');
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
        if (me.attr('value') === 'replace') {
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

  /*
  // CODE FOR ALL THE TOOLTIPS
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
          displayTips('Because this node is currently unreachable or incompatible with Riak Control, Riak Control is not able to assess its memory usage.');
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
      } else if (mytext === 'incompatible') {
          displayTips('This node is incompatible with Riak Control.  While in this state, Riak Control will be unable to determine status of this node.');
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

*/
});
