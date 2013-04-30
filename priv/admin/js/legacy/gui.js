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
      displayTips('The cluster view is where you can plan and commit changes to your cluster such as adding, removing, or replacing nodes.');
  }).on('mouseout', '#nav-cluster', emptyTips);

  // Nodes link in navigation
  $(document).on('mouseover', '#nav-nodes', function () {
      displayTips('The nodes view contains actions that are not part of the plan and commit process. From here you can stop nodes or mark them as down.');
  }).on('mouseout', '#nav-cluster', emptyTips);

  // Ring link in navigation
  $(document).on('mouseover', '#nav-ring', function () {
      displayTips('The ring view shows a list of your partitions indicating which node owns each partition.  You have the ability to apply filters to your view of the partitions and see indicators showing the status of various node workers, whether they are in active, fallback, or handoff states.');
  }).on('mouseout', '#nav-ring', emptyTips);

  // Snapshot link in navigation
  $(document).on('mouseover', '#nav-snapshot', function () {
      displayTips('Selecting the snapshot view commands Riak Control to gather some general information related to the current health of your cluster.  It is useful in quickly determining whether or not there are any issues to be concerned about.');
  }).on('mouseout', '#nav-snapshot', emptyTips);

  // Cluster/Nodes Views

  // Add new node area
  $(document).on('mouseover', '#add-node table', function () {
      displayTips('Type a node name (for example: dev2@127.0.0.1) into the text field and hit "Join Node" to add it to this cluster.  The node will then take ownership of partitions in the ring to help ensure balanced data across the cluster.');
  }).on('mouseout', '#add-node table', emptyTips);

  // Clear down/stop radios button
  $(document).on('mouseover', '.buttons .gui-rect-button', function () {
      displayTips('Click this button to clear marks from all of the radio buttons on this page.');
  }).on('mouseout', '.buttons .gui-rect-button', emptyTips);

  // Apply down/stop radios button
  $(document).on('mouseover', '.buttons .gui-point-button-right', function () {
      displayTips('Click this button to initialize all stops and downs on currently marked nodes.');
  }).on('mouseout', '.buttons .gui-point-button-right', emptyTips);

  // Commit cluster plan button
  $(document).on('mouseover', '#commit-button', function () {
      displayTips('Click this button to commit your cluster plan.  This will initialize all joins, leaves, and replacements as you have indicated.');
  }).on('mouseout', '#commit-button', emptyTips).on('click', '#commit-button', emptyTips);

  // Commit cluster plan button
  $(document).on('mouseover', '.clear-plan-box .gui-rect-button', function () {
      displayTips('Click this button to un-stage all changes made to the cluster plan.');
  }).on('mouseout', '.clear-plan-box .gui-rect-button', emptyTips).on('click', '.clear-plan-box .gui-rect-button', emptyTips);

  // Name of a node
  $(document).on('mouseover', '.node .name', function () {
      var name = $(this).text();
      displayTips('This node is named ' + name + '.  It is a member of this cluster.');
  }).on('mouseout', '.node .name', emptyTips);

  // Ring ownership percent
  $(document).on('mouseover', '.pct-box, .pct-arrows', function () {
      displayTips('The portion of the ring owned by this node.  When an arrow LED is lit, it is either in the process of receiving a partition from another node (up) or sending a partition to another node (down).');
  }).on('mouseout', '.pct-box, .pct-arrows', emptyTips);

  // Memory usage
  $(document).on('mouseover', '.membar-bg, .free-memory', function () {
      var parent = $(this).parent(),
          erlang_mem, non_erlang_mem,
          free_mem = parent.find('.unknown-mem').attr('name');
      if (!free_mem || free_mem.charAt(0) === '?') {
          displayTips('Because this node is currently unreachable or incompatible with Riak Control, Riak Control is not able to assess its memory usage.');
      } else {
          free_mem = parseInt(free_mem, 10);
          erlang_mem = parseInt(parent.find('.erlang-mem').attr('name'), 10);
          non_erlang_mem = parseInt(parent.find('.non-erlang-mem').attr('name'), 10);
          displayTips('The machine running this node currently has ' + free_mem + '% free memory.  Of the ' + (erlang_mem + non_erlang_mem) + '% currently in use, ' + erlang_mem + '% is being used by Riak and ' + non_erlang_mem + '% is being used by other processes.');
      }
  }).on('mouseout', '.membar-bg, .free-memory', emptyTips);

  // Node status
  $(document).on('mouseover', '.status-light', function () {
      var classes = this.className;
      if (/\bgreen\b/.test(classes)) {
          displayTips('This node is currently online and working.');
      } else if (/\bred\b/.test(classes)) {
          displayTips('This node is unreachable.  Riak may need to be restarted or there may be other connectivity issues.  Cluster membership changes like "join" and "leave" cannot complete until this node is reachable or marked as "down".');
      } else if (/\bblue\b/.test(classes)) {
          displayTips('This node has been marked as "down". While in this state it can not be interacted with but it will not impede cluster membership changes of other nodes.  To return to a "valid" state, simply restart Riak on this node.');
      } else if (/\borange\b/.test(classes)) {
          displayTips('This node is in process of leaving the cluster.  When it has finished relinquishing ownership and transferring data to other nodes, Riak will stop on this node and it will cease to be a member of the cluster.  You can not interact with this node during this process.');
      }
  }).on('mouseout', '.status-light', emptyTips);

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
