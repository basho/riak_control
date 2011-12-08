// This file handles all of the page transitions over pjax

$(function () {

    var mostRecentUrl = '';
    var appendedScripts = {};

    // I have a feeling that having some super-simple, app-wide, event handling could be useful soon...
    $.riakControl = $.riakControl || {};
    if (!$.riakControl.events) {
        $.riakControl.events = {};
    }

    // General publish function.
    // Invokes all functions subscribed to an event name.
    // Takes an event name, an array of arguments to pass to subscribed functions, and a context within
    // which to invoke them.
    $.riakControl.pub = $.riakControl.pub || function (eventName, argArray, context) {
        var i, l = ($.riakControl.events[eventName]) ? $.riakControl.events[eventName].length : 0, counter = 0;
        for (i = 0; i < l; i += 1) {
            $.riakControl.events[eventName][i].apply(context, argArray);
            counter += 1;
        }
        return (counter > 0);
    };

    // General subscribe function.
    // Allows you to define a function that will be invoked when an event is published by the
    // publish function.
    // Takes an event name and the function subscribing to the event.
    $.riakControl.sub = $.riakControl.sub || function (eventName, func) {
        if (!$.riakControl.events[eventName]) {
            $.riakControl.events[eventName] = [func];
        } else {
            $.riakControl.events[eventName].push(func);
        }
        return true;
    };


    // Define a reusable function to make pjax calls
    function grabPjax(url, successFunc) {
        if (mostRecentUrl !== url) {
            mostRecentUrl = url;
            return $.pjax({
                url: url,
                container: '#content-well',
                push: false,
                replace: false,
                success: function (x, y, z) {
                    successFunc && successFunc(x, y, z);
                }
            });
        }
    }
    
    // Define a reusable function for appending scripts to pages
    function appendScript(scriptID, scriptSRC) {
        var newScript;
        if (!appendedScripts[scriptID]) {
            newScript = document.createElement('script');
            newScript.setAttribute('id', scriptID);
            newScript.setAttribute('src', scriptSRC);
            document.body.appendChild(newScript);
            appendedScripts[scriptID] = newScript;
        }
    }
    
    // Call the snapshot page by default on docready
    grabPjax('/admin/ui/templates/snapshot.pjax', function () {
        appendScript('#snapshot-script', '/admin/ui/js/snapshot.js');
        $.riakControl.pub('templateSwitch', ['snapshot']);
    });

    // Calling the snapshot page on nav click...
    $('#nav-snapshot').on('click', function () {
        return grabPjax('/admin/ui/templates/snapshot.pjax', function () {
            appendScript('#snapshot-script', '/admin/ui/js/snapshot.js');
            $.riakControl.pub('templateSwitch', ['snapshot']);
        });
    });

    // Calling the cluster page on nav click...
    $('#nav-cluster').on('click', function () {
        return grabPjax('/admin/ui/templates/cluster.pjax', function () {
            appendScript('#cluster-script', '/admin/ui/js/cluster.js');
            $.riakControl.pub('templateSwitch', ['cluster']);
        });
    });

    // Calling the ring page on nav click...
    $('#nav-ring').on('click', function () {
        return grabPjax('/admin/ui/templates/ring.pjax', function () {
            appendScript('#ring-script', '/admin/ui/js/ring.js');
            $.riakControl.pub('templateSwitch', ['ring']);
        });
    });

    // Getting places when you're not clicking on the left hand nav
    $(document).on('click', '.go-to-cluster', function () {
        $('#nav-cluster').trigger('click');
    });

});