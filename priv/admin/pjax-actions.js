// This file handles all of the page transitions over pjax

$(function () {

    var mostRecentUrl = '';

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
        if (!$(scriptID).length) {
            $('body').append('<script id="' + scriptID + '" src="' + scriptSRC + '"></script>');
        }
    }
    
    // Call the cluster page by default on docready
    grabPjax('cluster.pjax', function () {
        appendScript('#cluster-script', '/admin/ui/cluster.js');
    });

    // Calling the cluster page on nav click...
    $('#nav-cluster').on('click', function () {
        return grabPjax('cluster.pjax', function () {
            appendScript('#cluster-script', '/admin/ui/cluster.js');
        });
    });

    // Calling the ring page on nav click...
    $('#nav-ring').on('click', function () {
        return grabPjax('ring.pjax', function () {
            appendScript('#ring-script', '/admin/ui/ring.js');
        });
    });

});