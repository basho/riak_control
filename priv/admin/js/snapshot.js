$(document).ready(function () {

	var pingAllowed = true;

	var previousProblems = {
		"down" : {},
		"unreachable" : {},
		"low_mem" : {}
	};

	function byId(str) {
		return document.getElementById(str);
	}

	function unhealthy_cluster(obj) {
		var i, l, newProblems = {
			"down" : {},
			"unreachable" : {},
			"low_mem" : {}
		};
		
		if ($('#healthy-cluster').css('display') === 'block') {
			$('#healthy-cluster').fadeOut();
		}
		if ($('#unhealthy-cluster').css('display') === 'none') {
			$('#unhealthy-cluster').css({
				"position" : "absolute",
				"top" : 0
			}).fadeIn(function () {
				$(this).css({
					"position" : "",
					"top" : ""
				})
			});
		}

		function cycle_problems(category) {
			var cat = category + '_nodes', len = obj[cat].length, i;
			if (len) {
				$('#' + category + '-nodes-title, #' + category + '-nodes-list').show();
				for (i = 0; i < len; i += 1) {
					if (!byId(category + '_' + obj[cat][i])) {
						$('#' + category + '-nodes-list').append('<li id="' + category + '_' + obj[cat][i] + '"><a class="go-to-cluster">' + obj[cat][i] + '</a></li>');
					}
					newProblems[category][obj[cat][i]] = true;
				}
				for (i in previousProblems[category]) {
					if (Object.prototype.hasOwnProperty.call(previousProblems[category], i)) {
						if (!newProblems[category][i]) {
							byId(category + '_' + i).parentNode.removeChild(byId(category + '_' + i));
						}
					}
				}
			} else {
				$('#' + category + '-nodes-list').empty();
				$('#' + category + '-nodes-title, #' + category + '-nodes-list').hide();
			}
		}

		cycle_problems('down');
		cycle_problems('unreachable');
		cycle_problems('low_mem');

		previousProblems = newProblems;

		ping_overview_data();
	}

	function healthy_cluster(obj) {
		if ($('#unhealthy-cluster').css('display') === 'block') {
			$('#unhealthy-cluster').fadeOut();
		}
		if ($('#healthy-cluster').css('display') === 'none') {
			$('#healthy-cluster').css({
				"position" : "absolute",
				"top" : 0
			}).fadeIn(function () {
				$(this).css({
					"position" : "",
					"top" : ""
				})
			});
		}
		ping_overview_data()
	}

	function ping_overview_data() {
		setTimeout(function () {
			if ($('#snapshot-headline').length && pingAllowed === true) {
				get_overview_data();
			} else {
				pingAllowed = false;
			}
		}, 5000);
	}

	function update_overview_data(d) {
		var i;
		for (i in d) {
			if (Object.prototype.hasOwnProperty.call(d, i)) {
				if (d[i].length) {
					return unhealthy_cluster(d);
				}
			}
		}
		return healthy_cluster(d);
	}

	function get_overview_data() {
		$.ajax({
            method:'GET',
            url:'/admin/overview',
            dataType:'json',
            failure:ping_overview_data,
            success: function (d) {
                update_overview_data(d);
            }
        });
	}

	function initialize() {
		get_overview_data();
	}

	initialize();

	// Subscribe to the 'templateSwitch' event.
	// This function will run when a template is switched.
	$.riakControl.sub('templateSwitch', function (templateName) {
		if (templateName === 'snapshot') {
			pingAllowed = true;
			initialize();
		}
	});

});