$(document).ready(function () {

	function unhealthy_cluster(obj) {
		var i, l;
		
		$('#unhealthy-cluster').show();

		console.log(obj);

		if (obj.down_nodes.length) {
			$('#down-nodes-title, #down-nodes-list').show();
			l = obj.down_nodes.length;
			for (i = 0; i < l; i += 1) {
				$('#down-nodes-list').append('<li name="' + obj.down_nodes[i] + '"><a class="go-to-cluster">' + obj.down_nodes[i] + '</a></li>');
			}
		}

		if (obj.unreachable_nodes.length) {
			$('#unreachable-nodes-title, #unreachable-nodes-list').show();
			l = obj.unreachable_nodes.length;
			for (i = 0; i < l; i += 1) {
				$('#unreachable-nodes-list').append('<li name=' + obj.unreachable_nodes[i] + '><a class="go-to-cluster">' + obj.unreachable_nodes[i] + '</a></li>');
			}
		}

		if (obj.low_mem_nodes.length) {
			$('#low-mem-nodes-title, #low-mem-nodes-list').show();
			l = obj.low_mem_nodes.length;
			for (i = 0; i < l; i += 1) {
				$('#low-mem-nodes-list').append('<li name=' + obj.low_mem_nodes[i] + '><a class="go-to-cluster">' + obj.low_mem_nodes[i] + '</a></li>');
			}
		}
	}

	function healthy_cluster(obj) {
		$('#healthy-cluster').show();
	}

	function ping_overview_data() {
		setTimeout(function () {
			get_overview_data();
		}, 1000);
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
			initialize();
		}
	});

});