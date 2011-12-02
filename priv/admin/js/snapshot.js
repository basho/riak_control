$(document).ready(function () {

	function unhealthy_cluster(obj) {
		var i;
		for (i in obj) {
			if (Object.prototype.hasOwnProperty.call(obj, i)) {
				
			}
		}

		$('#unhealthy-cluster').show();
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

});