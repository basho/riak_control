$(document).ready(function () {
	
	function initialize() {
		get_overview_data();
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

	function update_overview_data(d) {
		console.log(d);
		var i;
	}

	function ping_overview_data() {
		setTimeout(function () {
			get_overview_data();
		}, 1000);
	}

	initialize();

});