$(document).ready(function() {
	$('#fw_routes_form').submit(function(){
		var url = window.location.pathname;

		var self = this;
		$(this).ajaxSubmit({
			url: url,
			error: function(xhr){
				$('p.alert').remove();
				var errors = $.parseJSON(xhr.responseText);
				for (name in errors){
					$('<p class="alert alert-error">' + errors[name] + '</p>')
						.insertBefore('[name=' + name + ']');
				}
			},
			success: function(){
				$('p.alert').remove();
				$('h3').before('<p class="alert alert-success">Success! Feel free to continue editing.</p>');
				$("p.alert").delay(2000).fadeOut("slow", function(){$(this).remove();});
			}
		});

		return false;
	});
});
