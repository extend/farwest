$(document).ready(function() {
	$('#fw_userfile_form').submit(function(){
		var url = window.location.pathname + $(this).attr('action');
		var self = this;
		$(this).ajaxSubmit({
			url: url,
			error: function(xhr){
				var errors = $.parseJSON(xhr.responseText);
				for (name in errors){
					$('<p class="alert alert-error">' + errors[name] + '</p>')
						.insertBefore('[name=' + name + ']');
				}
			},
			success: function(){
				$('.alert', self).remove();
				$(self).prepend('<p class="alert alert-success">Success!</p>');
			}
		});

		return false;
	});
});
