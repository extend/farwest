$(document).ready(function() {
	$('#fw_template_form').submit(function(){
		var url;
		if ($('[name=name]', this).val() == undefined) {
			url = window.location.pathname + $(this).attr('action');
		} else {
			url = window.location.pathname + '/' + $('[name=name]', this).val()
				+ $(this).attr('action')
		}
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

	$('a.delete').bind('click', function(event){
		event.preventDefault();
		$.ajax({
			type: 'DELETE',
			url: this.href
		});
	});
});
